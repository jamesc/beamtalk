// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2849: a selector-kind x `InferredType`-shape coverage matrix for
//! argument-type checking.
//!
//! [BT-2843], [BT-2845], [BT-2846], [BT-2847], [BT-2848], and [BT-2871] were
//! six independent instances of the same bug shape: a specific validation
//! path (binary sends, cascades, FFI calls, nested generic args, class-body
//! defaults) assumed a "broader"/"more specific" check elsewhere covered
//! `Union`/`Dynamic` argument shapes, when in fact that other check only
//! handled `InferredType::Known`. Each fix landed with its own narrow
//! regression test (see `bt2843_binary_arg_union_check.rs`,
//! `bt2871_cascade_binary_arg_check.rs`, `bt2847_nested_union_type_args.rs`,
//! the BT-2845 tests in `arg_return_checking.rs`, and the BT-2846 tests in
//! `ffi.rs`), but nothing forced the *same* coverage matrix onto every
//! message-send shape uniformly — which is exactly why these gaps went
//! unnoticed for as long as they did.
//!
//! This module is that matrix. It parameterizes over:
//!
//! - **Send shape**: `Unary`, `Binary`, `Keyword`, `Cascade-first`,
//!   `Cascade-continuation` (both keyword and binary selectors — BT-2871
//!   showed these are genuinely different code paths), and `FFI call`.
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
//! Known scope limits (tracked, not silent gaps): the cascade-continuation
//! rows only exercise the instance-receiver dispatch branch of the
//! continuation loop (`c takeStr: ...; second: ...`), not the class-ref or
//! self-in-class-method branches — see [BT-2877]. Verified with an empirical
//! revert-and-check: temporarily undoing BT-2871's cascade-continuation-binary
//! fix in `inference.rs` made exactly the two cells that claim to cover it
//! fail, confirming this matrix has teeth rather than just re-asserting the
//! status quo.
//!
//! [BT-2843]: https://linear.app/beamtalk/issue/BT-2843
//! [BT-2845]: https://linear.app/beamtalk/issue/BT-2845
//! [BT-2846]: https://linear.app/beamtalk/issue/BT-2846
//! [BT-2847]: https://linear.app/beamtalk/issue/BT-2847
//! [BT-2848]: https://linear.app/beamtalk/issue/BT-2848
//! [BT-2871]: https://linear.app/beamtalk/issue/BT-2871
//! [BT-2877]: https://linear.app/beamtalk/issue/BT-2877

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
fn fixture(extra_class_method: &str) -> String {
    format!(
        r"typed Value subclass: Thing
  ++ other :: String -> String => other
  + other :: Object -> Object => other
  takeStr: aString :: String -> String => aString
  takeObj: anObject :: Object -> Object => anObject
  second: aString :: String -> String => aString
  secondObj: anObject :: Object -> Object => anObject

{extra_class_method}
"
    )
}

fn run(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
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
    ClassHierarchy::build(&module).0.unwrap()
}

fn assert_unary_cell(cell: &str, arg_ty: InferredType) {
    let hierarchy = unary_hierarchy();
    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &eco_string("Thing"),
        "probe",
        &[arg_ty],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "[{cell}] a 0-param (unary) method must never produce an argument-type \
         diagnostic, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn matrix_unary_known_compatible() {
    assert_unary_cell("unary/known-compatible", InferredType::known("String"));
}

#[test]
fn matrix_unary_known_incompatible() {
    assert_unary_cell("unary/known-incompatible", InferredType::known("Integer"));
}

#[test]
fn matrix_unary_union_all_compatible() {
    assert_unary_cell(
        "unary/union-all-compatible",
        InferredType::simple_union(&["String", "Symbol"]),
    );
}

#[test]
fn matrix_unary_union_incompatible_member() {
    assert_unary_cell(
        "unary/union-incompatible-member",
        InferredType::simple_union(&["String", "Nil"]),
    );
}

#[test]
fn matrix_unary_dynamic() {
    assert_unary_cell(
        "unary/dynamic",
        InferredType::Dynamic(DynamicReason::Unknown),
    );
}

// ---------------------------------------------------------------------------
// Binary — `c ++ <arg>` / `c + <arg>` (non-cascade). BT-2843's fix.
// ---------------------------------------------------------------------------

#[test]
fn matrix_binary_known_compatible() {
    let src = fixture("  class run -> Nil =>\n    c := self new\n    c ++ \"ok\"\n    nil");
    assert_compatible(&run(&src), "binary/known-compatible");
}

#[test]
fn matrix_binary_known_incompatible() {
    let src = fixture("  class run -> Nil =>\n    c := self new\n    c ++ 42\n    nil");
    assert_incompatible(&run(&src), "binary/known-incompatible");
}

#[test]
fn matrix_binary_union_all_compatible() {
    let src = fixture(
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c + y\n    nil",
    );
    assert_compatible(&run(&src), "binary/union-all-compatible");
}

#[test]
fn matrix_binary_union_incompatible_member() {
    let src =
        fixture("  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c ++ y\n    nil");
    assert_incompatible(&run(&src), "binary/union-incompatible-member");
}

#[test]
fn matrix_binary_dynamic() {
    let src = fixture("  class run -> Nil =>\n    c := self new\n    c ++ undeclaredVar\n    nil");
    assert_compatible(&run(&src), "binary/dynamic");
}

/// Supplementary to the five `matrix_binary_*` cells above: those route
/// through a custom `Thing`-defined `++`/`+`, so `check_binary_operand_types`
/// never has bespoke logic for them (`binary_operand_check_ran` stays
/// `false`) and every cell exercises only the generic `check_argument_types`
/// fallback. BT-2843's actual crux was the *interaction* between the two
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
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert_incompatible(
        checker.diagnostics(),
        "binary/builtin-receiver-known-incompatible-no-duplicate",
    );
}

// ---------------------------------------------------------------------------
// Keyword — `c takeStr: <arg>` / `c takeObj: <arg>` (non-cascade). BT-1832's
// original coverage (the baseline every other shape is measured against).
// ---------------------------------------------------------------------------

#[test]
fn matrix_keyword_known_compatible() {
    let src = fixture("  class run -> Nil =>\n    c := self new\n    c takeStr: \"ok\"\n    nil");
    assert_compatible(&run(&src), "keyword/known-compatible");
}

#[test]
fn matrix_keyword_known_incompatible() {
    let src = fixture("  class run -> Nil =>\n    c := self new\n    c takeStr: 42\n    nil");
    assert_incompatible(&run(&src), "keyword/known-incompatible");
}

#[test]
fn matrix_keyword_union_all_compatible() {
    let src = fixture(
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c takeObj: y\n    nil",
    );
    assert_compatible(&run(&src), "keyword/union-all-compatible");
}

#[test]
fn matrix_keyword_union_incompatible_member() {
    let src = fixture(
        "  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c takeStr: y\n    nil",
    );
    assert_incompatible(&run(&src), "keyword/union-incompatible-member");
}

#[test]
fn matrix_keyword_dynamic() {
    let src =
        fixture("  class run -> Nil =>\n    c := self new\n    c takeStr: undeclaredVar\n    nil");
    assert_compatible(&run(&src), "keyword/dynamic");
}

// ---------------------------------------------------------------------------
// Cascade-first — the first message of a cascade goes through the exact same
// `infer_message_send_with_receiver_ty` path as a non-cascade send, just
// reached via cascade syntax. A harmless `second: "y"` continuation keeps the
// cascade well-formed without affecting the cell under test.
// ---------------------------------------------------------------------------

#[test]
fn matrix_cascade_first_known_compatible() {
    let src = fixture(
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"ok\"; second: \"y\"\n    nil",
    );
    assert_compatible(&run(&src), "cascade-first/known-compatible");
}

#[test]
fn matrix_cascade_first_known_incompatible() {
    let src = fixture(
        "  class run -> Nil =>\n    c := self new\n    c takeStr: 42; second: \"y\"\n    nil",
    );
    assert_incompatible(&run(&src), "cascade-first/known-incompatible");
}

#[test]
fn matrix_cascade_first_union_all_compatible() {
    let src = fixture(
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c takeObj: y; secondObj: \"y\"\n    nil",
    );
    assert_compatible(&run(&src), "cascade-first/union-all-compatible");
}

#[test]
fn matrix_cascade_first_union_incompatible_member() {
    let src = fixture(
        "  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c takeStr: y; second: \"y\"\n    nil",
    );
    assert_incompatible(&run(&src), "cascade-first/union-incompatible-member");
}

#[test]
fn matrix_cascade_first_dynamic() {
    let src = fixture(
        "  class run -> Nil =>\n    c := self new\n    c takeStr: undeclaredVar; second: \"y\"\n    nil",
    );
    assert_compatible(&run(&src), "cascade-first/dynamic");
}

// ---------------------------------------------------------------------------
// Cascade-continuation (keyword) — a well-typed first message (`takeStr:
// "seed"`) followed by the `second:` continuation under test. BT-2845's fix
// (the cascade loop's `for msg in messages` never ran `check_argument_types`
// for continuation messages at all).
// ---------------------------------------------------------------------------

#[test]
fn matrix_cascade_continuation_keyword_known_compatible() {
    let src = fixture(
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; second: \"ok\"\n    nil",
    );
    assert_compatible(&run(&src), "cascade-continuation-keyword/known-compatible");
}

#[test]
fn matrix_cascade_continuation_keyword_known_incompatible() {
    let src = fixture(
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; second: 42\n    nil",
    );
    assert_incompatible(
        &run(&src),
        "cascade-continuation-keyword/known-incompatible",
    );
}

#[test]
fn matrix_cascade_continuation_keyword_union_all_compatible() {
    let src = fixture(
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c takeObj: \"seed\"; secondObj: y\n    nil",
    );
    assert_compatible(
        &run(&src),
        "cascade-continuation-keyword/union-all-compatible",
    );
}

#[test]
fn matrix_cascade_continuation_keyword_union_incompatible_member() {
    let src = fixture(
        "  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; second: y\n    nil",
    );
    assert_incompatible(
        &run(&src),
        "cascade-continuation-keyword/union-incompatible-member",
    );
}

#[test]
fn matrix_cascade_continuation_keyword_dynamic() {
    let src = fixture(
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; second: undeclaredVar\n    nil",
    );
    assert_compatible(&run(&src), "cascade-continuation-keyword/dynamic");
}

// ---------------------------------------------------------------------------
// Cascade-continuation (binary) — same shape as above, but the continuation
// message is a *binary* selector (`++` / `+`). BT-2871's fix specifically:
// `check_binary_operand_types` is only ever invoked from
// `infer_message_send_with_receiver_ty` (the first-message path), never from
// the cascade continuation loop, so a binary continuation's argument had
// *zero* fallback coverage even after BT-2845 fixed keyword continuations.
// ---------------------------------------------------------------------------

#[test]
fn matrix_cascade_continuation_binary_known_compatible() {
    let src = fixture(
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; ++ \"ok\"\n    nil",
    );
    assert_compatible(&run(&src), "cascade-continuation-binary/known-compatible");
}

#[test]
fn matrix_cascade_continuation_binary_known_incompatible() {
    let src = fixture(
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; ++ 42\n    nil",
    );
    assert_incompatible(&run(&src), "cascade-continuation-binary/known-incompatible");
}

#[test]
fn matrix_cascade_continuation_binary_union_all_compatible() {
    let src = fixture(
        "  class run: y :: String | Symbol -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; + y\n    nil",
    );
    assert_compatible(
        &run(&src),
        "cascade-continuation-binary/union-all-compatible",
    );
}

#[test]
fn matrix_cascade_continuation_binary_union_incompatible_member() {
    let src = fixture(
        "  class run: y :: String | Nil -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; ++ y\n    nil",
    );
    assert_incompatible(
        &run(&src),
        "cascade-continuation-binary/union-incompatible-member",
    );
}

#[test]
fn matrix_cascade_continuation_binary_dynamic() {
    let src = fixture(
        "  class run -> Nil =>\n    c := self new\n    c takeStr: \"seed\"; ++ undeclaredVar\n    nil",
    );
    assert_compatible(&run(&src), "cascade-continuation-binary/dynamic");
}

// ---------------------------------------------------------------------------
// FFI call — `check_ffi_argument_types` directly, mirroring the BT-2846
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
// regresses BT-2846's FFI Union handling instead of vacuously passing.
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
    let mut checker = TypeChecker::new();
    checker.check_ffi_argument_types("mymod", "fun", &sig, &[arg_type], span(), &hierarchy);
    checker.diagnostics().to_vec()
}

#[test]
fn matrix_ffi_known_compatible() {
    let diags = run_ffi(InferredType::known("String"), InferredType::known("String"));
    assert_compatible(&diags, "ffi/known-compatible");
}

#[test]
fn matrix_ffi_known_incompatible() {
    let diags = run_ffi(
        InferredType::known("String"),
        InferredType::known("Integer"),
    );
    assert_incompatible(&diags, "ffi/known-incompatible");
}

#[test]
fn matrix_ffi_union_all_compatible() {
    let diags = run_ffi(
        InferredType::known("Number"),
        InferredType::simple_union(&["Integer", "Float"]),
    );
    assert_compatible(&diags, "ffi/union-all-compatible");
}

#[test]
fn matrix_ffi_union_incompatible_member() {
    let diags = run_ffi(
        InferredType::known("String"),
        InferredType::simple_union(&["String", "Integer"]),
    );
    assert_incompatible(&diags, "ffi/union-incompatible-member");
}

#[test]
fn matrix_ffi_dynamic() {
    let diags = run_ffi(
        InferredType::known("String"),
        InferredType::Dynamic(DynamicReason::Unknown),
    );
    assert_compatible(&diags, "ffi/dynamic");
}

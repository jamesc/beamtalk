// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4415: a guard-style `^` (one inside an inlined conditional at the
//! method's top level) compiles as a direct return — no non-local-return
//! frame (`make_ref`, `try`, `throw`) — for every method kind, while a `^`
//! inside a real closure keeps it. `stdlib/test/guard_return_test.bt`
//! checks the runtime behaviour, state threading included.

use super::*;

fn codegen(src: &str) -> String {
    let (module, diagnostics) = parse_normalised(src);
    assert!(
        diagnostics
            .iter()
            .all(|d| d.severity != beamtalk_core::source_analysis::Severity::Error),
        "fixture failed to parse: {diagnostics:?}"
    );
    let code = generate_module(&module, CodegenOptions::new("guards").with_source(src))
        .expect("codegen should succeed");
    assert_compiles_through_erlc("guards", &code);
    code
}

fn assert_no_nlr_frame(code: &str) {
    for marker in ["make_ref", "NlrToken"] {
        assert!(
            !code.contains(marker),
            "guard-style `^` must not set up an NLR frame (found {marker}):\n{code}"
        );
    }
}

#[test]
fn canonical_class_side_guard_has_no_try() {
    let code = codegen(
        "Object subclass: Canon\n\
         \x20 class sealed canon: bytes :: Binary -> Binary =>\n\
         \x20   (bytes class) name =:= #Binary ifTrue: [^bytes]\n\
         \x20   Binary fromIolist: bytes\n",
    );
    assert_no_nlr_frame(&code);
    assert!(
        !code.contains("beamtalk_guard_return"),
        "`=:=` is a Boolean by construction, so the condition needs no helper:\n{code}"
    );
}

#[test]
fn object_method_guard_has_no_try() {
    let code = codegen(
        "Object subclass: G\n\
         \x20 sign: x =>\n\
         \x20   x > 0 ifTrue: [^1]\n\
         \x20   x < 0 ifTrue: [^-1]\n\
         \x20   0\n",
    );
    assert_no_nlr_frame(&code);
    assert_eq!(
        code.matches("call 'beamtalk_guard_return':'if_true'")
            .count(),
        2,
        "`x > 0` may answer a non-Boolean, so each guard's condition goes \
         through the helper:\n{code}"
    );
}

#[test]
fn value_method_guard_has_no_try() {
    let code = codegen(
        "Value subclass: V\n\
         \x20 state: n :: Integer = 0\n\
         \x20 clamp: x :: Integer =>\n\
         \x20   x < 0 ifTrue: [^self]\n\
         \x20   x ifNil: [^self]\n\
         \x20   self withN: x\n",
    );
    assert_no_nlr_frame(&code);
}

#[test]
fn actor_method_guard_has_no_try() {
    let code = codegen(
        "Actor subclass: A\n\
         \x20 state: count :: Integer = 0\n\
         \x20 bump: x :: Integer =>\n\
         \x20   self.count := self.count + 1\n\
         \x20   x > 10 ifTrue: [^#big]\n\
         \x20   self.count := self.count + x\n\
         \x20   #small\n",
    );
    assert_no_nlr_frame(&code);
}

#[test]
fn expect_directive_in_absorbed_statements_compiles() {
    // The statements after a guard move into its falling branch, a
    // statement-level `@expect` directive with them.
    let code = codegen(
        "Actor subclass: E\n\
         \x20 state: value = 0\n\
         \x20 bump => self.value := self.value + 1\n\
         \x20 bumpIfPositive: n :: Integer =>\n\
         \x20   n <= 0 ifTrue: [^\"skipped\"]\n\
         \x20   @expect lint \"trailing ^ under test\"\n\
         \x20   ^self bump\n",
    );
    assert_no_nlr_frame(&code);
}

#[test]
fn typed_comparison_guard_needs_no_helper() {
    // BT-4452: `x :: Integer` lowers `x > 0` to a bare BIF, a Boolean by
    // construction, so the condition needs no helper.
    let code = codegen(
        "Object subclass: G\n\
         \x20 sign: x :: Integer =>\n\
         \x20   x > 0 ifTrue: [^1]\n\
         \x20   x < 0 ifTrue: [^-1]\n\
         \x20   0\n",
    );
    assert_no_nlr_frame(&code);
    assert!(!code.contains("beamtalk_guard_return"), "{code}");
}

#[test]
fn guard_with_effectful_return_on_non_boolean_receiver_keeps_its_frame() {
    // `self withN: 0` would run outside the receiver's own `ifTrue:` once
    // rewritten, so only a Boolean by construction may take this shape.
    let code = codegen(
        "Value subclass: W\n\
         \x20 field: n :: Integer = 0\n\
         \x20 clamp: x =>\n\
         \x20   x < 0 ifTrue: [^self withN: 0]\n\
         \x20   self withN: x\n",
    );
    assert!(code.contains("make_ref"), "{code}");
}

#[test]
fn guard_with_effectful_return_on_typed_comparison_has_no_frame() {
    // BT-4452: with `x :: Integer`, `x < 0` is a bare BIF, so the effectful
    // `^self withN: 0` (and every other guard shape) is exact as rewritten.
    let code = codegen(
        "Value subclass: W\n\
         \x20 field: n :: Integer = 0\n\
         \x20 clamp: x :: Integer =>\n\
         \x20   x < 0 ifTrue: [^self withN: 0]\n\
         \x20   x > 100 ifTrue: [^self withN: 100] ifFalse: [self withN: x]\n",
    );
    assert_no_nlr_frame(&code);
    assert!(!code.contains("beamtalk_guard_return"), "{code}");
}

#[test]
fn untyped_receiver_guard_is_decided_by_the_helper() {
    // BT-4244: a receiver that is not a Boolean may implement `ifTrue:`
    // itself and must still be sent it, with a block that returns from the
    // method; `beamtalk_guard_return` does that without an NLR frame.
    let code = codegen(
        "Object subclass: U\n\
         \x20 sign: x =>\n\
         \x20   x ifFalse: [^1]\n\
         \x20   0\n",
    );
    assert_no_nlr_frame(&code);
    assert!(
        code.contains("call 'beamtalk_guard_return':'if_false'"),
        "{code}"
    );
}

#[test]
fn closure_nlr_keeps_its_frame() {
    let code = codegen(
        "Object subclass: F\n\
         \x20 find: x in: items =>\n\
         \x20   x isNil ifTrue: [^nil]\n\
         \x20   items do: [:e | e =:= x ifTrue: [^e]]\n\
         \x20   nil\n",
    );
    assert!(
        code.contains("call 'erlang':'throw'") && code.contains("make_ref"),
        "a `^` inside a `do:` closure still needs the NLR frame:\n{code}"
    );
}

/// The text of every binary literal in `code`, decoded (Core Erlang writes a
/// binary as `#{#<Byte>(8,1,...),...}#`), one per line.
fn binary_literal_texts(code: &str) -> String {
    let mut out = Vec::new();
    for binary in code.split("#{").skip(1) {
        let binary = binary.split("}#").next().unwrap_or_default();
        let bytes: Vec<u8> = binary
            .split("#<")
            .skip(1)
            .filter_map(|seg| seg.split('>').next()?.parse().ok())
            .collect();
        out.push(String::from_utf8_lossy(&bytes).into_owned());
    }
    out.join("\n")
}

#[test]
fn reflected_source_keeps_the_guard_as_written() {
    let src = "Object subclass: G\n\
               \x20 sign: x :: Integer =>\n\
               \x20   x > 0 ifTrue: [^1]\n\
               \x20   0\n";
    let code = codegen(src);
    assert_no_nlr_frame(&code);
    let texts = binary_literal_texts(&code);
    assert!(
        texts.contains("x > 0 ifTrue: [^1]"),
        "the method's reflected source is its text as written:\n{texts}"
    );
}

#[test]
fn xref_pairs_receiver_types_with_the_sends_as_written() {
    // An `ifFalse:` guard's rewrite swaps the branch order; xref must still
    // pair each send with its own receiver.
    let code = codegen(
        "Object subclass: X\n\
         \x20 m: x =>\n\
         \x20   x == 1 ifFalse: [^3 printString]\n\
         \x20   \"abc\" size\n",
    );
    assert_no_nlr_frame(&code);
    for (selector, recv_type) in [("printString", "Integer"), ("size", "String")] {
        let row = format!("'selector' => '{selector}', 'line' => ");
        let at = code
            .find(&row)
            .unwrap_or_else(|| panic!("no xref row for {selector}:\n{code}"));
        let rest = &code[at..];
        let row_end = rest.find("}~").expect("row end");
        assert!(
            rest[..row_end].contains(&format!("'recv_type' => '{recv_type}'")),
            "{selector} must carry recv_type {recv_type}: {}",
            &rest[..row_end]
        );
    }
}

/// BT-4451: an Erlang FFI call as the receiver of an actor conditional whose
/// arm writes a field is sequenced ahead of the `case` and runs once.
#[test]
fn ffi_receiver_of_state_threading_actor_conditional_compiles() {
    let code = codegen(
        "Actor subclass: P\n\
         \x20 state: n = 0\n\
         \x20 m: c =>\n\
         \x20   (Erlang beamtalk_guard_return ifTrue: c) ifTrue: [1] ifFalse: [self.n := 2. 3]\n",
    );
    assert_eq!(
        code.matches("'direct_call'('beamtalk_guard_return'")
            .count(),
        1,
        "the FFI receiver must be called exactly once:\n{code}"
    );
    // The FFI result is let-bound ahead of the conditional, whose `case`
    // scrutinee reads that binding: `let _V = ( call …direct_call(…) -| [_] )
    // in … let _Cond@N = _V in case _Cond@N of`.
    let call_at = code
        .find("call 'beamtalk_erlang_proxy':'direct_call'('beamtalk_guard_return'")
        .expect("the FFI call is emitted");
    let binding = code[..call_at]
        .rsplit_once("let ")
        .and_then(|(_, rest)| rest.split_once(" = "))
        .map_or_else(
            || panic!("the FFI call is let-bound:\n{code}"),
            |(var, _)| var,
        );
    let after = &code[call_at..];
    let cond_at = after
        .find("let _Cond@")
        .unwrap_or_else(|| panic!("the conditional follows the FFI call:\n{code}"));
    let (cond_var, rest) = after[cond_at + 4..]
        .split_once(" = ")
        .expect("the conditional binds its scrutinee");
    assert!(
        rest.starts_with(&format!("{binding} in case {cond_var} of")),
        "the conditional's `case` must scrutinise the FFI call's binding `{binding}`:\n{code}"
    );
}

#[test]
fn typed_comparison_guard_on_every_method_kind_has_no_frame() {
    // BT-4452: class-side, a String-literal left operand, and a foreign
    // extension method all lower the comparison bare, so every guard shape
    // compiles as a direct return.
    let class_side = codegen(
        "Object subclass: K\n\
         \x20 class sealed one => 1\n\
         \x20 class sealed pick: x :: Integer =>\n\
         \x20   x > 0 ifTrue: [^self one]\n\
         \x20   2\n",
    );
    assert_no_nlr_frame(&class_side);
    assert!(
        !class_side.contains("beamtalk_guard_return"),
        "{class_side}"
    );

    let literal_left = codegen(
        "Object subclass: L\n\
         \x20 pick: s =>\n\
         \x20   \"m\" < s ifTrue: [^s size]\n\
         \x20   0\n",
    );
    assert_no_nlr_frame(&literal_left);
    assert!(
        !literal_left.contains("beamtalk_guard_return"),
        "{literal_left}"
    );

    let extension = codegen(
        "Integer >> bt4452Pick: x :: Integer =>\n\
         \x20   x > 0 ifTrue: [^self abs]\n\
         \x20   2\n",
    );
    assert!(extension.contains("bt4452Pick"), "{extension}");
    assert_no_nlr_frame(&extension);
}

#[test]
fn shadowed_typed_parameter_keeps_its_frame() {
    // A block parameter named like the typed parameter makes it untrusted
    // (`trusted_param_types`), so `x > 0` may dispatch and the effectful
    // guard keeps its NLR frame.
    let code = codegen(
        "Object subclass: S\n\
         \x20 pick: x :: Integer =>\n\
         \x20   x > 0 ifTrue: [^x abs]\n\
         \x20   x ifNotNil: [:x | x]\n",
    );
    assert!(code.contains("make_ref"), "{code}");
}

#[test]
fn dispatched_typed_comparison_is_reported() {
    // BT-4452: a recorded typed comparison that any lowering dispatched (or
    // none lowered) is reported, so the driver refuses the compile.
    use crate::core_erlang::guard_returns::MethodKey;
    let module = parse_normalised("Object subclass: R\n  m: x :: Integer => x > 0\n").0;
    let method = &module.classes[0].methods[0];
    let key = (MethodKey::of(method), Span::new(0, 1));
    let mut generator = CoreErlangGenerator::new("test");
    generator
        .guard_rewrite
        .boolean_comparisons
        .insert(key.clone());
    assert_eq!(
        generator.unconfirmed_boolean_comparisons(),
        vec![key.clone()]
    );
    generator
        .boolean_comparison_lowerings
        .insert(key.clone(), true);
    assert!(generator.unconfirmed_boolean_comparisons().is_empty());
    generator
        .boolean_comparison_lowerings
        .insert(key.clone(), false);
    assert_eq!(generator.unconfirmed_boolean_comparisons(), vec![key]);
}

#[test]
fn same_file_extension_guard_compiles() {
    // A same-file `X >> m` is runtime-patched, not generated into X's
    // module, so its recorded guards (helper-decided or typed) are not
    // checked there; before BT-4452 they made the compile fail.
    codegen("Object subclass: X\n  n => 1\nX >> pick: x =>\n    x > 0 ifTrue: [^1]\n    2\n");
}

#[test]
fn same_file_extension_guard_is_not_recorded() {
    // BT-4497: the exclusion is made at record time, so the guard-return
    // checks stay fail-closed for every method the module does lower.
    let mut module = parse_normalised(
        "Object subclass: X\n  n => 1\nX >> pick: x =>\n    x > 0 ifTrue: [^1]\n    2\n",
    )
    .0;
    let rewrite = crate::core_erlang::guard_returns::rewrite_guard_returns(&mut module);
    assert!(rewrite.bodies.is_empty());
    assert!(rewrite.conditions.is_empty());
    assert!(rewrite.boolean_comparisons.is_empty());
}

#[test]
fn and_or_guard_of_booleans_has_no_frame() {
    // BT-4496: an inlined `and:`/`or:` of Booleans by construction (here
    // `=:=`, `==` and typed comparisons) is a Boolean, so every guard shape
    // on it compiles as a direct return with no helper.
    let code = codegen(
        "Value subclass: A\n\
         \x20 field: n :: Integer = 0\n\
         \x20 clamp: x :: Integer =>\n\
         \x20   (x < 0 or: [x > 100]) ifTrue: [^self withN: 0]\n\
         \x20   (x =:= 5 and: [x == 5]) ifTrue: [^self withN: 5] ifFalse: [self withN: x]\n",
    );
    assert_no_nlr_frame(&code);
    assert!(!code.contains("beamtalk_guard_return"), "{code}");
}

#[test]
fn and_guard_with_non_boolean_operand_keeps_its_frame() {
    let code = codegen(
        "Value subclass: A\n\
         \x20 field: n :: Integer = 0\n\
         \x20 pick: x =>\n\
         \x20   (x =:= 0 and: [x foo]) ifTrue: [^self withN: 0]\n\
         \x20   self withN: 1\n",
    );
    assert!(code.contains("make_ref"), "{code}");
}

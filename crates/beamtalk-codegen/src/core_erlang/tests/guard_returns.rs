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
}

#[test]
fn object_method_guard_has_no_try() {
    let code = codegen(
        "Object subclass: G\n\
         \x20 sign: x :: Integer =>\n\
         \x20   x > 0 ifTrue: [^1]\n\
         \x20   x < 0 ifTrue: [^-1]\n\
         \x20   0\n",
    );
    assert_no_nlr_frame(&code);
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
fn untyped_boolean_receiver_keeps_its_frame() {
    // BT-4244: a receiver not known to be a Boolean may implement `ifTrue:`
    // itself, and must receive it with a block that returns from the method.
    let code = codegen(
        "Object subclass: U\n\
         \x20 sign: x =>\n\
         \x20   x ifTrue: [^1]\n\
         \x20   0\n",
    );
    assert!(
        code.contains("call 'erlang':'throw'") && code.contains("make_ref"),
        "an untyped `ifTrue:` receiver keeps the NLR form:\n{code}"
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

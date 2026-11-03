// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0143 (BT-4347): binary construction lowers to Core Erlang `#{...}#`
//! in the shapes `erlc +to_core` emits for the same Erlang (verified against
//! OTP's `erlc +to_core`): one test per segment kind, then the operand
//! sequencing (a state-threading operand in a segment value or size).

use super::{assert_compiles_through_erlc, erlc_available};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};

/// Compiles `src` (no parse errors; warnings allowed) to Core Erlang, and
/// through `erlc` when it is available.
fn compile(src: &str) -> String {
    let (module, diagnostics) = parse(lex_with_eof(src));
    assert!(
        diagnostics.iter().all(|d| d.severity != Severity::Error),
        "{src}: {diagnostics:?}"
    );
    let code = generate_module_with_warnings(&module, CodegenOptions::new("bin_test"))
        .unwrap_or_else(|e| panic!("{src}: {e:?}"))
        .code;
    if erlc_available() {
        assert_compiles_through_erlc("bin_test", &code);
    }
    code
}

/// `body` as the body of `build: v with: p` in a plain class.
fn compile_body(body: &str) -> String {
    compile(&format!(
        "Object subclass: Framer\n  build: v with: p =>\n    n := 2\n    {body}\n"
    ))
}

/// The first `#{...}#` construction in the `build:with:` method of `code`
/// (from `#{` to its `}#`).
fn construction(code: &str) -> &str {
    let method = code
        .find("'build:with:'/3 = fun")
        .unwrap_or_else(|| panic!("no build:with: in\n{code}"));
    let start = method
        + code[method..]
            .find("#{")
            .unwrap_or_else(|| panic!("no construction in\n{code}"));
    let end = code[start..]
        .find("}#")
        .unwrap_or_else(|| panic!("unclosed construction in\n{code}"));
    &code[start..start + end + 2]
}

const INT: &str = "'integer',['unsigned'|['big']]";

#[test]
fn integer_variable_with_a_size() {
    let code = compile_body("<<v:4>>");
    let c = construction(&code);
    assert!(c.starts_with("#{#<_BinSeg"), "{c}");
    assert!(c.ends_with(&format!(">(4,1,{INT})}}#")), "{c}");
}

#[test]
fn integer_literal_takes_the_default_size() {
    let code = compile_body("<<16, 3:8, 300:16/little-signed>>");
    assert_eq!(
        construction(&code),
        format!(
            "#{{#<16>(8,1,{INT}), #<3>(8,1,{INT}), #<300>(16,1,'integer',['signed'|['little']])}}#"
        )
    );
}

#[test]
fn whole_binary_tail_is_all_compiler_generated() {
    let code = compile_body("<<p/binary>>");
    let c = construction(&code);
    assert!(
        c.contains(">(('all' -| ['compiler_generated']),8,'binary',['unsigned'|['big']])"),
        "{c}"
    );
    // ADR 0143 § String values: a `String` value builds its UTF-8 bytes.
    assert!(
        code.contains("<{'beamtalk_string', "),
        "no String unwrap in\n{code}"
    );
}

#[test]
fn bits_tail_is_binary_with_unit_one() {
    let code = compile_body("<<p/bits>>");
    assert!(
        construction(&code)
            .contains(">(('all' -| ['compiler_generated']),1,'binary',['unsigned'|['big']])"),
        "{code}"
    );
}

#[test]
fn sized_binary_with_a_variable_size() {
    let code = compile_body("<<p:n/binary>>");
    let c = construction(&code);
    assert!(c.contains(">(_BinSeg"), "{c}");
    assert!(c.contains(",8,'binary',['unsigned'|['big']])"), "{c}");
}

#[test]
fn unit_is_the_second_specifier() {
    let code = compile_body("<<v:8/unit:2>>");
    assert!(
        construction(&code).ends_with(&format!(">(8,2,{INT})}}#")),
        "{code}"
    );
}

#[test]
fn float_variable_takes_64_bits() {
    let code = compile_body("<<v/float, v:32/float-little>>");
    let c = construction(&code);
    assert!(c.contains(">(64,1,'float',['unsigned'|['big']])"), "{c}");
    assert!(c.contains(">(32,1,'float',['unsigned'|['little']])"), "{c}");
}

#[test]
fn float_literal_folds_to_its_bytes() {
    // erlc: <<1.5/float>> → #<63>, #<248>, then six #<0>.
    let code = compile_body("<<1.5/float>>");
    let mut expected = vec![63, 248];
    expected.extend([0; 6]);
    let bytes: Vec<String> = expected
        .iter()
        .map(|b| format!("#<{b}>(8,1,{INT})"))
        .collect();
    assert_eq!(construction(&code), format!("#{{{}}}#", bytes.join(",")));
}

#[test]
fn integer_literal_in_a_float_segment_folds_as_a_float() {
    // erlc: <<1:4/unit:8-float>> → 63,128,0,0 (1.0 as a 32-bit float).
    let code = compile_body("<<1:4/unit:8-float, 1.5:32/float-little>>");
    // Each folded float is one group of byte segments.
    let group = |bytes: &[u8]| {
        bytes
            .iter()
            .map(|b| format!("#<{b}>(8,1,{INT})"))
            .collect::<Vec<_>>()
            .join(",")
    };
    assert_eq!(
        construction(&code),
        format!(
            "#{{{}, {}}}#",
            group(&[63, 128, 0, 0]),
            group(&[0, 0, 192, 63])
        )
    );
}

#[test]
fn native_float_literal_is_not_folded() {
    let code = compile_body("<<1.5/float-native>>");
    assert_eq!(
        construction(&code),
        "#{#<1.5>(64,1,'float',['unsigned'|['native']])}#"
    );
}

#[test]
fn character_literal_is_its_code_point() {
    let code = compile_body("<<$A>>");
    assert_eq!(construction(&code), format!("#{{#<65>(8,1,{INT})}}#"));
}

#[test]
fn string_literal_is_its_utf8_bytes() {
    let code = compile_body("<<\"hé\">>");
    assert_eq!(
        construction(&code),
        format!("#{{#<104>(8,1,{INT}),#<195>(8,1,{INT}),#<169>(8,1,{INT})}}#")
    );
}

#[test]
fn utf_string_literal_expands_per_character() {
    let code = compile_body("<<\"hi\"/utf16>>");
    assert_eq!(
        construction(&code),
        "#{#<104>('undefined','undefined','utf16',['unsigned'|['big']]), #<105>('undefined','undefined','utf16',['unsigned'|['big']])}#"
    );
}

#[test]
fn utf_variable_segment() {
    let code = compile_body("<<v/utf8>>");
    assert!(
        construction(&code).contains(">('undefined','undefined','utf8',['unsigned'|['big']])"),
        "{code}"
    );
}

#[test]
fn empty_construction_and_empty_string() {
    assert_eq!(construction(&compile_body("<<>>")), "#{}#");
    assert_eq!(construction(&compile_body("<<\"\">>")), "#{}#");
}

#[test]
fn computed_value_and_size_are_bound_before_the_construction() {
    let code = compile_body("<<(p size):16, p:(n * 2)/binary>>");
    let c = construction(&code);
    // The construction names only variables and literals.
    assert!(!c.contains("call "), "{c}");
    let size_call = code.find("'size'").expect("p size is sent");
    let construction_at = code.find(c).unwrap();
    assert!(size_call < construction_at, "{code}");
}

#[test]
fn frame_layout_compiles() {
    let code = compile_body("len := p size\n    <<16:8, 3:8, len:24, p/binary>>");
    assert!(code.contains(&format!("(24,1,{INT})")), "{code}");
}

#[test]
fn class_reference_and_block_values_compile() {
    // Values only fail at run time (badarg); codegen lowers them like any
    // operand.
    compile_body("<<(n + 1):8, ([:x | x] value: 3):8>>");
}

#[test]
fn typed_string_literal_is_rejected_by_the_parser() {
    let (_, diagnostics) = parse(lex_with_eof("<<\"hi\"/binary>>"));
    assert!(
        diagnostics.iter().any(|d| d.severity == Severity::Error
            && d.message
                == "a literal string in a binary construction must not have a type or a size"),
        "{diagnostics:?}"
    );
}

// --- Operand sequencing (ADR 0118 literal-container rule) ---

#[test]
fn assignment_in_a_segment_value_is_sequenced_left_to_right() {
    for (context, src) in [
        (
            "class",
            "Object subclass: Seq\n  class go =>\n    t := 0\n    b := <<t:8, (t := t + 1):8, t:8>>\n    #[t, b]\n",
        ),
        (
            "actor",
            "Actor subclass: Seq\n  state: n = 0\n\n  go =>\n    t := 0\n    b := <<t:8, (t := t + 1):8, t:8>>\n    #[t, b]\n",
        ),
        (
            "actor field",
            "Actor subclass: Seq\n  state: n = 0\n\n  go =>\n    <<(self.n):8, (self.n := self.n + 1):8, (self.n):8>>\n",
        ),
        (
            "value type",
            "Value subclass: Seq\n  go =>\n    t := 0\n    <<(t := t + 1):(t + 7)>>\n",
        ),
        (
            "loop",
            "Actor subclass: Seq\n  state: n = 0\n\n  go =>\n    t := 0\n    #(1, 2) do: [:k | b := <<(t := t + k):8, t:8>>]\n    t\n",
        ),
        (
            "self-send operand",
            "Actor subclass: Seq\n  state: n = 0\n\n  bump => self.n := self.n + 1\n  go =>\n    <<(self bump):8, (self bump):8>>\n",
        ),
    ] {
        let code = compile(src);
        assert!(code.contains("#{"), "{context}: {code}");
    }
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary segment lowering shared by both directions (ADR 0143).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A `<<...>>` pattern (`patterns::match_lowering`) and a `<<...>>`
//! construction (`binary_construction`) lower each segment to the same Core
//! Erlang shape, `#<Value>(Size,Unit,Type,Flags)`. This shared leaf module
//! holds the parts that do not depend on the direction's value lowering:
//! the `(Size,Unit,Type,Flags)` specifier tail, the per-character expansion
//! of a `utf*`-typed string literal, and the constant folding of a float
//! literal to its bytes. Free functions with no generator state, so neither
//! direction keeps its own copy.

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_core::ast::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySegmentValue, BinarySignedness,
    Literal,
};
use beamtalk_core::source_analysis::Span;

use super::util::index_lit;

/// Which way a segment goes: the one place the two Core shapes differ is
/// the default size of a sizeless `binary`/`bits` segment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum SegmentDirection {
    /// A segment of a `<<...>>` pattern: `'all'`.
    Pattern,
    /// A segment of a `<<...>>` construction: `('all' -| ['compiler_generated'])`,
    /// exactly what `erlc +to_core` emits for `<<P/binary>>`.
    Construction,
}

/// The `(Size,Unit,Type,Flags)` tail of a Core Erlang binary segment
/// `#<Value>(Size,Unit,Type,Flags)`.
///
/// `size` is the lowered explicit size, or `None` for the type default:
/// `'all'` for `binary`/`bits` (annotated `compiler_generated` in a
/// construction), `'undefined'` for `utf8`/`utf16`/`utf32`, 8 for `integer`
/// and 64 for `float`. The unit is the explicit `unit:N` or the type
/// default (8 for `binary`, 1 for `integer`/`float`/`bits`, `'undefined'`
/// for `utf*`). `bits`/`bitstring` emit the `binary` type with unit 1 —
/// Core Erlang has no `bitstring` type; this is what `erlc` emits. Flags
/// default to unsigned, big-endian.
pub(in crate::core_erlang) fn segment_spec_tail<V>(
    seg: &BinarySegment<V>,
    size: Option<Document<'static>>,
    direction: SegmentDirection,
) -> Document<'static> {
    let seg_type = seg.effective_type();
    let size_doc = size.unwrap_or(match seg_type {
        BinarySegmentType::Binary | BinarySegmentType::Bitstring => match direction {
            SegmentDirection::Pattern => Document::Str("'all'"),
            SegmentDirection::Construction => Document::Str("('all' -| ['compiler_generated'])"),
        },
        BinarySegmentType::Utf8 | BinarySegmentType::Utf16 | BinarySegmentType::Utf32 => {
            Document::Str("'undefined'")
        }
        BinarySegmentType::Integer => Document::Str("8"),
        BinarySegmentType::Float => Document::Str("64"),
    });

    // Unit: explicit override (`/unit:N`, or synthetic split tails from
    // BT-4096) or the type default.
    let unit_doc = match seg.unit {
        Some(unit) => index_lit(unit),
        None => match seg_type {
            BinarySegmentType::Binary => Document::Str("8"),
            BinarySegmentType::Utf8 | BinarySegmentType::Utf16 | BinarySegmentType::Utf32 => {
                Document::Str("'undefined'")
            }
            BinarySegmentType::Integer
            | BinarySegmentType::Float
            | BinarySegmentType::Bitstring => Document::Str("1"),
        },
    };

    let type_doc = match seg_type {
        BinarySegmentType::Integer => Document::Str("'integer'"),
        BinarySegmentType::Float => Document::Str("'float'"),
        BinarySegmentType::Binary | BinarySegmentType::Bitstring => Document::Str("'binary'"),
        BinarySegmentType::Utf8 => Document::Str("'utf8'"),
        BinarySegmentType::Utf16 => Document::Str("'utf16'"),
        BinarySegmentType::Utf32 => Document::Str("'utf32'"),
    };

    // Flags: cons-cell list ['signedness'|['endianness']].
    let sign_doc = match seg.signedness.unwrap_or(BinarySignedness::Unsigned) {
        BinarySignedness::Signed => Document::Str("'signed'"),
        BinarySignedness::Unsigned => Document::Str("'unsigned'"),
    };
    let end_doc = match seg.endianness.unwrap_or(BinaryEndianness::Big) {
        BinaryEndianness::Big => Document::Str("'big'"),
        BinaryEndianness::Little => Document::Str("'little'"),
        BinaryEndianness::Native => Document::Str("'native'"),
    };

    docvec![
        "(", size_doc, ",", unit_doc, ",", type_doc, ",[", sign_doc, "|[", end_doc, "]])"
    ]
}

/// Expands each string literal in a `utf8`/`utf16`/`utf32` segment to one
/// segment per character (BT-4143), as `erlc` does for `<<"hi"/utf16>>`
/// (two `'utf16'` segments). Sizes and units never reach here on such
/// segments (the parser rejects them), so each expansion carries every
/// other specifier unchanged. `char_value` builds a character literal value
/// of the direction's value type.
pub(in crate::core_erlang) fn expand_utf_string_literals<V>(
    segments: &[BinarySegment<V>],
    char_value: fn(char, Span) -> V,
) -> Vec<BinarySegment<V>>
where
    V: BinarySegmentValue + Clone,
{
    let mut expanded: Vec<BinarySegment<V>> = Vec::with_capacity(segments.len());
    for seg in segments {
        let utf_typed = matches!(
            seg.segment_type,
            Some(BinarySegmentType::Utf8 | BinarySegmentType::Utf16 | BinarySegmentType::Utf32)
        );
        match seg.value.binary_literal() {
            Some((Literal::String(s), span)) if utf_typed && seg.size.is_none() => {
                for c in s.chars() {
                    expanded.push(BinarySegment {
                        value: char_value(c, span),
                        size: None,
                        segment_type: seg.segment_type,
                        signedness: seg.signedness,
                        endianness: seg.endianness,
                        unit: seg.unit,
                        span: seg.span,
                    });
                }
            }
            _ => expanded.push(seg.clone()),
        }
    }
    expanded
}

/// The float an integer literal in a `float` segment denotes — `erlc`
/// applies `float/1` to it (BT-4376). Shared by both directions.
#[expect(
    clippy::cast_precision_loss,
    reason = "matches erlc's float/1 conversion: the nearest f64, exactly like the runtime value"
)]
pub(in crate::core_erlang) fn int_to_f64(v: i64) -> f64 {
    v as f64
}

/// The bytes `erlc` folds a constant float segment to: `value` encoded as
/// an IEEE 754 float of the segment's literal bit size (16, 32 or 64,
/// size × unit), big- or little-endian. `None` when the segment cannot be
/// folded — a variable or bad size (`erlc` leaves those to fail at run
/// time) or `native` endianness, which depends on the host.
///
/// An out-of-range value encodes as infinity, as the BEAM builds it
/// (`<<1.0e300:32/float>>` is `<<127,128,0,0>>`).
pub(in crate::core_erlang) fn folded_float_bytes<V>(
    value: f64,
    seg: &BinarySegment<V>,
) -> Option<Vec<u8>> {
    let mut bytes = match seg.literal_bit_size()? {
        16 => f64_to_f16_bits(value).to_be_bytes().to_vec(),
        #[expect(
            clippy::cast_possible_truncation,
            reason = "the IEEE 754 narrowing the BEAM applies: nearest f32, infinity when out of range"
        )]
        32 => (value as f32).to_be_bytes().to_vec(),
        64 => value.to_be_bytes().to_vec(),
        _ => return None,
    };
    match seg.endianness.unwrap_or(BinaryEndianness::Big) {
        BinaryEndianness::Big => {}
        BinaryEndianness::Little => bytes.reverse(),
        BinaryEndianness::Native => return None,
    }
    Some(bytes)
}

/// `value` as an IEEE 754 half-precision float, rounding to nearest-even
/// and overflowing to infinity, as the BEAM encodes a 16-bit float
/// segment. Literal values are finite, so NaN is not handled specially.
fn f64_to_f16_bits(value: f64) -> u16 {
    let bits = value.to_bits();
    let sign = u16::from(bits >> 63 == 1) << 15;
    let biased = i64::try_from((bits >> 52) & 0x7ff).unwrap_or(0);
    let mantissa = bits & 0x000f_ffff_ffff_ffff;
    if biased == 0x7ff {
        // Infinity (NaN is not a literal).
        return sign | 0x7c00;
    }
    // The exponent rebased from f64's bias (1023) to f16's (15).
    let exponent = biased - 1023 + 15;
    if exponent >= 0x1f {
        return sign | 0x7c00;
    }
    if exponent <= 0 {
        // Subnormal (or zero): the 53-bit significand, shifted so its unit
        // is f16's smallest subnormal (2^-24).
        let shift = u32::try_from(43 - exponent).unwrap_or(u32::MAX);
        if shift > 53 {
            return sign;
        }
        let significand = mantissa | (1 << 52);
        // A carry out of the subnormal range lands on the smallest normal.
        return sign | u16::try_from(round_shift_even(significand, shift)).unwrap_or(0x7c00);
    }
    // Normal: a carry out of the 10-bit mantissa bumps the exponent, up to
    // infinity, which the plain sum encodes.
    let rounded = round_shift_even(mantissa, 42);
    let exponent = u64::try_from(exponent).unwrap_or(0);
    let magnitude = (exponent << 10) + rounded;
    sign | u16::try_from(magnitude.min(0x7c00)).unwrap_or(0x7c00)
}

/// `value >> shift`, rounded to nearest with ties to even (`shift` ≥ 1).
fn round_shift_even(value: u64, shift: u32) -> u64 {
    let quotient = value >> shift;
    let remainder = value & ((1u64 << shift) - 1);
    let half = 1u64 << (shift - 1);
    if remainder > half || (remainder == half && quotient & 1 == 1) {
        quotient + 1
    } else {
        quotient
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::ast::Expression;

    fn float_segment(
        size: Option<i64>,
        endianness: Option<BinaryEndianness>,
    ) -> BinarySegment<Expression> {
        BinarySegment {
            value: Expression::Literal(Literal::Float(1.5), Span::default()),
            size: size.map(|n| Box::new(Expression::Literal(Literal::Integer(n), Span::default()))),
            segment_type: Some(BinarySegmentType::Float),
            signedness: None,
            endianness,
            unit: None,
            span: Span::default(),
        }
    }

    /// Values checked against the BEAM: `<<X:16/float>>`.
    #[test]
    fn half_precision_matches_the_beam() {
        let cases: &[(f64, u16)] = &[
            (1.5, 0x3e00),
            (0.1, 0x2e66),
            (65519.0, 0x7bff),
            (65520.0, 0x7c00),
            (70000.0, 0x7c00),
            (1.0e300, 0x7c00),
            (1.0e-8, 0x0000),
            (6.0e-8, 0x0001),
            (1.0e-5, 0x00a8),
            (-0.0, 0x8000),
            (-2.0, 0xc000),
            (0.0, 0x0000),
        ];
        for (value, expected) in cases {
            assert_eq!(f64_to_f16_bits(*value), *expected, "{value}");
        }
    }

    #[test]
    fn float_literals_fold_like_erlc() {
        // erlc: <<1.5/float>> → 63,248,0,0,0,0,0,0
        assert_eq!(
            folded_float_bytes(1.5, &float_segment(None, None)),
            Some(vec![63, 248, 0, 0, 0, 0, 0, 0])
        );
        // <<1.5:32/float-little>> → 0,0,192,63
        assert_eq!(
            folded_float_bytes(
                1.5,
                &float_segment(Some(32), Some(BinaryEndianness::Little))
            ),
            Some(vec![0, 0, 192, 63])
        );
        // <<1.0e300:32/float>> → +infinity
        assert_eq!(
            folded_float_bytes(1.0e300, &float_segment(Some(32), None)),
            Some(vec![127, 128, 0, 0])
        );
        assert_eq!(
            folded_float_bytes(1.5, &float_segment(Some(16), None)),
            Some(vec![62, 0])
        );
    }

    #[test]
    fn unfoldable_float_segments_are_left_to_the_runtime() {
        assert_eq!(
            folded_float_bytes(1.5, &float_segment(Some(24), None)),
            None
        );
        assert_eq!(
            folded_float_bytes(1.5, &float_segment(None, Some(BinaryEndianness::Native))),
            None
        );
    }
}

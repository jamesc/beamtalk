// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary construction lowering (ADR 0143): `<<16:8, len:24, p/binary>>`
//! in expression position builds a binary with Core Erlang's `#{...}#`.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Each segment lowers to the shape `erlc +to_core` emits for the same
//! Erlang, `#<Value>(Size,Unit,Type,Flags)`, through the specifier tail
//! shared with binary patterns ([`super::binary_segments`]):
//!
//! ```text
//! <<v:4, p/binary>>  →  #{( #<V>(4,1,'integer',['unsigned'|['big']]) -| [{'segment', 1}] ),
//!                         ( #<P>(('all' -| ['compiler_generated']),8,'binary',['unsigned'|['big']])
//!                           -| [{'segment', 2}] )}#
//! <<n:8/unit:2>>     →  ( #<N>(8,2,'integer',['unsigned'|['big']]) -| [{'segment', 1}] )
//! <<$A>>             →  ( #<65>(8,1,'integer',['unsigned'|['big']]) -| [{'segment', 1}] )
//! <<"hi">>           →  #<104>(8,1,…),#<105>(8,1,…)      (its UTF-8 bytes)
//! <<1.5/float>>      →  #<63>(8,1,…),#<248>(8,1,…),…      (folded, as erlc does)
//! ```
//!
//! As from `erlc`, every `#<Value>(...)` segment carries its 1-based source
//! segment number (`{'segment', N}`), which the BEAM reports when the
//! construction fails, so the run-time error names the segment's position
//! (ADR 0143 § Runtime failures). Folded constant bytes cannot fail and
//! carry none; a `utf*` string literal expanded per character numbers every
//! character segment with its one source segment.
//!
//! Every segment value and size that is not a literal is an operand of the
//! construction, evaluated left to right (value, then size) before the
//! binary is built. The operands go through [`CoreErlangGenerator::thread_subexprs`],
//! the sequencing every literal container uses (ADR 0118), so a
//! state-threading operand (`<<(x := x + 1):8>>`, an actor self-send) has
//! its prelude spliced in order; each operand is then bound to a fresh
//! variable, so the construction itself only names variables and literals.

use super::binary_segments::{
    SegmentDirection, expand_utf_string_literals, folded_float_bytes, int_to_f64, segment_spec_tail,
};
use super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{
    BinarySegment, BinarySegmentType, BinarySegmentValue, Expression, Literal, binary_literal_size,
};

/// A construction segment's lowered value.
enum SegmentValue<'a> {
    /// A literal, lowered (and possibly folded) by the segment.
    Literal(&'a Literal),
    /// An operand, already bound to this Core variable.
    Var(String),
}

/// Whether a segment value is a raw BEAM bitstring by its syntax alone, so a
/// `binary`/`bits` segment needs no `String` unwrap (ADR 0143 § String
/// values): a nested construction (`<<(<<a:8>>)/binary>>`), through any
/// parentheses, always answers a raw bitstring.
///
/// Deliberately syntactic rather than the type checker's `TypeMap`:
/// `String` is a subclass of `Binary`, so a value typed `Binary` may hold a
/// `{beamtalk_string, B}` at run time, and a declared annotation is not
/// checked at run time either.
fn is_raw_bitstring_by_construction(value: &Expression) -> bool {
    matches!(value.unwrap_parens(), Expression::BinaryConstruction { .. })
}

/// `( Segment -| [{'segment', N}] )`: a construction segment annotated with
/// its 1-based source segment number, the shape `erlc +to_core` emits, so a
/// failed construction's `error_info` cause names the segment's position.
fn segment_numbered(segment: Document<'static>, source_segment: usize) -> Document<'static> {
    let number = i64::try_from(source_segment).unwrap_or(i64::MAX);
    docvec![
        "( ",
        segment,
        " -| [{",
        leaf::atom("segment"),
        ", ",
        leaf::int_lit(number),
        "}] )"
    ]
}

impl CoreErlangGenerator {
    /// Lowers a binary construction (ADR 0143) to `#{...}#`. See the module
    /// documentation for the segment shapes.
    pub(super) fn generate_binary_construction(
        &mut self,
        segments: &[BinarySegment<Expression>],
    ) -> Result<Document<'static>> {
        // Each source segment expanded on its own, so every expanded segment
        // keeps the 1-based number of the source segment it came from: a
        // `utf*` string literal expands to one segment per character, all
        // naming the one source segment in a run-time failure.
        let segments: Vec<(usize, BinarySegment<Expression>)> = segments
            .iter()
            .enumerate()
            .flat_map(|(index, seg)| {
                expand_utf_string_literals(std::slice::from_ref(seg), |c, span| {
                    Expression::Literal(Literal::Character(c), span)
                })
                .into_iter()
                .map(move |expanded| (index + 1, expanded))
            })
            .collect();

        // The operands, in evaluation order: each segment's value, then its
        // size, unless a literal.
        let mut operands: Vec<&Expression> = Vec::new();
        for (_, seg) in &segments {
            if seg.value.binary_literal().is_none() {
                operands.push(&seg.value);
            }
            if let Some(size) = seg.size.as_deref() {
                if binary_literal_size(size).is_none() {
                    operands.push(size);
                }
            }
        }
        let (preamble, docs) = self.thread_subexprs(&operands, "BinOp")?;
        let mut docs = docs.into_iter();
        let mut next_operand = || {
            docs.next().ok_or_else(|| {
                CodeGenError::Internal(
                    "binary construction: fewer operand documents than operands".to_string(),
                )
            })
        };

        let mut bindings: Vec<Document<'static>> = Vec::new();
        let mut parts: Vec<Document<'static>> = Vec::new();
        for (source_segment, seg) in &segments {
            let value = if let Some((lit, _)) = seg.value.binary_literal() {
                SegmentValue::Literal(lit)
            } else {
                let var = self.bind_construction_operand(next_operand()?, &mut bindings);
                // ADR 0143 § String values: a `String` builds its UTF-8
                // bytes in a `binary`/`bits` segment, through the same
                // unwrap as a binary pattern's subject — unless the value
                // is a raw bitstring by construction.
                if matches!(
                    seg.effective_type(),
                    BinarySegmentType::Binary | BinarySegmentType::Bitstring
                ) && !is_raw_bitstring_by_construction(&seg.value)
                {
                    let (prefix, bytes) = self.binary_match_scrutinee(&var);
                    bindings.push(prefix);
                    SegmentValue::Var(bytes)
                } else {
                    SegmentValue::Var(var)
                }
            };
            let size = match seg.size.as_deref() {
                None => None,
                Some(size) => Some(match binary_literal_size(size) {
                    Some((n, _)) => leaf::int_lit(n),
                    None => {
                        leaf::var(self.bind_construction_operand(next_operand()?, &mut bindings))
                    }
                }),
            };
            parts.extend(Self::construction_segment(
                *source_segment,
                seg,
                value,
                size,
            )?);
        }

        let construction = docvec![
            Document::Vec(bindings),
            "#{",
            Self::join_docs_with_commas(parts),
            "}#"
        ];
        Ok(self.close_prelude(&preamble, construction, "BinLit"))
    }

    /// Binds an operand's document to a fresh variable (`let <V> = Doc in `,
    /// appended to `bindings`), answering the variable.
    fn bind_construction_operand(
        &mut self,
        doc: Document<'static>,
        bindings: &mut Vec<Document<'static>>,
    ) -> String {
        let var = self.fresh_temp_var("BinSeg");
        bindings.push(docvec!["let ", leaf::var(var.clone()), " = ", doc, " in "]);
        var
    }

    /// One construction segment's Core segments: usually one, none for an
    /// empty string literal, and one per byte for a string literal or a
    /// folded float.
    ///
    /// A segment that can fail at run time — every `#<Value>(...)` segment,
    /// literal or not, as `erlc` annotates them — carries its 1-based source
    /// segment number as `( #<V>(...) -| [{'segment', N}] )`, which the BEAM
    /// reports in the failure's `format_bs_fail` cause (ADR 0143 § Runtime
    /// failures). Folded constant bytes (a string literal, a constant float)
    /// cannot fail and, as from `erlc`, carry none.
    fn construction_segment(
        source_segment: usize,
        seg: &BinarySegment<Expression>,
        value: SegmentValue<'_>,
        size: Option<Document<'static>>,
    ) -> Result<Vec<Document<'static>>> {
        let seg_type = seg.effective_type();
        let value_doc = match value {
            SegmentValue::Var(var) => leaf::var(var),
            // A bare string literal is its UTF-8 bytes (ADR 0143 § String
            // values). A typed one is rejected by the parser
            // (`check_binary_literal_shapes`), and a `utf*` one was expanded
            // per character, so a specifier here is a compiler bug.
            SegmentValue::Literal(Literal::String(s)) => {
                let bare = seg.size.is_none()
                    && seg.segment_type.is_none()
                    && seg.signedness.is_none()
                    && seg.endianness.is_none()
                    && seg.unit.is_none();
                if !bare {
                    return Err(CodeGenError::Internal(
                        "binary construction: typed string literal reached codegen".to_string(),
                    ));
                }
                return Ok(if s.is_empty() {
                    Vec::new()
                } else {
                    vec![leaf::binary_segments(s)]
                });
            }
            // A number in a `float` segment is a float (`erlc` applies
            // `float/1` to an integer there), folded to its bytes when the
            // size and endianness are constant, as `erlc` folds it.
            SegmentValue::Literal(lit) if seg_type == BinarySegmentType::Float => {
                let float = match lit {
                    Literal::Float(f) => Some(*f),
                    Literal::Integer(v) => Some(int_to_f64(*v)),
                    Literal::Character(c) => Some(f64::from(u32::from(*c))),
                    _ => None,
                };
                match float {
                    Some(f) => {
                        if let Some(bytes) = folded_float_bytes(f, seg) {
                            return Ok(vec![leaf::byte_segments_lit(&bytes)]);
                        }
                        leaf::float_lit(f)
                    }
                    None => Self::literal_doc(lit),
                }
            }
            // `$A` is its code point (`#<65>`); any other literal lowers as
            // itself, and a misfit (`<<1.5>>`, warned at parse time) fails at
            // run time exactly as `erlc` leaves it to.
            SegmentValue::Literal(lit) => Self::literal_doc(lit),
        };
        Ok(vec![segment_numbered(
            docvec![
                "#<",
                value_doc,
                ">",
                segment_spec_tail(seg, size, SegmentDirection::Construction)
            ],
            source_segment,
        )])
    }
}

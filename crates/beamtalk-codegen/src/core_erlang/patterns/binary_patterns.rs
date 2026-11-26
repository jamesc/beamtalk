// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary-pattern match arms: segment planning, segmented and nested arms, hoisted sizes (split from `match_lowering`, BT-5041).
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::binary_segments::{
    SegmentDirection, expand_utf_string_literals, float_segment_value, segment_spec_tail,
};
use crate::core_erlang::util::index_lit;
use crate::core_erlang::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{
    BinarySegment, BinarySegmentType, Expression, Identifier, Literal, MatchArm, Pattern,
};
use beamtalk_core::source_analysis::Span;
use std::collections::HashSet;

use super::match_lowering::MatchThreading;

impl CoreErlangGenerator {
    /// Compiles a top-level binary arm with dependent sizes (BT-4096).
    ///
    /// Core Erlang requires segment sizes bound *before* the pattern, so the
    /// arm's chunks (see [`plan_binary_split`]) become nested matches over
    /// `('all',1,'binary')` rest tails — exactly what `erlc` emits for the
    /// same Erlang. Non-trivial size expressions are evaluated into fresh
    /// variables under `try`/`catch` before their chunk's match, so a
    /// raising size fails the arm instead of crashing (review finding 1).
    /// The arm guard applies at the innermost level (all variables bound);
    /// every level falls through to the later arms.
    /// BT-4173: binds a fresh scrutinee for a top-level binary arm — a
    /// `String` subject (`{'beamtalk_string', bytes}`) unwraps to its bytes
    /// so `<<...>>` keeps matching Strings as UTF-8 binaries (BT-663); a
    /// bare `Binary` (or any other value) passes through untouched, so
    /// non-binaries still fail the native binary pattern and fall through.
    /// `/binary` segments therefore always bind bare binaries (`Binary`).
    ///
    /// Returns the `let`-prefix (ending in `in `) plus the fresh variable
    /// name — the same open-`let`-chain convention
    /// `interpolation_segment_chain` uses.
    ///
    /// ADR 0143 § String values: a construction's `binary`/`bits` segment
    /// value unwraps through this same `case`, so a `String` builds its
    /// UTF-8 bytes in both directions.
    pub(in crate::core_erlang) fn binary_match_scrutinee(
        &mut self,
        match_var: &str,
    ) -> (Document<'static>, String) {
        let scrutinee = self.fresh_temp_var("BinMatch");
        let inner = self.fresh_temp_var("BinInner");
        let other = self.fresh_temp_var("BinOther");
        let prefix = docvec![
            "let ",
            leaf::var(scrutinee.clone()),
            " = case ",
            leaf::var(match_var.to_string()),
            " of ",
            "<{'beamtalk_string', ",
            leaf::var(inner.clone()),
            "}> when 'true' -> ",
            leaf::var(inner),
            " <",
            leaf::var(other.clone()),
            "> when 'true' -> ",
            leaf::var(other),
            " end in ",
        ];
        (prefix, scrutinee)
    }

    /// BT-4223: replaces every binary sub-pattern nested inside a composite
    /// pattern (tuple, list, map value, constructor binding) with a fresh
    /// variable, collecting `(variable, binary pattern)` pairs in source
    /// order. A top-level binary arm is not hoisted — callers route it
    /// through [`Self::binary_match_scrutinee`] directly.
    pub(super) fn hoist_nested_binaries(
        &mut self,
        pattern: &mut Pattern,
        hoisted: &mut Vec<(String, Pattern)>,
    ) {
        match pattern {
            Pattern::Tuple { elements, .. } => {
                for elem in elements {
                    self.hoist_binary_subpattern(elem, hoisted);
                }
            }
            Pattern::List { elements, tail, .. } => {
                for elem in elements {
                    self.hoist_binary_subpattern(elem, hoisted);
                }
                if let Some(tail) = tail {
                    self.hoist_binary_subpattern(tail, hoisted);
                }
            }
            Pattern::Map { pairs, .. } => {
                for pair in pairs {
                    self.hoist_binary_subpattern(&mut pair.value, hoisted);
                }
            }
            Pattern::Constructor { keywords, .. } => {
                for (_, binding) in keywords {
                    self.hoist_binary_subpattern(binding, hoisted);
                }
            }
            // A top-level binary is matched by its own arm path; `Array`
            // and `Type` sub-patterns are rejected by `generate_pattern`;
            // leaves hold no sub-patterns.
            Pattern::Binary { .. }
            | Pattern::Array { .. }
            | Pattern::Type { .. }
            | Pattern::Wildcard(_)
            | Pattern::Literal(..)
            | Pattern::Nil(_)
            | Pattern::Variable(_) => {}
        }
    }

    /// One position of [`Self::hoist_nested_binaries`]: a binary sub-pattern
    /// is swapped for a fresh variable; anything else is searched further.
    fn hoist_binary_subpattern(&mut self, sub: &mut Pattern, hoisted: &mut Vec<(String, Pattern)>) {
        if let Pattern::Binary { span, .. } = sub {
            let tmp = self.fresh_temp_var("BinNested");
            let replacement = Pattern::Variable(Identifier::new(tmp.as_str(), *span));
            let binary = std::mem::replace(sub, replacement);
            hoisted.push((tmp, binary));
        } else {
            self.hoist_nested_binaries(sub, hoisted);
        }
    }

    /// Compiles a match arm whose composite pattern nests binary
    /// sub-patterns (BT-4223).
    ///
    /// Strings are tagged `{'beamtalk_string', bytes}` (BT-4173), so a
    /// native binary sub-pattern would never match a `String` element. The
    /// binaries are hoisted to fresh variables (`outer`/`hoisted`, see
    /// [`Self::hoist_nested_binaries`]); the outer pattern matches first,
    /// then each hoisted value is unwrapped by
    /// [`Self::binary_match_scrutinee`] — exactly like a top-level binary
    /// arm — and matched against its binary pattern in a nested `case`.
    /// The arm guard applies at the innermost level (every variable bound);
    /// every level falls through to the later arms.
    pub(super) fn generate_nested_binary_arm(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        outer: &Pattern,
        hoisted: &[(String, Pattern)],
        rest_arms: &[MatchArm],
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        // Bind all pattern variables, generate guard + body first (mirror
        // the array arm: later arms must not advance generator state first).
        self.push_scope();
        Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let mut guard_part = if let Some(guard) = &arm.guard {
            let gd = self.generate_guard_expression(guard)?;
            Some(docvec![" when ", gd])
        } else {
            None
        };
        let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
        self.pop_scope();

        let shared = self.generate_shared_rest(match_var, rest_arms, threading)?;
        let rest_doc = shared.apply();

        // Each level runs after the outer pattern and every earlier hoisted
        // binary have matched, so a segment size may name their variables
        // (`{n, <<payload:n/binary>>}`): mirror exactly those bindings in
        // scope while each binary pattern is lowered.
        let outer_doc = self.generate_pattern(outer)?;
        self.push_scope();
        Self::collect_pattern_variables(outer, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let mut levels = Vec::with_capacity(hoisted.len());
        for (var, binary) in hoisted {
            let (prefix, scrutinee) = self.binary_match_scrutinee(var);
            let pattern_doc = match self.generate_pattern(binary) {
                Ok(doc) => doc,
                Err(err) => {
                    self.pop_scope();
                    return Err(err);
                }
            };
            Self::collect_pattern_variables(binary, |name, core_var| {
                self.bind_var(name, core_var);
            });
            levels.push((prefix, scrutinee, pattern_doc));
        }
        self.pop_scope();

        // Assemble innermost-first: the last level carries the guard.
        let mut inner = body_doc;
        for (prefix, scrutinee, pattern_doc) in levels.into_iter().rev() {
            let guard = guard_part.take().unwrap_or(Document::Str(" when 'true'"));
            let fallthrough = self.fresh_temp_var("NoMatch");
            inner = docvec![
                prefix,
                "case ",
                leaf::var(scrutinee),
                " of <",
                pattern_doc,
                ">",
                guard,
                " -> ",
                inner,
                " <",
                leaf::var(fallthrough),
                "> when 'true' -> ",
                rest_doc.clone(),
                " end"
            ];
        }

        let fallthrough = self.fresh_temp_var("NoMatch");
        Ok(shared.bind_around(docvec![
            "case ",
            leaf::var(match_var.to_string()),
            " of <",
            outer_doc,
            "> when 'true' -> ",
            inner,
            " <",
            leaf::var(fallthrough),
            "> when 'true' -> ",
            rest_doc,
            " end"
        ]))
    }

    pub(super) fn generate_segmented_binary_arm(
        &mut self,
        match_var: &str,
        arm: &MatchArm,
        segments: &[BinarySegment],
        rest_arms: &[MatchArm],
        threading: Option<&MatchThreading<'_>>,
    ) -> Result<Document<'static>> {
        // Bind all pattern variables, generate guard + body first (mirror
        // the array arm: later arms must not advance generator state first).
        self.push_scope();
        Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
            self.bind_var(name, core_var);
        });
        let guard_part = if let Some(guard) = &arm.guard {
            let gd = self.generate_guard_expression(guard)?;
            docvec![" when ", gd]
        } else {
            Document::Str(" when 'true'")
        };
        let body_doc = self.generate_match_arm_body(&arm.body, threading)?;
        self.pop_scope();

        let shared = self.generate_shared_rest(match_var, rest_arms, threading)?;
        let rest_doc = shared.apply();

        let plan = plan_binary_split(segments);
        debug_assert!(
            plan.len() > 1 || plan.iter().any(|chunk| !chunk.hoists.is_empty()),
            "dispatch only routes split or hoisted plans here"
        );

        // BT-4173: match the first chunk against unwrapped bytes so a
        // `String` subject matches like the `Binary` it encodes.
        let (scrutinee_prefix, scrutinee) = self.binary_match_scrutinee(match_var);
        let levels =
            self.build_segmented_levels(&scrutinee, arm.pattern.span(), segments, &plan)?;
        let arm_doc = self.assemble_segmented_levels(levels, guard_part, body_doc, &rest_doc);
        Ok(shared.bind_around(docvec![scrutinee_prefix, arm_doc]))
    }

    /// Emits one [`SegmentedLevel`] per chunk of a split plan (BT-4096).
    ///
    /// Runs in a single emission scope mirroring the Core-bound variables
    /// (earlier chunks, hoists) so size validation resolves exactly what
    /// the nested matches bind.
    fn build_segmented_levels(
        &mut self,
        match_var: &str,
        arm_span: Span,
        segments: &[BinarySegment],
        plan: &[BinaryChunkPlan],
    ) -> Result<Vec<SegmentedLevel>> {
        self.push_scope();
        let mut levels: Vec<SegmentedLevel> = Vec::with_capacity(plan.len());
        let mut scrutinee = match_var.to_string();
        for (chunk_idx, chunk) in plan.iter().enumerate() {
            let mut hoists = Vec::with_capacity(chunk.hoists.len());
            let mut synthetic: Vec<BinarySegment> = Vec::with_capacity(chunk.end - chunk.start + 1);
            for (offset, seg) in segments[chunk.start..chunk.end].iter().enumerate() {
                let i = chunk.start + offset;
                let size = match &seg.size {
                    Some(size) if chunk.hoists.contains(&i) => {
                        self.check_binary_size(size)?;
                        let fresh = self.fresh_temp_var("Size");
                        let hoist_doc = self.generate_guard_expression(size)?;
                        self.bind_var(&fresh, &fresh);
                        hoists.push((fresh.clone(), hoist_doc));
                        Some(Box::new(Expression::Identifier(Identifier::new(
                            fresh.as_str(),
                            seg.span,
                        ))))
                    }
                    other => other.clone(),
                };
                synthetic.push(BinarySegment {
                    value: seg.value.clone(),
                    size,
                    segment_type: seg.segment_type,
                    signedness: seg.signedness,
                    endianness: seg.endianness,
                    unit: seg.unit,
                    span: seg.span,
                });
            }
            // Non-final chunks match a rest tail for the next level, exactly
            // as `erlc` does: `('all',1,'binary')` (unit 1: the split point
            // may be bit-unaligned).
            let next_scrutinee = if chunk_idx + 1 < plan.len() {
                let tmp = self.fresh_temp_var("Split");
                synthetic.push(BinarySegment {
                    value: Pattern::Variable(Identifier::new(tmp.as_str(), arm_span)),
                    size: None,
                    segment_type: Some(BinarySegmentType::Binary),
                    signedness: None,
                    endianness: None,
                    unit: Some(1),
                    span: arm_span,
                });
                self.bind_var(&tmp, &tmp);
                tmp
            } else {
                String::new()
            };
            let chunk_doc = self.generate_pattern(&Pattern::Binary {
                segments: synthetic,
                span: arm_span,
            })?;
            // Bind this chunk's values for later chunks' sizes.
            for seg in &segments[chunk.start..chunk.end] {
                if let Pattern::Variable(id) = &seg.value {
                    let core_var = Self::to_core_erlang_var(&id.name);
                    self.bind_var(&id.name, &core_var);
                }
            }
            levels.push(SegmentedLevel {
                scrutinee: std::mem::replace(&mut scrutinee, next_scrutinee),
                chunk_doc,
                hoists,
            });
        }
        self.pop_scope();
        Ok(levels)
    }

    /// Assembles [`SegmentedLevel`]s inside-out: the guard applies
    /// innermost; every level falls through to the later arms; each level's
    /// hoists evaluate under `try`/`catch` before its own match (see
    /// [`Self::wrap_hoists_in_try`]).
    fn assemble_segmented_levels(
        &mut self,
        levels: Vec<SegmentedLevel>,
        guard_part: Document<'static>,
        body_doc: Document<'static>,
        rest_doc: &Document<'static>,
    ) -> Document<'static> {
        let mut rev = levels.into_iter().rev();
        let last = rev.next().expect("a split plan always has chunks");
        let fallthrough_var = self.fresh_temp_var("NoMatch");
        let mut next = docvec![
            "case ",
            leaf::var(last.scrutinee),
            " of <",
            last.chunk_doc,
            ">",
            guard_part,
            " -> ",
            body_doc,
            " <",
            leaf::var(fallthrough_var),
            "> when 'true' -> ",
            rest_doc.clone(),
            " end"
        ];
        next = self.wrap_hoists_in_try(last.hoists, next, rest_doc);
        for level in rev {
            let fallthrough_var = self.fresh_temp_var("NoMatch");
            next = docvec![
                "case ",
                leaf::var(level.scrutinee),
                " of <",
                level.chunk_doc,
                "> when 'true' -> ",
                next,
                " <",
                leaf::var(fallthrough_var),
                "> when 'true' -> ",
                rest_doc.clone(),
                " end"
            ];
            next = self.wrap_hoists_in_try(level.hoists, next, rest_doc);
        }
        next
    }

    /// Wraps `next` in one `try`/`catch` per hoist (review finding 1).
    ///
    /// A raising size expression must fail the arm — not crash the match —
    /// so each hoist evaluates under `try`, whose catch answers the later
    /// arms. Observably equivalent to Erlang's `try` → `'bad_size'` (the
    /// whole-scrutinee pre-match `erlc` adds is only an optimization: with
    /// catch-to-rest, a non-binary scrutinee falls through identically).
    fn wrap_hoists_in_try(
        &mut self,
        hoists: Vec<(String, Document<'static>)>,
        next: Document<'static>,
        rest_doc: &Document<'static>,
    ) -> Document<'static> {
        let mut next = next;
        for (var, value) in hoists.into_iter().rev() {
            let (t, e, s) = (
                self.fresh_temp_var("Catch"),
                self.fresh_temp_var("Catch"),
                self.fresh_temp_var("Catch"),
            );
            // Note: concrete Core `try` takes no `end` terminator (unlike
            // `case`/`let` nesting might suggest — see `erlc +to_core`
            // output or `number_coercion_try_catch`).
            next = form::try_of_catch(
                value,
                [leaf::var(var)],
                next,
                [leaf::var(t), leaf::var(e), leaf::var(s)],
                rest_doc.clone(),
                Layout::Flat,
            );
        }
        next
    }

    /// Generates a Core Erlang binary pattern: `#{seg,seg,...}#` (BT-4096).
    ///
    /// Segment sizes resolve left to right. A size naming a variable bound
    /// by an earlier segment of this same pattern cannot emit inline — Core
    /// Erlang requires sizes bound before the pattern — so it is a clean
    /// error here (top-level match arms split instead; see the chain
    /// dispatch, which never routes them here). Anything else bound
    /// (parameters, locals, earlier chunks) emits inline. Each segment's
    /// value binds into a scope that ends with the pattern; the caller binds
    /// all pattern variables separately for the guard/body.
    pub(in crate::core_erlang) fn generate_binary_pattern(
        &mut self,
        segments: &[BinarySegment],
    ) -> Result<Document<'static>> {
        // BT-4143: a string literal with a `utf8`/`utf16`/`utf32` type
        // expands to one segment per character — exactly what `erlc` emits
        // for `<<"hi"/utf16>>` (two `'utf16'` segments). Shared with
        // construction (ADR 0143).
        let expanded = expand_utf_string_literals(segments, |c, span| {
            Pattern::Literal(Literal::Character(c), span)
        });
        let segments = expanded.as_slice();
        self.push_scope();
        let mut bound_here: HashSet<&str> = HashSet::new();
        let mut parts = vec![Document::Str("#{")];
        for (i, seg) in segments.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(","));
            }
            if let Some(size) = &seg.size
                && let Some(name) = size_vars(size).iter().find(|v| bound_here.contains(*v))
            {
                self.pop_scope();
                return Err(CodeGenError::UnsupportedFeature {
                    feature: format!(
                        "variable '{name}' cannot be used as a binary segment size here: it is bound by an earlier segment of the same pattern, and Core Erlang requires sizes bound before the pattern (top-level match arms split automatically; nested patterns must match the enclosing value first)"
                    ),
                    span: Some(seg.span),
                });
            }
            parts.push(self.generate_binary_segment(seg)?);
            if let Pattern::Variable(id) = &seg.value {
                let core_var = Self::to_core_erlang_var(&id.name);
                self.bind_var(&id.name, &core_var);
                bound_here.insert(id.name.as_str());
            }
        }
        parts.push(Document::Str("}#"));
        self.pop_scope();
        Ok(Document::Vec(parts))
    }

    /// Generates a Core Erlang binary segment size (BT-4096, BT-4097).
    ///
    /// A size is a guard position in Core Erlang: only literals, bound
    /// variables, and arithmetic/comparison expressions are valid. Anything
    /// else — a field read, a message send, a variable bound only later in
    /// the pattern — would compile to Core Erlang that `erlc` rejects, so
    /// it is a clean compile error here instead.
    fn generate_binary_size(&mut self, size: &Expression) -> Result<Document<'static>> {
        self.check_binary_size(size)?;
        self.generate_guard_expression(size)
            .map_err(|err| match err {
                CodeGenError::UnsupportedFeature { feature, span } => {
                    CodeGenError::UnsupportedFeature {
                        feature: format!("in binary segment size: {feature}"),
                        span,
                    }
                }
                other => other,
            })
    }

    /// Validates a segment size before [`Self::generate_binary_size`] lowers it.
    ///
    /// Every identifier must already be bound (an earlier segment's value, a
    /// parameter, or a local); anything that is not a literal, a variable, or
    /// a message send is rejected outright (operator validity is left to the
    /// guard lowering, which rephrases the error with size context).
    fn check_binary_size(&self, size: &Expression) -> Result<()> {
        match size {
            Expression::Literal(..) => Ok(()),
            Expression::Identifier(id) => {
                if self.lookup_var(id.name.as_str()).is_none() {
                    Err(CodeGenError::UnsupportedFeature {
                        feature: format!(
                            "variable '{}' cannot be used as a binary segment size here: it is not bound to a value. \
                             A size may name a variable bound by an earlier segment of the same binary pattern, \
                             a method parameter, or a local variable (assign a field to a local first)",
                            id.name
                        ),
                        span: Some(id.span),
                    })
                } else {
                    Ok(())
                }
            }
            Expression::Parenthesized { expression, .. } => self.check_binary_size(expression),
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                self.check_binary_size(receiver)?;
                for arg in arguments {
                    self.check_binary_size(arg)?;
                }
                Ok(())
            }
            other => Err(CodeGenError::UnsupportedFeature {
                feature: "only literals, variables, and arithmetic/comparison expressions are supported in a binary segment size".to_string(),
                span: Some(other.span()),
            }),
        }
    }

    /// Generates a Core Erlang binary segment: `#<Value>(Size,Unit,Type,Flags)`.
    ///
    /// Core Erlang binary segment format:
    /// ```text
    /// #<VarName>(Size, Unit, Type, ['flag1'|['flag2']])
    /// ```
    ///
    /// Defaults: type=integer, signedness=unsigned, endianness=big,
    /// size=8 for integer/float, 'all' for binary/bitstring, 'undefined'
    /// for utf8/utf16/utf32. Unit=1 for integer/float/bitstring, 8 for
    /// binary, 'undefined' for utf8/utf16/utf32.
    pub(in crate::core_erlang) fn generate_binary_segment(
        &mut self,
        seg: &BinarySegment,
    ) -> Result<Document<'static>> {
        // Value: the variable, wildcard, or literal. A string literal folds
        // to one big-endian integer over its bytes — `erlc`-identical for
        // ASCII (see `binary_string_folded_value` for the UTF-8 decision);
        // characters and floats lower through the ordinary literal path
        // (codepoint integer and float literal, respectively).
        //
        // An integer or character literal in a `float` segment lowers as a
        // float literal (BT-4376): `erlc` rewrites `<<1/float>>` to
        // `#<1.0e0>(64,1,'float',…)`, while an integer value there is a
        // clause that can never match.
        let seg_type = seg.segment_type.unwrap_or(BinarySegmentType::Integer);
        let value_doc = match (&seg.value, seg_type) {
            (Pattern::Literal(Literal::String(s), _), _) => binary_string_folded_value(s),
            (
                Pattern::Literal(
                    lit @ (Literal::Integer(_) | Literal::BigInteger(_) | Literal::Character(_)),
                    _,
                ),
                BinarySegmentType::Float,
            ) if float_segment_value(lit).is_some() => {
                leaf::float_lit(float_segment_value(lit).unwrap_or_default())
            }
            _ => self.generate_pattern(&seg.value)?,
        };

        // Size: explicit, or the type default (`segment_spec_tail`); a
        // folded string literal spans its bytes (8 bits each).
        let size_doc = if let Some(size_expr) = &seg.size {
            Some(self.generate_binary_size(size_expr)?)
        } else if let (Pattern::Literal(Literal::String(s), _), BinarySegmentType::Integer) =
            (&seg.value, seg_type)
        {
            Some(index_lit(s.len().saturating_mul(8)))
        } else {
            None
        };

        Ok(docvec![
            "#<",
            value_doc,
            ">",
            segment_spec_tail(seg, size_doc, SegmentDirection::Pattern)
        ])
    }
}

/// One Core Erlang binary pattern's worth of segments (BT-4096).
///
/// A Core binary pattern's sizes must be bound *before* the pattern, so a
/// segment whose size names a variable bound by an earlier segment of the
/// same chunk starts a new chunk; each chunk matches the previous chunk's
/// rest tail (mirroring what `erlc` emits for the same Erlang).
pub(in crate::core_erlang) struct BinaryChunkPlan {
    /// First segment index (inclusive) within the arm's segment list.
    pub(in crate::core_erlang) start: usize,
    /// Last segment index (exclusive) within the arm's segment list.
    pub(in crate::core_erlang) end: usize,
    /// Absolute indices of segments whose size is a non-trivial expression,
    /// replaced by a hoisted fresh variable evaluated before this chunk.
    pub(in crate::core_erlang) hoists: Vec<usize>,
}

/// Splits binary segments into [`BinaryChunkPlan`]s (BT-4096, BT-4097).
///
/// Pure over the pattern AST: a size naming a variable bound by an earlier
/// segment of the current chunk forces a split there; names bound by
/// earlier chunks or outside the pattern need none (validated at emission).
/// More than one chunk means the arm needs segmented lowering.
pub(in crate::core_erlang) fn plan_binary_split(
    segments: &[BinarySegment],
) -> Vec<BinaryChunkPlan> {
    let mut chunks = Vec::new();
    let mut chunk_start = 0;
    let mut hoists = Vec::new();
    let mut bound_in_chunk: HashSet<&str> = HashSet::new();
    for (i, seg) in segments.iter().enumerate() {
        if let Some(size) = &seg.size {
            if size_vars(size).iter().any(|v| bound_in_chunk.contains(*v)) {
                chunks.push(BinaryChunkPlan {
                    start: chunk_start,
                    end: i,
                    hoists: std::mem::take(&mut hoists),
                });
                chunk_start = i;
                bound_in_chunk.clear();
            }
            if !is_trivial_size(size) {
                hoists.push(i);
            }
        }
        if let Pattern::Variable(id) = &seg.value {
            bound_in_chunk.insert(id.name.as_str());
        }
    }
    chunks.push(BinaryChunkPlan {
        start: chunk_start,
        end: segments.len(),
        hoists,
    });
    chunks
}

/// True when a top-level binary pattern needs segmented lowering: more
/// than one chunk, or hoisted size expressions (BT-4096, BT-4097). Either
/// way the pattern cannot emit as one native Core binary pattern.
pub(super) fn binary_needs_segmented(segments: &[BinarySegment]) -> bool {
    let plan = plan_binary_split(segments);
    plan.len() > 1 || plan.iter().any(|chunk| !chunk.hoists.is_empty())
}

/// Folds a string literal's bytes into the single big-endian integer used
/// as its segment value (`<<"hi">>` is `#<26729>(16,1,'integer',...)`),
/// rendered decimal. The fold runs over UTF-8 bytes — Beamtalk source is
/// UTF-8, so this matches the literal's runtime bytes exactly (this is a
/// deliberate divergence from `erlc`, which folds the source file's raw
/// encoding bytes instead). Core Erlang integers are bignums, so
/// [`leaf::bigint_lit`] renders strings of any length — no `i64`/`u64`
/// ceiling at 8 bytes.
fn binary_string_folded_value(s: &str) -> Document<'static> {
    leaf::bigint_lit(s.as_bytes())
}

/// True for sizes that emit inline: literals and variables, with any
/// parentheses unwrapped. Only genuine expressions hoist, so `<<a:(8)>>`
/// stays a one-pattern match (review finding 6).
fn is_trivial_size(size: &Expression) -> bool {
    match size {
        Expression::Literal(..) | Expression::Identifier(..) => true,
        Expression::Parenthesized { expression, .. } => is_trivial_size(expression),
        _ => false,
    }
}

/// Collects every variable named in a segment size expression.
fn size_vars(size: &Expression) -> Vec<&str> {
    fn inner<'a>(size: &'a Expression, vars: &mut Vec<&'a str>) {
        match size {
            Expression::Identifier(id) => vars.push(id.name.as_str()),
            Expression::Parenthesized { expression, .. } => inner(expression, vars),
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                inner(receiver, vars);
                for arg in arguments {
                    inner(arg, vars);
                }
            }
            _ => {}
        }
    }
    let mut vars = Vec::new();
    inner(size, &mut vars);
    vars
}

/// Whether `pattern` is, or nests inside a composite pattern, a binary
/// pattern (BT-4173/BT-4223): such an arm needs a per-arm `String` unwrap
/// the all-native `case` fast path can't express.
pub(super) fn pattern_has_binary(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Binary { .. } => true,
        Pattern::Tuple { elements, .. } => elements.iter().any(pattern_has_binary),
        Pattern::List { elements, tail, .. } => {
            elements.iter().any(pattern_has_binary)
                || tail.as_deref().is_some_and(pattern_has_binary)
        }
        Pattern::Map { pairs, .. } => pairs.iter().any(|pair| pattern_has_binary(&pair.value)),
        Pattern::Constructor { keywords, .. } => keywords
            .iter()
            .any(|(_, binding)| pattern_has_binary(binding)),
        Pattern::Array { .. }
        | Pattern::Type { .. }
        | Pattern::Wildcard(_)
        | Pattern::Literal(..)
        | Pattern::Nil(_)
        | Pattern::Variable(_) => false,
    }
}

/// Span of a binary pattern needing segmented lowering nested inside a
/// composite pattern, if any. Only a top-level match-arm binary pattern
/// splits (BT-4096); anything nested is a clean compile error at the call
/// site.
pub(super) fn nested_split_span(pattern: &Pattern) -> Option<Span> {
    match pattern {
        Pattern::Binary { segments, span } => {
            if binary_needs_segmented(segments) {
                Some(*span)
            } else {
                // Non-split binaries are fine nested — but recurse into
                // segment values defensively (constructed ASTs only).
                segments
                    .iter()
                    .find_map(|seg| nested_split_span(&seg.value))
            }
        }
        Pattern::Tuple { elements, .. } | Pattern::List { elements, .. } => {
            elements.iter().find_map(nested_split_span).or_else(|| {
                if let Pattern::List { tail: Some(t), .. } = pattern {
                    nested_split_span(t)
                } else {
                    None
                }
            })
        }
        Pattern::Array { elements, rest, .. } => elements
            .iter()
            .find_map(nested_split_span)
            .or_else(|| rest.as_ref().and_then(|r| nested_split_span(r))),
        Pattern::Map { pairs, .. } => pairs.iter().find_map(|pair| nested_split_span(&pair.value)),
        Pattern::Constructor { keywords, .. } => {
            keywords.iter().find_map(|(_, b)| nested_split_span(b))
        }
        Pattern::Wildcard(_)
        | Pattern::Literal(..)
        | Pattern::Nil(_)
        | Pattern::Variable(_)
        | Pattern::Type { .. } => None,
    }
}

/// One nested level of a segmented binary arm (BT-4096): this chunk's
/// pattern, the size-expression hoists evaluated before it, and the
/// scrutinee it matches.
struct SegmentedLevel {
    scrutinee: String,
    chunk_doc: Document<'static>,
    hoists: Vec<(String, Document<'static>)>,
}

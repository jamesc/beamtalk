// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Instance-variable access query — find reads/writes of a named instance
//! variable within a single method's source.
//!
//! **DDD Context:** Language Service
//!
//! Backs `SystemNavigation instVarReadersOf:in:` and `instVarWritersOf:in:`
//! (BT-2208). Where `sendersOf:` answers "who calls this method?" and
//! `referencesTo:` answers "who mentions this class?", these queries answer
//! "which methods read (or write) this slot?". Given the source text of a
//! single compiled method (as returned by `CompiledMethod source`) and an
//! instance-variable name, each query returns the 1-based line numbers,
//! relative to the method source, at which the named slot is read (or written).
//!
//! # What counts as a read vs a write
//!
//! In Beamtalk an instance variable is accessed as `self.x`, parsed as an
//! [`Expression::FieldAccess`] whose `field` name is the slot name. A bare `x`
//! is a *local* variable, not an instance variable, and is ignored.
//!
//! - A **write** is a `FieldAccess` (with the matching field name) that appears
//!   as the `target` of an [`Expression::Assignment`] (`self.x := ...`).
//! - A **read** is any other `FieldAccess` with the matching field name,
//!   including a `FieldAccess` nested inside an assignment's `value`
//!   (`self.x := self.x + 1` reads `x` on the RHS and writes it on the LHS).
//!
//! Following Pharo precedent, the assignment *target* `self.x` counts as a
//! write **only** — it is not also reported as a read. The reader walker
//! therefore treats an assignment's target specially: it descends into the
//! target's receiver (so a deeper read inside the receiver is still found) but
//! does not report the target `FieldAccess` itself, while still recursing into
//! the assignment `value` where ordinary read rules apply.
//!
//! # Parsing strategy
//!
//! Reuses the synthetic-class-header trick from
//! [`crate::queries::senders_query`]: a bare method definition is not a valid
//! top-level form, so the input is wrapped in
//! `Object subclass: __SyntheticIvarScope\n` before lexing/parsing. Line
//! numbers are translated back to input-source space by subtracting the prefix
//! line count.
//!
//! # Error handling
//!
//! Parse errors are tolerated: any sub-trees that parsed successfully still
//! contribute results. A completely unparseable source returns an empty list.
//! Callers treat "no accesses found" identically to "could not parse".

use crate::ast::{Expression, Pattern, StringSegment};
use crate::source_analysis::{Span, lex_with_eof, parse};

/// Number of newlines in the synthetic class header that wraps the input.
/// Used to translate line numbers from wrapped-source to input-source space.
const PREFIX_LINES: u32 = 1;

/// The synthetic class header used to make a bare method definition parseable.
/// The single newline at the end is counted by [`PREFIX_LINES`]. The class
/// name uses a leading underscore so it cannot collide with a real class.
const SYNTHETIC_PREFIX: &str = "Object subclass: __SyntheticIvarScope\n";

/// Which kind of instance-variable access the walker is collecting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AccessKind {
    /// A read of the slot (`self.x` anywhere except an assignment target).
    Read,
    /// A write of the slot (`self.x := ...` — the assignment target).
    Write,
}

/// Find the 1-based line numbers within `method_source` where the instance
/// variable `ivar_name` is **read**.
///
/// Walks the parsed AST for [`Expression::FieldAccess`] nodes whose `field`
/// matches `ivar_name`, excluding those that appear as an assignment target
/// (those are writes only). Each matching read produces one entry; multiple
/// reads on the same line produce multiple entries, preserving source order.
///
/// Returns an empty vector if no reads are found or the source cannot be parsed
/// at all.
#[must_use]
pub fn find_ivar_readers_in_source(method_source: &str, ivar_name: &str) -> Vec<u32> {
    collect_ivar_lines(method_source, ivar_name, AccessKind::Read)
}

/// Find the 1-based line numbers within `method_source` where the instance
/// variable `ivar_name` is **written** (assigned).
///
/// Walks the parsed AST for [`Expression::Assignment`] nodes whose `target` is
/// a [`Expression::FieldAccess`] with a matching field name. Each matching
/// write produces one entry; multiple writes on the same line produce multiple
/// entries, preserving source order.
///
/// Returns an empty vector if no writes are found or the source cannot be
/// parsed at all.
#[must_use]
pub fn find_ivar_writers_in_source(method_source: &str, ivar_name: &str) -> Vec<u32> {
    collect_ivar_lines(method_source, ivar_name, AccessKind::Write)
}

/// Shared driver for both queries: wrap, parse, walk, and translate line
/// numbers back to input-source space.
fn collect_ivar_lines(method_source: &str, ivar_name: &str, kind: AccessKind) -> Vec<u32> {
    let wrapped = format!("{SYNTHETIC_PREFIX}{method_source}");
    let tokens = lex_with_eof(&wrapped);
    let (module, _diags) = parse(tokens);

    let mut wrapped_lines = Vec::new();

    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for stmt in &method.body {
                collect_access_lines(
                    &stmt.expression,
                    ivar_name,
                    kind,
                    &wrapped,
                    &mut wrapped_lines,
                );
            }
        }
    }

    // Sources that look like top-level expressions, or that parsed as
    // standalone `Class >> selector => body` definitions, are walked as
    // fallbacks so partial-parse cases still contribute results.
    for stmt in &module.expressions {
        collect_access_lines(
            &stmt.expression,
            ivar_name,
            kind,
            &wrapped,
            &mut wrapped_lines,
        );
    }
    for smd in &module.method_definitions {
        for stmt in &smd.method.body {
            collect_access_lines(
                &stmt.expression,
                ivar_name,
                kind,
                &wrapped,
                &mut wrapped_lines,
            );
        }
    }

    // Translate from wrapped-source line numbers back to input-source space.
    wrapped_lines
        .into_iter()
        .map(|line| line.saturating_sub(PREFIX_LINES).max(1))
        .collect()
}

/// Collect a stand-alone `FieldAccess` (i.e. one that is NOT an assignment
/// target). Outside an assignment target a `self.x` access is a read; it is
/// never a write (writes only come from the `Assignment` arm). The receiver is
/// walked so a nested access in the receiver is still found.
fn collect_field_access(
    receiver: &Expression,
    field: &crate::ast::Identifier,
    span: Span,
    ivar_name: &str,
    kind: AccessKind,
    source: &str,
    lines: &mut Vec<u32>,
) {
    if kind == AccessKind::Read && field.name == ivar_name {
        lines.push(field_access_line(field.span, span, source));
    }
    collect_access_lines(receiver, ivar_name, kind, source, lines);
}

/// Collect an `Assignment`. The target is a write only — never also a read
/// (Pharo precedent). When collecting writes, a target that is a `FieldAccess`
/// with the matching field name is reported. The target's receiver is still
/// walked (so a read embedded in the receiver is not lost), but the target
/// field itself is never reported as a read — this is the rule that keeps
/// `self.x := ...` from double-counting `x` as both a read and a write. Reads on
/// the right-hand side count normally.
fn collect_assignment(
    target: &Expression,
    value: &Expression,
    ivar_name: &str,
    kind: AccessKind,
    source: &str,
    lines: &mut Vec<u32>,
) {
    if let Expression::FieldAccess {
        receiver,
        field,
        span,
    } = target
    {
        if kind == AccessKind::Write && field.name == ivar_name {
            lines.push(field_access_line(field.span, *span, source));
        }
        // Walk the target's receiver only — NOT the target field as a read.
        collect_access_lines(receiver, ivar_name, kind, source, lines);
    } else {
        // Non-field target (a local identifier or destructure): it cannot be an
        // ivar write, so walk it normally.
        collect_access_lines(target, ivar_name, kind, source, lines);
    }
    collect_access_lines(value, ivar_name, kind, source, lines);
}

/// Recursively collect line numbers of instance-variable accesses of `kind`
/// matching `ivar_name`.
fn collect_access_lines(
    expr: &Expression,
    ivar_name: &str,
    kind: AccessKind,
    source: &str,
    lines: &mut Vec<u32>,
) {
    match expr {
        Expression::FieldAccess {
            receiver,
            field,
            span,
        } => collect_field_access(receiver, field, *span, ivar_name, kind, source, lines),
        Expression::Assignment { target, value, .. } => {
            collect_assignment(target, value, ivar_name, kind, source, lines);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            collect_access_lines(receiver, ivar_name, kind, source, lines);
            for arg in arguments {
                collect_access_lines(arg, ivar_name, kind, source, lines);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_access_lines(receiver, ivar_name, kind, source, lines);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_access_lines(arg, ivar_name, kind, source, lines);
                }
            }
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            collect_access_lines(value, ivar_name, kind, source, lines);
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                collect_access_lines(&stmt.expression, ivar_name, kind, source, lines);
            }
        }
        Expression::Parenthesized { expression, .. } => {
            collect_access_lines(expression, ivar_name, kind, source, lines);
        }
        Expression::Match { value, arms, .. } => {
            collect_access_lines(value, ivar_name, kind, source, lines);
            for arm in arms {
                collect_pattern_access_lines(&arm.pattern, ivar_name, kind, source, lines);
                if let Some(guard) = &arm.guard {
                    collect_access_lines(guard, ivar_name, kind, source, lines);
                }
                collect_access_lines(&arm.body, ivar_name, kind, source, lines);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let StringSegment::Interpolation(inner) = segment {
                    collect_access_lines(inner, ivar_name, kind, source, lines);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_access_lines(element, ivar_name, kind, source, lines);
            }
            if let Some(tail_expr) = tail {
                collect_access_lines(tail_expr, ivar_name, kind, source, lines);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_access_lines(element, ivar_name, kind, source, lines);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_access_lines(&pair.key, ivar_name, kind, source, lines);
                collect_access_lines(&pair.value, ivar_name, kind, source, lines);
            }
        }
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

/// Recursively collect line numbers of instance-variable accesses inside a
/// [`Pattern`].
///
/// Patterns are name-binding/literal-matching constructs and carry no field
/// accesses today — the only expression slot in the [`Pattern`] enum is a
/// `BinarySegment::size`, which the parser restricts to an integer literal or a
/// bare identifier. The walker is defensive (mirroring `senders_query`): it
/// traverses container patterns and the binary-segment size expression so that
/// loosening the parser later does not silently regress these queries.
fn collect_pattern_access_lines(
    pattern: &Pattern,
    ivar_name: &str,
    kind: AccessKind,
    source: &str,
    lines: &mut Vec<u32>,
) {
    match pattern {
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                collect_pattern_access_lines(&segment.value, ivar_name, kind, source, lines);
                if let Some(size) = &segment.size {
                    collect_access_lines(size, ivar_name, kind, source, lines);
                }
            }
        }
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                collect_pattern_access_lines(element, ivar_name, kind, source, lines);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                collect_pattern_access_lines(element, ivar_name, kind, source, lines);
            }
            if let Some(rest_pattern) = rest {
                collect_pattern_access_lines(rest_pattern, ivar_name, kind, source, lines);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_pattern_access_lines(element, ivar_name, kind, source, lines);
            }
            if let Some(tail_pattern) = tail {
                collect_pattern_access_lines(tail_pattern, ivar_name, kind, source, lines);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_access_lines(&pair.value, ivar_name, kind, source, lines);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_selector, inner) in keywords {
                collect_pattern_access_lines(inner, ivar_name, kind, source, lines);
            }
        }
        Pattern::Wildcard(..) | Pattern::Literal(..) | Pattern::Variable(..) => {}
    }
}

/// Determine the line number to report for a matching access. Uses the span of
/// the field-name identifier (so the line points at the slot token rather than
/// the receiver, for multi-line `self\n  .x` accesses). `_fallback` is the
/// enclosing field-access span, kept for symmetry with `senders_query` and in
/// case the field span is ever made optional; it is unused today because the
/// field identifier always carries a valid span.
fn field_access_line(field_span: Span, _fallback: Span, source: &str) -> u32 {
    field_span.line_number(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_pure_reader() {
        // `getValue => self.value` reads `value` once on line 1.
        let lines = find_ivar_readers_in_source("getValue => self.value", "value");
        assert_eq!(lines, vec![1]);
        // No writes in a pure reader.
        let writes = find_ivar_writers_in_source("getValue => self.value", "value");
        assert!(writes.is_empty());
    }

    #[test]
    fn finds_pure_writer() {
        // `reset => self.value := 0` writes `value` once; the literal RHS has
        // no read of `value`.
        let writes = find_ivar_writers_in_source("reset => self.value := 0", "value");
        assert_eq!(writes, vec![1]);
        let reads = find_ivar_readers_in_source("reset => self.value := 0", "value");
        assert!(reads.is_empty());
    }

    #[test]
    fn read_modify_write_counts_in_both_sets() {
        // `increment => self.value := self.value + 1` reads `value` on the RHS
        // and writes it on the LHS — exactly one of each, on line 1.
        let src = "increment => self.value := self.value + 1";
        let reads = find_ivar_readers_in_source(src, "value");
        assert_eq!(reads, vec![1], "RHS self.value is a read");
        let writes = find_ivar_writers_in_source(src, "value");
        assert_eq!(writes, vec![1], "LHS self.value is a write");
    }

    #[test]
    fn assignment_target_is_not_also_a_read() {
        // `set => self.value := 0` — the target `self.value` must NOT show up
        // as a read (Pharo precedent). The RHS is a literal, so there are zero
        // reads.
        let reads = find_ivar_readers_in_source("set => self.value := 0", "value");
        assert!(
            reads.is_empty(),
            "assignment target must not count as a read, got {reads:?}"
        );
    }

    #[test]
    fn bare_identifier_is_not_an_ivar() {
        // A bare `value` is a LOCAL variable, not `self.value` — neither a read
        // nor a write of the slot.
        let src = "compute =>\n  value := 1\n  value + 1";
        assert!(find_ivar_readers_in_source(src, "value").is_empty());
        assert!(find_ivar_writers_in_source(src, "value").is_empty());
    }

    #[test]
    fn finds_read_inside_block() {
        // `report => [:x | x + self.value] value: 1` reads `value` on line 2.
        let src = "report =>\n  [:x | x + self.value] value: 1";
        let reads = find_ivar_readers_in_source(src, "value");
        assert_eq!(reads, vec![2]);
    }

    #[test]
    fn finds_write_inside_block() {
        // `bump => items do: [:i | self.total := self.total + i]` writes
        // `total` on line 2 and reads it on line 2.
        let src = "bump =>\n  items do: [:i | self.total := self.total + i]";
        let writes = find_ivar_writers_in_source(src, "total");
        assert_eq!(writes, vec![2]);
        let reads = find_ivar_readers_in_source(src, "total");
        assert_eq!(reads, vec![2]);
    }

    #[test]
    fn finds_read_in_message_argument() {
        // `send => target at: self.value put: 1` reads `value` on line 1.
        let reads = find_ivar_readers_in_source("send => target at: self.value put: 1", "value");
        assert_eq!(reads, vec![1]);
    }

    #[test]
    fn finds_read_in_cascade_argument() {
        // Cascade message arguments are walked for reads.
        let src = "report =>\n  Transcript\n    show: self.value;\n    show: self.value";
        let reads = find_ivar_readers_in_source(src, "value");
        assert_eq!(reads, vec![3, 4]);
    }

    #[test]
    fn multiple_writes_to_same_slot() {
        // Three sequential writes to `count`, each also reading it on the RHS.
        let src = "addThrice: n =>\n  self.count := self.count + n\n  self.count := self.count + n\n  self.count := self.count + n";
        let writes = find_ivar_writers_in_source(src, "count");
        assert_eq!(writes, vec![2, 3, 4]);
        let reads = find_ivar_readers_in_source(src, "count");
        assert_eq!(reads, vec![2, 3, 4]);
    }

    #[test]
    fn does_not_match_other_slots() {
        // A query for `count` must not match `self.total`.
        let src = "swap =>\n  self.count := self.total\n  self.total := self.count";
        let reads = find_ivar_readers_in_source(src, "count");
        // `self.count` read appears only on line 3 (RHS of the second
        // assignment); line 2's `self.count` is a write target, not a read.
        assert_eq!(reads, vec![3]);
        let writes = find_ivar_writers_in_source(src, "count");
        assert_eq!(writes, vec![2]);
    }

    #[test]
    fn finds_read_in_return_value() {
        // An early return reading the slot counts as a read.
        let src = "guard =>\n  done ifTrue: [^self.value]\n  -1";
        let reads = find_ivar_readers_in_source(src, "value");
        assert_eq!(reads, vec![2]);
    }

    #[test]
    fn returns_empty_for_untouched_slot() {
        let reads = find_ivar_readers_in_source("greet => self name", "value");
        assert!(reads.is_empty());
        let writes = find_ivar_writers_in_source("greet => self name", "value");
        assert!(writes.is_empty());
    }

    #[test]
    fn handles_unparseable_source_without_panicking() {
        let reads = find_ivar_readers_in_source(")@!", "value");
        assert!(reads.is_empty());
        let writes = find_ivar_writers_in_source(")@!", "value");
        assert!(writes.is_empty());
    }

    #[test]
    fn finds_access_in_class_method_with_return_type() {
        // Class-side methods carry a return-type arrow in the signature. The
        // walker must still find a `classState:` access in the body.
        let src = "create -> Counter =>\n  self.instanceCount := self.instanceCount + 1";
        let writes = find_ivar_writers_in_source(src, "instanceCount");
        assert_eq!(writes, vec![2]);
        let reads = find_ivar_readers_in_source(src, "instanceCount");
        assert_eq!(reads, vec![2]);
    }

    #[test]
    fn finds_read_in_map_literal() {
        // A slot read embedded in a map literal value is found.
        let src = "snapshot =>\n  #{#v => self.value}";
        let reads = find_ivar_readers_in_source(src, "value");
        assert_eq!(reads, vec![2]);
    }
}

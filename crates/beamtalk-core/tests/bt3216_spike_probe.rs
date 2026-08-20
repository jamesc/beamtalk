// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//! BT-3216 / ADR 0115 Phase 1 spike instrumentation.
//!
//! Measures how many message-send *receiver* expressions across `stdlib/src`
//! have an entry in the type checker's [`TypeMap`], and which `InferredType`
//! variant they carry — the numbers behind
//! `docs/internal/adr-0115-phase1-spike-findings.md` §1.
//!
//! `#[ignore]` on purpose: this reports, it does not assert, and it reads
//! `stdlib/src` through a path relative to the crate directory. Run it
//! explicitly:
//!
//! ```text
//! cargo test -p beamtalk-core --test bt3216_spike_probe -- --ignored --nocapture
//! ```

use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;
use beamtalk_core::semantic_analysis::type_checker::{InferredType, infer_types_and_returns};
use beamtalk_core::source_analysis::{Span, lex_with_eof, parse};

/// Percentage of `d`, guarding a zero denominator.
#[expect(
    clippy::cast_precision_loss,
    reason = "reporting-only counts, far below f64's 2^53 exact-integer range"
)]
fn pct(n: usize, d: usize) -> f64 {
    100.0 * n as f64 / d.max(1) as f64
}

fn collect_receiver_spans(expr: &Expression, out: &mut Vec<(String, Span)>) {
    use beamtalk_core::ast::MessageSelector;
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            let name = match selector {
                MessageSelector::Unary(n) | MessageSelector::Binary(n) => n.to_string(),
                MessageSelector::Keyword(parts) => {
                    parts.iter().map(|p| p.keyword.as_str()).collect()
                }
            };
            out.push((name, receiver.span()));
            collect_receiver_spans(receiver, out);
            for a in arguments {
                collect_receiver_spans(a, out);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            out.push(("<cascade>".to_string(), receiver.span()));
            collect_receiver_spans(receiver, out);
            for m in messages {
                for a in &m.arguments {
                    collect_receiver_spans(a, out);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => collect_receiver_spans(expression, out),
        Expression::Return { value, .. } | Expression::DestructureAssignment { value, .. } => {
            collect_receiver_spans(value, out);
        }
        Expression::Assignment { target, value, .. } => {
            collect_receiver_spans(target, out);
            collect_receiver_spans(value, out);
        }
        Expression::FieldAccess { receiver, .. } => collect_receiver_spans(receiver, out),
        Expression::Block(b) => {
            for stmt in &b.body {
                collect_receiver_spans(&stmt.expression, out);
            }
        }
        Expression::Match { value, arms, .. } => {
            collect_receiver_spans(value, out);
            for arm in arms {
                collect_receiver_spans(&arm.body, out);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for e in elements {
                collect_receiver_spans(e, out);
            }
            if let Some(t) = tail {
                collect_receiver_spans(t, out);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for e in elements {
                collect_receiver_spans(e, out);
            }
        }
        _ => {}
    }
}

/// Per-variant tallies of how the type checker typed each send receiver.
#[derive(Default)]
struct Counts {
    entries: usize,
    receivers: usize,
    in_map: usize,
    known: usize,
    dynamic: usize,
    meta: usize,
    union: usize,
    other: usize,
}

impl Counts {
    fn tally(&mut self, ty: Option<&InferredType>) {
        match ty {
            Some(InferredType::Known { .. }) => self.known += 1,
            Some(InferredType::Dynamic(_)) => self.dynamic += 1,
            Some(InferredType::Meta { .. }) => self.meta += 1,
            Some(InferredType::Union { .. }) => self.union += 1,
            Some(_) => self.other += 1,
            None => return,
        }
        self.in_map += 1;
    }
}

/// Type-check one file and fold its receiver types into `counts`.
fn scan_file(path: &str, counts: &mut Counts) -> usize {
    let Ok(source) = std::fs::read_to_string(path) else {
        return 0;
    };
    let tokens = lex_with_eof(&source);
    let (module, _diags) = parse(tokens);
    let (Ok(hierarchy), _) = ClassHierarchy::build(&module) else {
        println!("  SKIP {path}: hierarchy build failed");
        return 0;
    };
    let (type_map, _) = infer_types_and_returns(&module, &hierarchy, None);

    let mut recvs = Vec::new();
    for class in &module.classes {
        for m in class.methods.iter().chain(class.class_methods.iter()) {
            for stmt in &m.body {
                collect_receiver_spans(&stmt.expression, &mut recvs);
            }
        }
    }

    let before = counts.in_map;
    for (_selector, span) in &recvs {
        counts.tally(type_map.get(*span));
    }
    let missed = recvs.len() - (counts.in_map - before);
    if missed > 0 {
        println!(
            "  MISS {path}: {missed} of {} receivers absent from TypeMap",
            recvs.len()
        );
    }
    counts.entries += type_map.len();
    counts.receivers += recvs.len();
    recvs.len()
}

#[test]
#[ignore = "BT-3216 spike instrumentation: reports numbers, asserts nothing"]
fn probe() {
    let mut files: Vec<std::path::PathBuf> = std::fs::read_dir("../../stdlib/src")
        .expect("stdlib/src")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|x| x == "bt"))
        .collect();
    files.sort();

    let mut counts = Counts::default();
    let mut worst: (usize, String) = (0, String::new());
    for path in &files {
        let path = path.display().to_string();
        let sends = scan_file(&path, &mut counts);
        if sends > worst.0 {
            worst = (sends, path);
        }
    }

    let total = counts.receivers;
    println!("--- BT-3216 TypeMap receiver-span coverage over stdlib/src ---");
    println!("files                  : {}", files.len());
    println!("total TypeMap entries  : {}", counts.entries);
    println!("send receivers         : {total}");
    println!(
        "receivers in TypeMap   : {} ({:.1}%)",
        counts.in_map,
        pct(counts.in_map, total)
    );
    println!(
        "  Known{{..}}             : {} ({:.1}%)  <- would key recv_type",
        counts.known,
        pct(counts.known, total)
    );
    println!(
        "  Meta{{C}} (class object) : {} ({:.1}%)  <- ADR write path has no rule for this",
        counts.meta,
        pct(counts.meta, total)
    );
    println!(
        "  Union                  : {} ({:.1}%)",
        counts.union,
        pct(counts.union, total)
    );
    println!(
        "  Dynamic(_)             : {} ({:.1}%)",
        counts.dynamic,
        pct(counts.dynamic, total)
    );
    println!(
        "  Never/Negation/Inter.  : {} ({:.1}%)",
        counts.other,
        pct(counts.other, total)
    );
    println!(
        "largest file by sends  : {} ({} receivers)",
        worst.1, worst.0
    );
}

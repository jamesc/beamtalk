// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Criterion benchmarks for the Beamtalk compiler pipeline.
//!
//! Measures each phase independently (lex, parse, analyse, codegen) and
//! end-to-end compilation for small, medium, and large inputs.
//!
//! Run with: `just bench` or `cargo bench -p beamtalk-core`

use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};

use beamtalk_core::codegen::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::semantic_analysis::analyse;
use beamtalk_core::source_analysis::{lex_with_eof, parse};

// ---------------------------------------------------------------------------
// Benchmark inputs — inline source to avoid filesystem I/O in the hot path
// ---------------------------------------------------------------------------

/// Minimal: a single Value class with one method (≈10 lines).
const SOURCE_MINIMAL: &str = r"
Value subclass: Point
  state: x :: Integer = 0
  state: y :: Integer = 0

  x => ^self.x
  y => ^self.y

  distanceTo: other =>
    dx := other x - self.x
    dy := other y - self.y
    ^(dx * dx + dy * dy)
";

/// Typical: an Actor with state, control flow, and string operations (≈30 lines).
const SOURCE_TYPICAL: &str = r#"
Actor subclass: Account
  state: balance = 0
  state: owner = ""

  deposit: amount =>
    amount < 0 ifTrue: [self error: "Amount must be non-negative"]
    self.balance := self.balance + amount
    ^self.balance

  withdraw: amount =>
    amount < 0 ifTrue: [self error: "Amount must be non-negative"]
    self.balance < amount
      ifTrue: [
        self
          error: "Insufficient funds (balance: "
            ++ self.balance printString
            ++ ", requested: "
            ++ amount printString
            ++ ")"
      ]
    self.balance := self.balance - amount
    ^self.balance

  balance => ^self.balance

  setOwner: name => self.owner := name

  getOwner => ^self.owner
"#;

/// Large: multiple classes with inheritance, closures, pattern matching (≈80 lines).
const SOURCE_LARGE: &str = r#"
Value subclass: Shape
  area => ^0

Value subclass: Circle
  superclass: Shape
  state: radius :: Integer = 0

  radius => ^self.radius

  area => ^self.radius * self.radius * 3

  circumference => ^2 * 3 * self.radius

  scale: factor =>
    ^Circle new radius: self.radius * factor

Actor subclass: ShapeCollection
  state: shapes = #()
  state: name = "default"

  add: shape =>
    self.shapes := self.shapes ++ #(shape)

  totalArea =>
    total := 0
    self.shapes do: [:each |
      total := total + each area
    ]
    ^total

  count => ^self.shapes size

  largestArea =>
    max := 0
    self.shapes do: [:each |
      each area > max ifTrue: [max := each area]
    ]
    ^max

  describe =>
    ^"Collection '" ++ self.name ++ "' with " ++ self.count printString ++ " shapes"

  setName: newName => self.name := newName

  getName => ^self.name

  filterByMinArea: minArea =>
    result := #()
    self.shapes do: [:each |
      each area >= minArea ifTrue: [
        result := result ++ #(each)
      ]
    ]
    ^result
"#;

/// Multi-file: simulates project compilation by concatenating multiple class
/// definitions (representative of a real project with cross-references).
const SOURCE_MULTI_FILE: &str = r#"
Value subclass: Money
  state: amount :: Integer = 0
  state: currency = "USD"

  amount => ^self.amount
  currency => ^self.currency

  add: other =>
    self.currency = other currency
      ifFalse: [self error: "Currency mismatch"]
    ^Money new amount: self.amount + other amount currency: self.currency

  printString => ^self.amount printString ++ " " ++ self.currency

Actor subclass: Wallet
  state: balances = #()
  state: owner = ""

  setOwner: name => self.owner := name
  owner => ^self.owner

  deposit: money =>
    self.balances := self.balances ++ #(money)

  totalFor: curr =>
    sum := 0
    self.balances do: [:each |
      each currency = curr ifTrue: [sum := sum + each amount]
    ]
    ^sum

  count => ^self.balances size

Actor subclass: Exchange
  state: rates = #()
  state: name = "Default Exchange"

  setRate: rate from: fromCurr to: toCurr =>
    self.rates := self.rates ++ #(#(fromCurr, toCurr, rate))

  convert: money to: targetCurr =>
    money currency = targetCurr ifTrue: [^money]
    self.rates do: [:entry |
      src := entry at: 1
      dst := entry at: 2
      rate := entry at: 3
      src = money currency ifTrue: [
        dst = targetCurr ifTrue: [
          ^Money new amount: money amount * rate currency: targetCurr
        ]
      ]
    ]
    self error: "No rate found"

  getName => ^self.name
  setName: n => self.name := n
"#;

// ---------------------------------------------------------------------------
// Benchmark groups
// ---------------------------------------------------------------------------

fn bench_lex(c: &mut Criterion) {
    let mut group = c.benchmark_group("lex");

    for (name, source) in [
        ("minimal", SOURCE_MINIMAL),
        ("typical", SOURCE_TYPICAL),
        ("large", SOURCE_LARGE),
        ("multi_file", SOURCE_MULTI_FILE),
    ] {
        group.bench_with_input(BenchmarkId::new("lex", name), &source, |b, src| {
            b.iter(|| black_box(lex_with_eof(src)));
        });
    }

    group.finish();
}

fn bench_parse(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse");

    for (name, source) in [
        ("minimal", SOURCE_MINIMAL),
        ("typical", SOURCE_TYPICAL),
        ("large", SOURCE_LARGE),
        ("multi_file", SOURCE_MULTI_FILE),
    ] {
        // Pre-lex so we only measure parsing.
        let tokens = lex_with_eof(source);
        group.bench_with_input(BenchmarkId::new("parse", name), &tokens, |b, toks| {
            b.iter(|| black_box(parse(toks.clone())));
        });
    }

    group.finish();
}

fn bench_analyse(c: &mut Criterion) {
    let mut group = c.benchmark_group("analyse");

    for (name, source) in [
        ("minimal", SOURCE_MINIMAL),
        ("typical", SOURCE_TYPICAL),
        ("large", SOURCE_LARGE),
        ("multi_file", SOURCE_MULTI_FILE),
    ] {
        // Pre-parse so we only measure semantic analysis (includes all validator phases).
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        group.bench_with_input(BenchmarkId::new("analyse", name), &module, |b, m| {
            b.iter(|| black_box(analyse(m)));
        });
    }

    group.finish();
}

fn bench_codegen(c: &mut Criterion) {
    let mut group = c.benchmark_group("codegen");

    for (name, source) in [
        ("minimal", SOURCE_MINIMAL),
        ("typical", SOURCE_TYPICAL),
        ("large", SOURCE_LARGE),
        ("multi_file", SOURCE_MULTI_FILE),
    ] {
        // Pre-parse and analyse so we only measure codegen.
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        // Analyse to ensure the module is valid (codegen may rely on analysis side-effects).
        let _ = analyse(&module);

        let module_name = format!("bench_{name}");
        group.bench_with_input(BenchmarkId::new("codegen", name), &module, |b, m| {
            b.iter(|| {
                let opts = CodegenOptions::new(&module_name).with_source(source);
                black_box(generate_module(m, opts))
            });
        });
    }

    group.finish();
}

fn bench_end_to_end(c: &mut Criterion) {
    let mut group = c.benchmark_group("end_to_end");

    for (name, source) in [
        ("minimal", SOURCE_MINIMAL),
        ("typical", SOURCE_TYPICAL),
        ("large", SOURCE_LARGE),
        ("multi_file", SOURCE_MULTI_FILE),
    ] {
        let module_name = format!("bench_{name}");
        group.bench_with_input(BenchmarkId::new("compile", name), &source, |b, src| {
            b.iter(|| {
                let tokens = lex_with_eof(src);
                let (module, _diags) = parse(tokens);
                let _analysis = analyse(&module);
                let opts = CodegenOptions::new(&module_name).with_source(src);
                black_box(generate_module(&module, opts))
            });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_lex,
    bench_parse,
    bench_analyse,
    bench_codegen,
    bench_end_to_end
);
criterion_main!(benches);

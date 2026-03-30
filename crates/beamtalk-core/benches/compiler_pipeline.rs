// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Criterion benchmarks for the Beamtalk compiler pipeline.
//!
//! Measures each phase independently (lex, parse, analyse, codegen) and
//! end-to-end compilation for small, medium, and large inputs.
//!
//! Run with: `just bench` or `cargo bench -p beamtalk-core`

use std::path::Path;

use std::hint::black_box;

use criterion::{BatchSize, BenchmarkId, Criterion, criterion_group, criterion_main};

use beamtalk_core::codegen::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::semantic_analysis::analyse;
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};

// ---------------------------------------------------------------------------
// Benchmark inputs — read from real fixtures to guarantee valid Beamtalk.
// File I/O happens once at setup, not in the hot path.
// ---------------------------------------------------------------------------

/// Load a fixture file relative to the workspace root.
fn load_fixture(relative_path: &str) -> String {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let path = workspace.join(relative_path);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read fixture {}: {e}", path.display()))
}

/// Assert that source parses without errors.
/// Called once per input during benchmark setup (not in the hot path).
fn assert_no_parse_errors(name: &str, source: &str) {
    let tokens = lex_with_eof(source);
    let (_module, parse_diags) = parse(tokens);
    let errors: Vec<_> = parse_diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(errors.is_empty(), "Parse errors in {name}: {errors:?}");
}

/// Assert that source parses and analyses without errors.
/// Called once per input during benchmark setup (not in the hot path).
fn assert_no_errors(name: &str, source: &str) {
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    let errors: Vec<_> = parse_diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(errors.is_empty(), "Parse errors in {name}: {errors:?}");

    let analysis = analyse(&module);
    let errors: Vec<_> = analysis
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(errors.is_empty(), "Analysis errors in {name}: {errors:?}");
}

struct BenchInput {
    name: &'static str,
    module_name: &'static str,
    fixture: &'static str,
    /// If false, this input has analysis errors and should only be used for
    /// lex/parse benchmarks (not analyse/codegen/end-to-end).
    analysable: bool,
}

const INPUTS: &[BenchInput] = &[
    // Minimal: simple Value class with accessors (≈15 lines)
    BenchInput {
        name: "minimal",
        module_name: "bench_minimal",
        fixture: "tests/e2e/fixtures/point.bt",
        analysable: true,
    },
    // Typical: Actor with state, control flow, error handling (≈35 lines)
    BenchInput {
        name: "typical",
        module_name: "bench_typical",
        fixture: "tests/e2e/fixtures/bank_account.bt",
        analysable: true,
    },
    // Large: SICP Scheme evaluator — complex pattern matching and recursion (≈238 lines)
    BenchInput {
        name: "large",
        module_name: "bench_large",
        fixture: "examples/sicp/src/scheme/eval.bt",
        analysable: true,
    },
    // XLarge: String stdlib — many methods, string interpolation, 574 lines.
    // Has analysis errors (sealed class) but lexes and parses cleanly.
    BenchInput {
        name: "xlarge",
        module_name: "bench_xlarge",
        fixture: "stdlib/src/String.bt",
        analysable: false,
    },
];

// ---------------------------------------------------------------------------
// Benchmark groups
// ---------------------------------------------------------------------------

fn bench_lex(c: &mut Criterion) {
    let mut group = c.benchmark_group("lex");

    for input in INPUTS {
        let source = load_fixture(input.fixture);
        group.bench_with_input(BenchmarkId::new("lex", input.name), &source, |b, src| {
            b.iter(|| black_box(lex_with_eof(src)));
        });
    }

    group.finish();
}

fn bench_parse(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse");

    for input in INPUTS {
        let source = load_fixture(input.fixture);
        assert_no_parse_errors(input.name, &source);
        let tokens = lex_with_eof(&source);
        group.bench_with_input(BenchmarkId::new("parse", input.name), &tokens, |b, toks| {
            // Use iter_batched so the token clone happens in setup, not the timed path.
            b.iter_batched(
                || toks.clone(),
                |t| black_box(parse(t)),
                BatchSize::SmallInput,
            );
        });
    }

    group.finish();
}

fn bench_analyse(c: &mut Criterion) {
    let mut group = c.benchmark_group("analyse");

    for input in INPUTS.iter().filter(|i| i.analysable) {
        let source = load_fixture(input.fixture);
        assert_no_errors(input.name, &source);
        let tokens = lex_with_eof(&source);
        let (module, _) = parse(tokens);
        group.bench_with_input(BenchmarkId::new("analyse", input.name), &module, |b, m| {
            b.iter(|| black_box(analyse(m)));
        });
    }

    group.finish();
}

fn bench_codegen(c: &mut Criterion) {
    let mut group = c.benchmark_group("codegen");

    for input in INPUTS.iter().filter(|i| i.analysable) {
        let source = load_fixture(input.fixture);
        assert_no_errors(input.name, &source);
        let tokens = lex_with_eof(&source);
        let (module, _) = parse(tokens);

        group.bench_with_input(BenchmarkId::new("codegen", input.name), &module, |b, m| {
            b.iter(|| {
                let opts = CodegenOptions::new(input.module_name).with_source(&source);
                black_box(generate_module(m, opts).expect("codegen must succeed"))
            });
        });
    }

    group.finish();
}

fn bench_end_to_end(c: &mut Criterion) {
    let mut group = c.benchmark_group("end_to_end");

    for input in INPUTS.iter().filter(|i| i.analysable) {
        let source = load_fixture(input.fixture);
        assert_no_errors(input.name, &source);
        group.bench_with_input(
            BenchmarkId::new("compile", input.name),
            &source,
            |b, src| {
                b.iter(|| {
                    let tokens = lex_with_eof(src);
                    let (module, _diags) = parse(tokens);
                    let _analysis = analyse(&module);
                    let opts = CodegenOptions::new(input.module_name).with_source(src);
                    black_box(generate_module(&module, opts).expect("compile must succeed"))
                });
            },
        );
    }

    group.finish();
}

/// Sequential multi-file benchmark: compile the SICP Scheme interpreter files
/// (7 files, ≈611 lines) independently in sequence. Does not exercise
/// cross-file resolution — measures per-file compilation cost at project scale.
fn bench_project(c: &mut Criterion) {
    let fixtures = [
        ("bench_main", "examples/sicp/src/main.bt"),
        ("bench_env", "examples/sicp/src/scheme/env.bt"),
        ("bench_eval", "examples/sicp/src/scheme/eval.bt"),
        ("bench_lambda", "examples/sicp/src/scheme/lambda.bt"),
        ("bench_printer", "examples/sicp/src/scheme/printer.bt"),
        ("bench_reader", "examples/sicp/src/scheme/reader.bt"),
        ("bench_symbol", "examples/sicp/src/scheme/symbol.bt"),
    ];
    let sources: Vec<_> = fixtures
        .iter()
        .map(|(name, path)| {
            let source = load_fixture(path);
            assert_no_errors(name, &source);
            (*name, source)
        })
        .collect();

    c.bench_function("sequential/sicp_7_files", |b| {
        b.iter(|| {
            for (module_name, source) in &sources {
                let tokens = lex_with_eof(source);
                let (module, _) = parse(tokens);
                let _analysis = analyse(&module);
                let opts = CodegenOptions::new(module_name).with_source(source);
                black_box(generate_module(&module, opts).unwrap());
            }
        });
    });
}

/// Sequential multi-file benchmark: compile the OTP supervision tree files
/// (4 Actor files, ≈71 lines) independently in sequence. Measures Actor-heavy
/// per-file compilation cost without cross-file resolution.
fn bench_project_otp(c: &mut Criterion) {
    let fixtures = [
        (
            "bench_app_supervisor",
            "examples/otp-tree/src/app_supervisor.bt",
        ),
        (
            "bench_event_logger",
            "examples/otp-tree/src/event_logger.bt",
        ),
        ("bench_task_worker", "examples/otp-tree/src/task_worker.bt"),
        ("bench_worker_pool", "examples/otp-tree/src/worker_pool.bt"),
    ];
    let sources: Vec<_> = fixtures
        .iter()
        .map(|(name, path)| {
            let source = load_fixture(path);
            assert_no_errors(name, &source);
            (*name, source)
        })
        .collect();

    c.bench_function("sequential/otp_tree_4_files", |b| {
        b.iter(|| {
            for (module_name, source) in &sources {
                let tokens = lex_with_eof(source);
                let (module, _) = parse(tokens);
                let _analysis = analyse(&module);
                let opts = CodegenOptions::new(module_name).with_source(source);
                black_box(generate_module(&module, opts).unwrap());
            }
        });
    });
}

/// Token allocation overhead investigation (BT-1680).
///
/// Measures `Vec<Token>::clone()` cost in isolation vs parse cost, to determine
/// whether token allocation is a meaningful overhead. Clone is required because
/// `parse()` takes `Vec<Token>` by value, so benchmarking requires a fresh copy
/// each iteration.
///
/// ## Findings (BT-1680)
///
/// **Token clone cost is significant in benchmarks but not in production.**
/// `Vec<Token>::clone()` costs ~61 ns/token (880 tokens: ~54 µs). In
/// production, `lex_with_eof` creates the vec once and passes it by value to
/// `parse()` — no clone occurs. The clone is only needed for benchmarking
/// (`iter_batched` setup).
///
/// **Internal `advance()` clones were the real overhead.** The parser called
/// `.clone()` on every token it advanced past, even though it owns the vec.
/// Switching to `Token::take_kind()` (moves the kind out, leaves span/trivia
/// intact for post-advance lookups) gave a consistent ~18-22% parse speedup:
///
/// | Input | Tokens | Parse (clone) | Parse (take) | Improvement |
/// |-------|--------|---------------|--------------|-------------|
/// | minimal | 22 | 3.6 µs | 2.96 µs | -18% |
/// | typical | 105 | 13.6 µs | 10.5 µs | -22% |
/// | large | 880 | 118.9 µs | 94.0 µs | -21% |
/// | xlarge | 526 | 145.9 µs | 120.6 µs | -17% |
///
/// **Conclusion:** The `take_kind` optimization is worth keeping. Arena
/// allocation or slice-based parsing would yield diminishing returns since
/// `EcoString` (used in `TokenKind` and `Trivia`) is already reference-counted.
fn bench_token_clone_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("token_clone_overhead");

    // Use all INPUTS including xlarge — we only need lex+parse, no analysis.
    for input in INPUTS {
        let source = load_fixture(input.fixture);
        let tokens = lex_with_eof(&source);

        // Report token count for context (visible in benchmark output)
        let token_count = tokens.len();
        let source_lines = source.lines().count();
        eprintln!(
            "[BT-1680] {}: {} lines, {} tokens, {} bytes/token (approx)",
            input.name,
            source_lines,
            token_count,
            std::mem::size_of::<beamtalk_core::source_analysis::Token>(),
        );

        // Measure just the clone cost
        group.bench_with_input(
            BenchmarkId::new("clone_only", input.name),
            &tokens,
            |b, toks| {
                b.iter(|| black_box(toks.clone()));
            },
        );

        // Measure parse (including the clone in setup via iter_batched)
        group.bench_with_input(
            BenchmarkId::new("parse_only", input.name),
            &tokens,
            |b, toks| {
                b.iter_batched(
                    || toks.clone(),
                    |t| black_box(parse(t)),
                    BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_lex,
    bench_parse,
    bench_analyse,
    bench_codegen,
    bench_end_to_end,
    bench_project,
    bench_project_otp,
    bench_token_clone_overhead
);
criterion_main!(benches);

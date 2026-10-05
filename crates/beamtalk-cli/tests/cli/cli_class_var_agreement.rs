// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0130 Phase 1 (BT-3705): the class-variable agreement property, run on
//! a BEAM.
//!
//! `beamtalk-core`'s `test_helpers::class_var_program` generates class-method
//! programs over two class variables (loops, `on:do:`/`ensure:`/`tryDo:`,
//! stored closures, writing and late-bound self-sends) and interprets them per
//! ADR 0130 §4. This module compiles every program in three spellings (open,
//! `sealed`, base class + overriding subclass) through the real `beamtalk
//! test` pipeline (debug codegen with the `ThreadedIr` verifier on, `erlc`,
//! execution) and asserts each spelling answers what the reference
//! interpretation says, so the three spellings agree with each other.
//!
//! Two properties share one harness:
//!
//! - `class_var_agreement_supported_shapes` draws only the shapes whose
//!   programs all pass on the current compiler. It is a normal test: it guards
//!   the shapes ADR 0130 Phase 3 must not regress.
//! - `class_var_agreement_all_shapes` draws every shape. It is `#[ignore]`d
//!   (red on `main` today) and prints the failure rate and the failing
//!   shapes; the Phase 3 flip removes the `#[ignore]` (see `just
//!   test-class-var-corpus`).
//!
//! A batch is one package compiled and run once; if the package fails to
//! compile (a rejected program, or a `ThreadedIr` verifier `internal:` error
//! or debug panic) the batch is bisected to find the offending programs.

use crate::cli_common;

use beamtalk_core::test_helpers::class_var_program::{
    Package, Program, Shapes, Spelling, render_package,
};
use beamtalk_core::test_helpers::test_support::arb_class_program;
use proptest::strategy::{Strategy, ValueTree};
use proptest::test_runner::{Config, RngAlgorithm, TestRng, TestRunner};
use std::fmt::Write as _;
use std::path::Path;

/// Programs drawn per property run; override with `CV_CORPUS_CASES`.
const DEFAULT_CASES: usize = 48;

/// Failing programs whose full source is printed in a report.
const MAX_SOURCES_PRINTED: usize = 3;

/// One generated program with the pair that reproduces it.
struct Case {
    index: usize,
    seed: u64,
    size: u32,
    program: Program,
}

fn cases() -> usize {
    std::env::var("CV_CORPUS_CASES")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(DEFAULT_CASES)
}

/// Draws `n` programs from the shared strategy with a fixed RNG, so the case
/// budget is the same corpus on every run (and every machine).
fn draw(shapes: Shapes, n: usize) -> Vec<Case> {
    let mut runner = TestRunner::new_with_rng(
        Config::default(),
        TestRng::deterministic_rng(RngAlgorithm::ChaCha),
    );
    let strategy = arb_class_program(shapes);
    (0..n)
        .map(|index| {
            let (seed, size, program) = strategy
                .new_tree(&mut runner)
                .expect("strategy draws a value")
                .current();
            Case {
                index,
                seed,
                size,
                program,
            }
        })
        .collect()
}

/// Why one program (or spelling) failed.
#[derive(Debug)]
enum Failure {
    /// The package did not compile with this program in it: a compile error,
    /// a debug-build `ThreadedIr` verifier panic, or an `internal:` diagnostic.
    Compile(String, String),
    /// The spelling compiled but answered something other than the reference
    /// interpretation.
    Wrong(Spelling, String),
}

struct Failed {
    index: usize,
    seed: u64,
    size: u32,
    failure: Failure,
}

fn write_package(root: &Path, pkg: &Package) {
    let fixtures = root.join("test/fixtures");
    std::fs::create_dir_all(&fixtures).expect("mkdir fixtures");
    std::fs::write(
        root.join("beamtalk.toml"),
        "# Copyright 2026 James Casey\n\
         # SPDX-License-Identifier: Apache-2.0\n\
         \n\
         [package]\n\
         name = \"class_var_agreement\"\n\
         version = \"0.1.0\"\n\
         \n\
         [dependencies]\n",
    )
    .expect("write beamtalk.toml");
    for (name, source) in &pkg.fixtures {
        std::fs::write(fixtures.join(name), source).expect("write fixture");
    }
    std::fs::write(root.join("test").join(&pkg.test.0), &pkg.test.1).expect("write test");
}

/// Outcome of running one package.
enum Batch {
    /// Every test passed.
    Pass,
    /// Compiled and ran; these test methods failed.
    Tests(Vec<String>, String),
    /// Did not compile (or crashed before running tests).
    Broken(String),
}

fn run_package(pkg: &Package) -> Batch {
    let dir = tempfile::tempdir().expect("tempdir");
    write_package(dir.path(), pkg);
    let out = cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["test"])
        .output()
        .expect("run beamtalk test");
    let text = format!(
        "{}\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    // An `internal:` diagnostic is a verifier failure even when the compile
    // went on to succeed (a release build degrades the verifier to one).
    // Today the CLI driver never prints it: `write_core_erlang_with_bindings`
    // calls `generate_module`, which drops the generator's warnings, so a
    // release `beamtalk build` ships unverified output silently (ADR 0130
    // Open Question 2). This check is what turns that into a failure once the
    // driver surfaces them; the in-process `beamtalk-codegen` property already
    // asserts it on `generate_module_with_warnings`.
    if text.contains("internal:") {
        return Batch::Broken(text);
    }
    if out.status.success() {
        return Batch::Pass;
    }
    let failed: Vec<String> = pkg
        .test_names
        .iter()
        .filter(|(_, _, name)| failed_in_output(&text, name))
        .map(|(_, _, name)| name.clone())
        .collect();
    if text.contains("Running tests") && !failed.is_empty() {
        Batch::Tests(failed, text)
    } else {
        Batch::Broken(text)
    }
}

/// Whether `name` is reported as failing in `beamtalk test` output.
fn failed_in_output(text: &str, name: &str) -> bool {
    text.lines().any(|l| {
        l.contains(name) && (l.contains("FAIL") || l.contains('✗') || l.contains("failed"))
    })
}

/// The compiler's one-line reason from a failed compile (the line after
/// `Failed to generate Core Erlang`, or a debug-build verifier panic).
fn headline(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();
    // A debug-build panic (the `ThreadedIr` verifier's `debug_assert!`): the
    // message is the line after `panicked at <location>:`.
    let panic_message = lines
        .iter()
        .position(|l| l.contains("panicked at"))
        .and_then(|i| lines.get(i + 1))
        .copied();
    let line = text
        .lines()
        .find(|l| l.contains("╰─▶") && !l.contains("Failed to"))
        .or_else(|| text.lines().find(|l| l.contains("ThreadedIr verify")))
        .or(panic_message)
        .or_else(|| text.lines().find(|l| l.contains("internal:")))
        .unwrap_or("(no reason found)");
    // Drop the shape-specific names so equal causes group together.
    let line = line
        .trim_start_matches(|c: char| !c.is_alphanumeric())
        .trim();
    let mut out = String::new();
    let mut quoted = false;
    for ch in line.chars() {
        if ch == '\'' {
            quoted = !quoted;
            out.push(ch);
        } else if !quoted && !ch.is_ascii_digit() {
            out.push(ch);
        }
    }
    out.chars().take(70).collect()
}

fn tail(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();
    let start = lines.len().saturating_sub(40);
    lines[start..].join("\n")
}

/// Runs `cases` and returns every failure, bisecting broken batches.
fn check(cases: &[&Case], failed: &mut Vec<Failed>) {
    if cases.is_empty() {
        return;
    }
    let programs: Vec<(usize, Program)> =
        cases.iter().map(|c| (c.index, c.program.clone())).collect();
    let pkg = render_package(&programs);
    let record = |c: &Case, failure: Failure, failed: &mut Vec<Failed>| {
        failed.push(Failed {
            index: c.index,
            seed: c.seed,
            size: c.size,
            failure,
        });
    };
    match run_package(&pkg) {
        Batch::Pass => {}
        Batch::Tests(names, text) => {
            for (index, spelling, name) in &pkg.test_names {
                if names.contains(name) {
                    let c = cases.iter().find(|c| c.index == *index).expect("case");
                    let detail = text
                        .lines()
                        .filter(|l| l.contains("FAIL") && l.contains(name.as_str()))
                        .collect::<Vec<_>>()
                        .join("; ");
                    record(c, Failure::Wrong(*spelling, detail), failed);
                }
            }
        }
        Batch::Broken(text) => {
            // The compiler names the fixture it stopped at: blame that
            // program, drop it, and run the rest (a compile-only failure is
            // quick, so this is far cheaper than bisecting).
            let offender = pkg
                .fixtures
                .iter()
                .zip(&pkg.fixture_owner)
                .find(|((file, _), _)| {
                    text.lines().any(|l| {
                        l.contains("Failed to compile fixture") && l.contains(file.as_str())
                    })
                })
                .map(|(_, owner)| *owner);
            if let Some(owner) = offender {
                let c = cases.iter().find(|c| c.index == owner).expect("case");
                record(c, Failure::Compile(headline(&text), tail(&text)), failed);
                let rest: Vec<&Case> = cases.iter().copied().filter(|c| c.index != owner).collect();
                check(&rest, failed);
            } else if let [only] = cases {
                record(only, Failure::Compile(headline(&text), tail(&text)), failed);
            } else {
                let (a, b) = cases.split_at(cases.len() / 2);
                check(a, failed);
                check(b, failed);
            }
        }
    }
}

fn report(shapes: Shapes, all: &[Case], failed: &[Failed]) -> String {
    let mut s = String::new();
    let mut by_program: Vec<usize> = failed.iter().map(|f| f.index).collect();
    by_program.sort_unstable();
    by_program.dedup();
    let _ = writeln!(
        s,
        "class-variable agreement: {} of {} programs failed ({}%), shapes {:?}",
        by_program.len(),
        all.len(),
        100 * by_program.len() / all.len().max(1),
        shapes
    );
    // Group the failures by cause: the compiler's one-line reason, or the
    // wrong-answer spelling.
    let mut causes: Vec<(String, usize)> = Vec::new();
    for f in failed {
        let cause = match &f.failure {
            Failure::Compile(h, _) => format!("did not compile: {h}"),
            Failure::Wrong(sp, _) => format!("wrong answer ({sp:?})"),
        };
        match causes.iter_mut().find(|(c, _)| *c == cause) {
            Some((_, n)) => *n += 1,
            None => causes.push((cause, 1)),
        }
    }
    causes.sort_by(|a, b| b.1.cmp(&a.1));
    for (cause, n) in &causes {
        let _ = writeln!(s, "  {n:>3} x {cause}");
    }
    for (i, f) in failed.iter().enumerate() {
        let _ = writeln!(
            s,
            "--- program {} (seed {}, size {}): {}",
            f.index,
            f.seed,
            f.size,
            match &f.failure {
                Failure::Compile(_, t) => format!("did not compile\n{t}"),
                Failure::Wrong(sp, d) => format!("wrong answer in the {sp:?} spelling: {d}"),
            }
        );
        // Full sources only for the first few; the rest are reproducible from
        // the printed (seed, size) pair.
        if i < MAX_SOURCES_PRINTED {
            if let Some(c) = all.iter().find(|c| c.index == f.index) {
                for class in c.program.render(c.index, Spelling::Open) {
                    let _ = writeln!(s, "{}", class.source);
                }
                let _ = writeln!(s, "expected {:?}", c.program.interpret());
            }
        }
    }
    s
}

fn property(shapes: Shapes) {
    let all = draw(shapes, cases());
    let refs: Vec<&Case> = all.iter().collect();
    let mut failed = Vec::new();
    check(&refs, &mut failed);
    let text = report(shapes, &all, &failed);
    eprintln!("{text}");
    assert!(failed.is_empty(), "{text}");
}

#[test]
fn class_var_agreement_supported_shapes() {
    property(Shapes::SUPPORTED_TODAY);
}

#[test]
#[ignore = "red on main until ADR 0130 Phase 3 (BT-3713) flips it; run via `just test-class-var-corpus`"]
fn class_var_agreement_all_shapes() {
    // `CV_CORPUS_SHAPES=do,cond,...` narrows the draw to find which shape a
    // failure belongs to (names: `Shapes::names`); default is every shape.
    let shapes = std::env::var("CV_CORPUS_SHAPES")
        .ok()
        .map_or(Shapes::all(), |s| {
            Shapes::parse(&s).unwrap_or_else(|| panic!("unknown shape in CV_CORPUS_SHAPES={s}"))
        });
    property(shapes);
}

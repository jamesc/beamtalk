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
//! - `class_var_agreement_enabled_shapes` draws [`Shapes::ENABLED`], every shape
//!   except `local_touch`. It is a normal test (ADR 0130 Phase 3, BT-3713,
//!   enabled it); `just test-class-var-corpus` runs it with a larger draw.
//! - `class_var_agreement_local_touch` draws every shape including
//!   `local_touch` (an outer local mutated inside a protected block). It is
//!   `#[ignore]`d: those programs fail today for a local-variable threading
//!   reason, BT-3738, not fixed by ADR 0130.
//!
//! A batch is one package compiled and run once; if the package fails to
//! compile (a rejected program, or a `ThreadedIr` verifier `internal:` error
//! or debug panic) the batch is bisected to find the offending programs.
//!
//! # Environment failures (BT-3767)
//!
//! Before compiling anything, [`preflight`] asserts the runtime exports
//! `beamtalk_class_vars:snapshot/0` and the stdlib `TestCase` class loads, so
//! a missing or stale `runtime/` build fails with one line naming
//! `just build-stdlib` (a plain `cargo test` does not build the stdlib). A
//! batch in which every test failed and the output still looks like an
//! environment failure (`Undefined function`, `setUp failed`, `abi_mismatch`,
//! or every program and spelling of at least [`MIN_PROGRAMS_SAME_MESSAGE`]
//! programs failing with the same message) is reported as probably a broken
//! environment, not as N wrong answers, and is not bisected. A marker in only
//! some tests is a program's failure (the preflight already passed), reported
//! and bisected like any other. A real codegen bug can still look like an
//! environment failure, so that report carries every `(index, seed, size)`
//! pair in the batch and the marker's (or first failing) program's detail and
//! source.
//!
//! # Budget (BT-3767)
//!
//! A plain `cargo test` (every `just test-rust`, on three OSes per PR) draws
//! [`DEFAULT_CASES`] (16) programs. `just test-class-var-corpus` runs the
//! full 48 and the nightly `class-var-corpus` job (`fuzz.yml`) 192;
//! `CV_CORPUS_CASES` sets the draw. Measured on a Linux dev container (debug
//! build, warm runtime): 16 programs took about 19 s, 48 about 37 s.

use crate::cli_common;

use beamtalk_cli::repl_startup;
use beamtalk_core::test_helpers::class_var_program::{
    Package, Program, Shapes, Spelling, TEST_CLASS, corpus_cases_from_env, corpus_shapes_from_env,
    normalize_cause, render_package,
};
use beamtalk_core::test_helpers::test_support::draw_class_programs;
use std::fmt::Write as _;
use std::path::Path;
use std::sync::OnceLock;

/// Programs drawn per property run in a plain `cargo test`; override with
/// `CV_CORPUS_CASES` (see the module doc's budget section).
const DEFAULT_CASES: usize = 16;

/// Output fragments that mean the environment (a stale or missing
/// `runtime/` / stdlib build), not the generated program, is at fault, when
/// every test in the batch failed (see [`environment_cause`]).
const ENVIRONMENT_MARKERS: [(&str, &str); 3] = [
    (
        "Undefined function",
        "a module or function the test needs is missing from the runtime or stdlib build",
    ),
    (
        "setUp failed",
        "TestCase setUp failed before any program ran",
    ),
    (
        "abi_mismatch",
        "the compiled code and the runtime disagree on the ABI",
    ),
];

/// Fewest distinct programs in a batch before "every program and spelling
/// failed with the same message" counts as an environment failure: with fewer,
/// one codegen bug hitting a narrowed draw (`CV_CORPUS_SHAPES`, a small
/// `CV_CORPUS_CASES`) looks the same.
const MIN_PROGRAMS_SAME_MESSAGE: usize = 4;

/// The one-line report for a failed [`preflight`]: certainly the environment.
fn preflight_headline(cause: &str) -> String {
    format!(
        "class-variable agreement: broken environment, not codegen: {cause}. \
         The runtime/stdlib under runtime/ is probably stale or unbuilt; run \
         `just build-stdlib` and retry."
    )
}

/// The one-line report for a batch that looks like an environment failure
/// after the preflight passed: probable, not certain.
fn environment_headline(cause: &str) -> String {
    format!(
        "class-variable agreement: probably a broken environment (or a codegen \
         bug that fails every program the same way): {cause}. The preflight \
         passed, but the runtime/stdlib under runtime/ may still be stale; run \
         `just build-stdlib` and retry. If it persists, reproduce from the \
         (index, seed, size) pairs and the first failing program below."
    )
}

/// Asserts, once per process, that the runtime the CLI will use has the
/// ADR 0130 class-variable API and the stdlib `TestCase` class, before any
/// program is compiled (BT-3767).
fn preflight() {
    static DONE: OnceLock<()> = OnceLock::new();
    DONE.get_or_init(|| {
        let paths = repl_startup::beam_paths(&cli_common::project_root().join("runtime"));
        let eval = "Check = fun(M, F, A) -> \
                        _ = code:ensure_loaded(M), \
                        case erlang:function_exported(M, F, A) of \
                            true -> ok; \
                            false -> io:format(\"PREFLIGHT_MISSING ~s:~s/~b~n\", [M, F, A]) \
                        end \
                    end, \
                    Check(beamtalk_class_vars, snapshot, 0), \
                    Check('bt@stdlib@test_case', new, 0), \
                    io:format(\"PREFLIGHT_DONE~n\"), \
                    halt().";
        let out = std::process::Command::new("erl")
            .arg("-noshell")
            .args(repl_startup::beam_pa_args(&paths))
            .args(["-eval", eval])
            .output()
            .unwrap_or_else(|e| panic!("{}", preflight_headline(&format!("cannot run erl: {e}"))));
        let stdout = String::from_utf8_lossy(&out.stdout);
        let missing: Vec<&str> = stdout
            .lines()
            .filter_map(|l| l.strip_prefix("PREFLIGHT_MISSING "))
            .collect();
        assert!(
            missing.is_empty() && stdout.contains("PREFLIGHT_DONE"),
            "{}\n{}{}",
            preflight_headline(&if missing.is_empty() {
                "preflight: erl did not finish the runtime check".to_string()
            } else {
                format!("preflight: the runtime lacks {}", missing.join(", "))
            }),
            stdout,
            String::from_utf8_lossy(&out.stderr)
        );
    });
}

/// Failing programs whose full source is printed in a report.
const MAX_SOURCES_PRINTED: usize = 3;

/// One generated program with the pair that reproduces it.
struct Case {
    index: usize,
    seed: u64,
    size: u32,
    program: Program,
}

/// The shared deterministic draw, tagged with each program's index.
fn draw(shapes: Shapes, n: usize) -> Vec<Case> {
    draw_class_programs(shapes, n)
        .into_iter()
        .enumerate()
        .map(|(index, (seed, size, program))| Case {
            index,
            seed,
            size,
            program,
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
    /// Did not compile (or crashed before running tests). `environment` is
    /// the headline when the output says the environment, not a program, is
    /// at fault; such a batch is reported as is, never bisected.
    Broken {
        text: String,
        environment: Option<String>,
    },
}

impl Batch {
    fn broken(text: String) -> Batch {
        Batch::Broken {
            text,
            environment: None,
        }
    }
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
    // went on to succeed (a release build degrades the verifier to a warning
    // that the CLI build path prints, ADR 0111 Addendum 17, BT-3724).
    if text.contains("internal:") {
        return Batch::broken(text);
    }
    if out.status.success() {
        return Batch::Pass;
    }
    if let Some(cause) = environment_cause(pkg, &text) {
        return Batch::Broken {
            text,
            environment: Some(environment_headline(&cause)),
        };
    }
    let failed: Vec<String> = pkg
        .test_names
        .iter()
        .filter(|(_, _, name)| failure_message(&text, name).is_some())
        .map(|(_, _, name)| name.clone())
        .collect();
    if text.contains("Running tests") && !failed.is_empty() {
        Batch::Tests(failed, text)
    } else {
        Batch::broken(text)
    }
}

/// Why `text` (a failed run of `pkg`) looks like a broken environment rather
/// than a wrong program, or `None` (BT-3767). Only a batch in which every test
/// failed qualifies: the preflight has already ruled out a stale runtime, so a
/// failure confined to some programs is theirs (reported and bisected like any
/// other), and one codegen bug never hides the rest of the batch. Such a batch
/// is an environment failure when its output contains one of
/// [`ENVIRONMENT_MARKERS`], or when it has at least
/// [`MIN_PROGRAMS_SAME_MESSAGE`] programs and every program and spelling
/// failed with the same message (generated programs expect different answers,
/// so identical messages across that many are unlikely to be one bug).
fn environment_cause(pkg: &Package, text: &str) -> Option<String> {
    let messages: Vec<&str> = pkg
        .test_names
        .iter()
        .map(|(_, _, name)| failure_message(text, name))
        .collect::<Option<_>>()?;
    if let Some((marker, cause)) = ENVIRONMENT_MARKERS.iter().find(|(m, _)| text.contains(m)) {
        let line = text.lines().find(|l| l.contains(marker)).unwrap_or(marker);
        return Some(format!("{cause} (`{}`)", line.trim()));
    }
    let programs = {
        let mut ids: Vec<usize> = pkg.test_names.iter().map(|(i, _, _)| *i).collect();
        ids.sort_unstable();
        ids.dedup();
        ids.len()
    };
    match messages.as_slice() {
        [first, rest @ ..]
            if programs >= MIN_PROGRAMS_SAME_MESSAGE && rest.iter().all(|m| m == first) =>
        {
            Some(format!(
                "every program and spelling failed with the same message (`{first}`)"
            ))
        }
        _ => None,
    }
}

/// The test runner's own failure message for test method `name`: the rest of
/// its `FAIL <class> <method>: <message>` result line (see
/// `fail_detail_line` in `commands/test.rs`), or `None` when it did not fail.
fn failure_message<'a>(text: &'a str, name: &str) -> Option<&'a str> {
    let prefix = format!("FAIL {TEST_CLASS} {name}:");
    text.lines()
        .find_map(|l| l.trim_start().strip_prefix(prefix.as_str()))
        .map(str::trim)
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
    normalize_cause(line)
}

fn tail(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();
    let start = lines.len().saturating_sub(40);
    lines[start..].join("\n")
}

/// Appends `c`'s full source (open spelling) and expected answer.
fn write_source(s: &mut String, c: &Case) {
    for class in c.program.render(c.index, Spelling::Open) {
        let _ = writeln!(s, "{}", class.source);
    }
    let _ = writeln!(s, "expected {:?}", c.program.interpret());
}

/// The panic message for a batch classified as an environment failure: the
/// headline, every `(index, seed, size)` pair in the batch, then the
/// per-spelling results and source of the program whose result line carries an
/// [`ENVIRONMENT_MARKERS`] entry (else the first failing program, else the
/// first program), then the output tail, so a wrong guess is never a dead end.
fn environment_report(headline: &str, text: &str, pkg: &Package, cases: &[&Case]) -> String {
    let mut s = format!("{headline}\n\n");
    let pairs: Vec<String> = cases
        .iter()
        .map(|c| format!("({}, {}, {})", c.index, c.seed, c.size))
        .collect();
    let _ = writeln!(
        s,
        "batch of {} programs, (index, seed, size): {}",
        cases.len(),
        pairs.join(" ")
    );
    let failing_where = |pred: &dyn Fn(&str) -> bool| {
        pkg.test_names
            .iter()
            .find(|(_, _, name)| failure_message(text, name).is_some_and(pred))
            .map(|(index, _, _)| *index)
    };
    let marked = failing_where(&|m| ENVIRONMENT_MARKERS.iter().any(|(k, _)| m.contains(k)));
    let (chosen, label) = match marked {
        Some(i) => (Some(i), "environment-marker"),
        None => (failing_where(&|_| true), "first failing"),
    };
    let first = chosen
        .and_then(|i| cases.iter().find(|c| c.index == i))
        .or_else(|| cases.first());
    if let Some(c) = first {
        let _ = writeln!(
            s,
            "--- {} program {} (seed {}, size {})",
            if chosen.is_some() {
                label
            } else {
                "no test result line; first"
            },
            c.index,
            c.seed,
            c.size
        );
        for (index, spelling, name) in &pkg.test_names {
            if *index == c.index {
                let _ = writeln!(
                    s,
                    "  {spelling:?}: {}",
                    failure_message(text, name).unwrap_or("(no FAIL line)")
                );
            }
        }
        write_source(&mut s, c);
    }
    let _ = write!(s, "--- output tail\n{}", tail(text));
    s
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
                    let detail = failure_message(&text, name).unwrap_or_default().to_string();
                    record(c, Failure::Wrong(*spelling, detail), failed);
                }
            }
        }
        // Not (probably) a program's fault: no bisection; the report leads
        // with the cause, then every repro pair and the first failing program.
        Batch::Broken {
            text,
            environment: Some(headline),
        } => panic!("{}", environment_report(&headline, &text, &pkg, cases)),
        Batch::Broken {
            text,
            environment: None,
        } => {
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
                let before = failed.len();
                check(a, failed);
                check(b, failed);
                // The batch was broken but every half passed (an interaction
                // between programs, or a flaky run): never let that pass
                // silently; blame the batch's first case.
                if failed.len() == before {
                    record(
                        cases[0],
                        Failure::Compile(
                            format!(
                                "batch of {} broke but each half passed: {}",
                                cases.len(),
                                headline(&text)
                            ),
                            tail(&text),
                        ),
                        failed,
                    );
                }
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
                write_source(&mut s, c);
            }
        }
    }
    s
}

fn property(shapes: Shapes) {
    preflight();
    let all = draw(shapes, corpus_cases_from_env(DEFAULT_CASES));
    let refs: Vec<&Case> = all.iter().collect();
    let mut failed = Vec::new();
    check(&refs, &mut failed);
    let text = report(shapes, &all, &failed);
    eprintln!("{text}");
    assert!(failed.is_empty(), "{text}");
}

#[test]
fn class_var_agreement_enabled_shapes() {
    // `CV_CORPUS_SHAPES=do,cond,...` narrows the draw to find which shape a
    // failure belongs to (names: `Shapes::names`); default is `Shapes::ENABLED`.
    property(corpus_shapes_from_env(Shapes::ENABLED));
}

/// BT-3738: `local_touch` (an outer local mutated inside a `on:do:`,
/// `ensure:` or `Result tryDo:` block) fails today. Remove the `#[ignore]` (and
/// add `LOCAL_TOUCH` to `Shapes::ENABLED`) when BT-3738 is fixed.
#[test]
#[ignore = "local_touch fails today (BT-3738); run via `just test-class-var-corpus-local-touch`"]
fn class_var_agreement_local_touch() {
    property(Shapes::all());
}

/// BT-3767: the harness reads failures off the test runner's own
/// `FAIL <class> <method>: <message>` result lines, and classifies an
/// environment failure as a broken batch, not as wrong answers. No BEAM.
#[test]
fn class_var_agreement_classifies_runner_output() {
    use beamtalk_core::test_helpers::class_var_program::gen_program;
    let cases: Vec<Case> = (0..MIN_PROGRAMS_SAME_MESSAGE)
        .map(|index| Case {
            index,
            seed: 1000 + index as u64,
            size: 1,
            program: gen_program(1000 + index as u64, 1, Shapes::ENABLED),
        })
        .collect();
    let package_of = |cases: &[Case]| {
        let programs: Vec<(usize, Program)> =
            cases.iter().map(|c| (c.index, c.program.clone())).collect();
        render_package(&programs)
    };
    let pkg = package_of(&cases);
    let names: Vec<&str> = pkg.test_names.iter().map(|(_, _, n)| n.as_str()).collect();
    let line = |name: &str, msg: &str| format!("  FAIL {TEST_CLASS} {name}: {msg}\n");

    // One wrong answer: found by its result line, not an environment failure.
    let one = format!("Running tests...\n{}", line(names[0], "expected 3, got 4"));
    assert_eq!(failure_message(&one, names[0]), Some("expected 3, got 4"));
    assert_eq!(failure_message(&one, names[1]), None);
    assert_eq!(environment_cause(&pkg, &one), None);

    // Free text naming a test is not a result line.
    let prose = format!("note: {} failed to FAIL", names[0]);
    assert_eq!(failure_message(&prose, names[0]), None);

    // Every program and spelling failing with the same message, across
    // `MIN_PROGRAMS_SAME_MESSAGE` programs.
    let same: String = names.iter().map(|n| line(n, "boom")).collect();
    assert!(environment_cause(&pkg, &same).is_some_and(|c| c.contains("same message")));
    // ... but below the threshold it may be one codegen bug: not classified.
    let small = package_of(&cases[..MIN_PROGRAMS_SAME_MESSAGE - 1]);
    let small_same: String = small
        .test_names
        .iter()
        .map(|(_, _, n)| line(n, "boom"))
        .collect();
    assert_eq!(environment_cause(&small, &small_same), None);
    // ... but different messages are wrong answers.
    let differ: String = names
        .iter()
        .enumerate()
        .map(|(i, n)| line(n, &format!("expected {i}")))
        .collect();
    assert_eq!(environment_cause(&pkg, &differ), None);

    // A marker in one program of a mixed batch is that program's failure
    // (the preflight passed): not an environment failure, so it is reported
    // and bisected like any other.
    let one_marker = line(names[0], "Undefined function: beamtalk_x:y/0");
    assert_eq!(environment_cause(&pkg, &one_marker), None);

    // A stale stdlib: the marker in every program is an environment failure.
    let stale: String = names
        .iter()
        .map(|n| {
            line(
                n,
                "setUp failed: Undefined function: bt@stdlib@test_case:new/0",
            )
        })
        .collect();
    let cause = environment_cause(&pkg, &stale).expect("environment");
    assert!(
        cause.contains("missing from the runtime or stdlib build"),
        "{cause}"
    );
    let headline = environment_headline(&cause);
    assert!(headline.contains("just build-stdlib"), "{headline}");
    assert!(headline.contains("preflight passed"), "{headline}");
    assert!(headline.contains("codegen bug"), "{headline}");

    // The environment report carries every repro pair and the first failing
    // program's detail, not only the tail.
    let refs: Vec<&Case> = cases.iter().collect();
    let report = environment_report(&headline, &stale, &pkg, &refs);
    for c in &cases {
        let pair = format!("({}, {}, {})", c.index, c.seed, c.size);
        assert!(report.contains(&pair), "missing {pair}:\n{report}");
    }
    assert!(
        report.contains("environment-marker program 0 (seed 1000, size 1)"),
        "{report}"
    );
    assert!(report.contains("expected "), "{report}");

    // The report names the program whose result line has the marker, not the
    // first FAIL line in test order.
    let marked = 2;
    let late_marker: String = pkg
        .test_names
        .iter()
        .map(|(i, _, n)| {
            if *i == marked {
                line(n, "abi_mismatch: expected 3, runtime 2")
            } else {
                line(n, "expected 1, got 2")
            }
        })
        .collect();
    let cause = environment_cause(&pkg, &late_marker).expect("environment");
    let report = environment_report(&environment_headline(&cause), &late_marker, &pkg, &refs);
    assert!(
        report.contains(&format!(
            "environment-marker program {marked} (seed {}, size 1)",
            1000 + marked
        )),
        "{report}"
    );
}

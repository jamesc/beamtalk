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
//! `just build-stdlib` (a plain `cargo test` does not build the stdlib).
//!
//! A batch of at least [`MIN_ENVIRONMENT_PROGRAMS`] distinct programs is
//! suspected of a broken environment when either no test produced a result
//! line (the runner crashed after load) and its output contains
//! `Undefined function`, `setUp failed` or `abi_mismatch`, or every test
//! failed and either every failure message carries the same one of those
//! markers or every program and spelling failed with the same message. A
//! suspicion is only a guess: before reporting it, the harness re-runs two
//! different programs of the batch (the first and the last) separately. A
//! broken runtime or stdlib fails every subset the same way, so only when
//! both re-runs show the same kind of environment failure is the batch
//! reported as probably a broken environment (not as N
//! wrong answers, and not bisected). Otherwise the guess is dropped and the
//! batch is reported and bisected like any other, so one program's codegen
//! bug (say a one-class ABI stamp bug hitting `abi_mismatch` at load) is
//! isolated rather than hiding the batch (one probe alone could be that very
//! program, failing the same way, hence two). The confirmation costs two extra
//! `beamtalk test` runs, paid only on the environment path. Anything else (a
//! marker in only some tests, or any smaller batch) is a program's failure,
//! since the preflight already passed. The environment report still carries
//! every `(index, seed, size)` pair in the batch, the marker's (or first
//! failing) program's detail and source, and any failures already recorded.
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

/// Output fragments that suggest the environment (a stale or missing
/// `runtime/` / stdlib build), not the generated program, is at fault, in a
/// batch of at least [`MIN_ENVIRONMENT_PROGRAMS`] programs where either no
/// test produced a result line (a runner crash after load: anywhere in the
/// output) or every test failed (then in every failure message); see
/// [`environment_cause`]. A re-run confirms the guess before it is reported
/// ([`environment_confirmed`]).
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

/// Fewest distinct programs in a batch before any rule of
/// [`environment_cause`] (a marker with no result lines or with every test
/// failed, or one message everywhere) counts as an environment failure. The
/// default (16) and nightly (192) draws are above it, so a real runner crash
/// short-circuits at the top level. With fewer programs (a bisection
/// sub-batch, or a narrowed draw via `CV_CORPUS_SHAPES` or a small
/// `CV_CORPUS_CASES`), one codegen bug looks the same, so the batch goes to
/// `Batch::Tests` or the cheap offender search, which blame the right program.
const MIN_ENVIRONMENT_PROGRAMS: usize = 4;

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
         (index, seed, size) pairs and the marker's (or first failing) \
         program below."
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
    /// Did not compile, crashed before any result line, or failed in a way
    /// [`environment_cause`] attributes to the environment. `environment` is
    /// then that suspected cause, which [`check`] confirms by a re-run before
    /// reporting it (unconfirmed, the batch is classified again without it).
    /// `None` is a compile failure or crash, bisected to its programs.
    Broken {
        text: String,
        environment: Option<EnvCause>,
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

/// The raw result of one `beamtalk test` run.
struct Run {
    success: bool,
    text: String,
}

fn run_package(pkg: &Package) -> Run {
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
    Run {
        success: out.status.success(),
        text,
    }
}

/// Classifies a run of `pkg`. `environment_min` is the fewest distinct
/// programs [`environment_cause`] may suspect the environment in, or `None`
/// to never suspect it (a guess the re-run did not confirm).
fn classify(pkg: &Package, run: &Run, environment_min: Option<usize>) -> Batch {
    let text = run.text.clone();
    // An `internal:` diagnostic is a verifier failure even when the compile
    // went on to succeed (a release build degrades the verifier to a warning
    // that the CLI build path prints, ADR 0111 Addendum 17, BT-3724).
    if text.contains("internal:") {
        return Batch::broken(text);
    }
    if run.success {
        return Batch::Pass;
    }
    if let Some(cause) = environment_min.and_then(|min| environment_cause(pkg, &text, min)) {
        return Batch::Broken {
            text,
            environment: Some(cause),
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

/// The kind of environment failure a batch is suspected of; a re-run
/// confirms the suspicion only when it shows the same kind.
#[derive(Debug, Clone, PartialEq, Eq)]
enum EnvKind {
    /// An [`ENVIRONMENT_MARKERS`] entry (its marker text).
    Marker(&'static str),
    /// Every program and spelling failed with this one message.
    SameMessage(String),
}

/// A suspected environment failure: its kind, and a one-line description
/// for the headline.
#[derive(Debug, PartialEq, Eq)]
struct EnvCause {
    kind: EnvKind,
    detail: String,
}

/// Why `text` (a failed run of `pkg`) looks like a broken environment rather
/// than a wrong program, or `None` (BT-3767). The preflight has already ruled
/// out a stale runtime, so this is deliberately narrow; one codegen bug must
/// never hide the rest of the batch. A batch of fewer than `min_programs`
/// distinct programs ([`MIN_ENVIRONMENT_PROGRAMS`] for a first guess) is never
/// environment (it falls through to `Batch::Tests` or bisection, which blame
/// its programs). In a larger batch:
///
/// - No test produced a result line (the runner crashed after load) and the
///   output contains an [`ENVIRONMENT_MARKERS`] entry: environment.
/// - Some tests failed and some did not: never environment (reported and
///   bisected like any other failure).
/// - Every test failed: environment when every failure message carries the
///   same marker (a stale runtime fails every test the same way; a marker in
///   only some messages is those programs' bug), or when every program and
///   spelling failed with the same message (generated programs expect
///   different answers, so identical messages across that many are unlikely
///   to be one bug).
///
/// This is a suspicion: [`check`] confirms it by a re-run
/// ([`environment_confirmed`]) before reporting it.
fn environment_cause(pkg: &Package, text: &str, min_programs: usize) -> Option<EnvCause> {
    let mut programs: Vec<usize> = pkg.test_names.iter().map(|(i, _, _)| *i).collect();
    programs.sort_unstable();
    programs.dedup();
    if programs.len() < min_programs {
        return None;
    }
    let reported: Vec<Option<&str>> = pkg
        .test_names
        .iter()
        .map(|(_, _, name)| failure_message(text, name))
        .collect();
    let marker = |(marker, cause): &(&'static str, &str), line: &str| EnvCause {
        kind: EnvKind::Marker(marker),
        detail: format!("{cause} (`{}`)", line.trim()),
    };
    if reported.iter().all(Option::is_none) {
        // Crashed before any result line: a marker anywhere in the output.
        let found = ENVIRONMENT_MARKERS.iter().find(|(m, _)| text.contains(m))?;
        let line = text
            .lines()
            .find(|l| l.contains(found.0))
            .unwrap_or(found.0);
        return Some(marker(found, line));
    }
    let messages: Vec<&str> = reported.into_iter().collect::<Option<_>>()?;
    let first = *messages.first()?;
    if let Some(found) = ENVIRONMENT_MARKERS
        .iter()
        .find(|(m, _)| messages.iter().all(|msg| msg.contains(m)))
    {
        return Some(marker(found, first));
    }
    messages.iter().all(|m| *m == first).then(|| EnvCause {
        kind: EnvKind::SameMessage(first.to_string()),
        detail: format!("every program and spelling failed with the same message (`{first}`)"),
    })
}

/// Does `rerun` (a run of `probe`, one program of the suspect batch, alone)
/// confirm the suspected environment failure `first`? A broken runtime or
/// stdlib fails every subset of a batch the same way; a codegen bug in one
/// program does not, so only the same [`EnvKind`] confirms (BT-3767).
fn environment_confirmed(first: &EnvCause, probe: &Package, rerun: &Run) -> bool {
    matches!(
        classify(probe, rerun, Some(1)),
        Batch::Broken { environment: Some(c), .. } if c.kind == first.kind
    )
}

/// Do all of `probes` (two different programs of the suspect batch, each run
/// alone) confirm `first`? One probe could be the very program with the bug
/// (failing alone the same way), so every probe must confirm (BT-3767).
fn environment_confirmed_by(first: &EnvCause, probes: &[(Package, Run)]) -> bool {
    !probes.is_empty()
        && probes
            .iter()
            .all(|(pkg, run)| environment_confirmed(first, pkg, run))
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
/// first program), then any failures `earlier` batches recorded, then the
/// output tail, so a wrong guess is never a dead end and the panic never drops
/// what bisection already found.
fn environment_report(
    headline: &str,
    text: &str,
    pkg: &Package,
    cases: &[&Case],
    earlier: &[Failed],
) -> String {
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
    if !earlier.is_empty() {
        let _ = writeln!(
            s,
            "--- failures recorded before this batch: {}",
            earlier.len()
        );
        for f in earlier {
            let _ = writeln!(
                s,
                "  program {} (seed {}, size {}): {}",
                f.index,
                f.seed,
                f.size,
                match &f.failure {
                    Failure::Compile(h, _) => format!("did not compile: {h}"),
                    Failure::Wrong(sp, d) => format!("wrong answer in the {sp:?} spelling: {d}"),
                }
            );
        }
    }
    let _ = write!(s, "--- output tail\n{}", tail(text));
    s
}

/// Runs one case's program alone, for the environment confirmation.
fn run_alone(case: &Case) -> (Package, Run) {
    let pkg = render_package(&[(case.index, case.program.clone())]);
    let run = run_package(&pkg);
    (pkg, run)
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
    let run = run_package(&pkg);
    let batch = match classify(&pkg, &run, Some(MIN_ENVIRONMENT_PROGRAMS)) {
        // Suspected environment: confirm by re-running two different programs
        // alone (the first and the last; a broken environment fails every
        // subset, while a single buggy program fails only itself) before
        // reporting it. Unconfirmed, classify the batch again without the
        // guess, so its programs are isolated.
        Batch::Broken {
            environment: Some(cause),
            ..
        } => {
            let first_probe = cases[0];
            let last_probe = cases[cases.len() - 1];
            let probes = [first_probe, last_probe].map(run_alone);
            if cases.len() > 1 && environment_confirmed_by(&cause, &probes) {
                // Not (probably) a program's fault: no bisection; the report
                // leads with the cause, then every repro pair, the marker's
                // (or first failing) program, and any failures already
                // recorded.
                let headline = format!(
                    "{} Confirmed by re-running programs {} (seed {}, size {}) and {} \
                     (seed {}, size {}) separately.",
                    environment_headline(&cause.detail),
                    first_probe.index,
                    first_probe.seed,
                    first_probe.size,
                    last_probe.index,
                    last_probe.seed,
                    last_probe.size
                );
                panic!(
                    "{}",
                    environment_report(&headline, &run.text, &pkg, cases, failed)
                );
            }
            classify(&pkg, &run, None)
        }
        batch => batch,
    };
    match batch {
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
        Batch::Broken { text, .. } => {
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

/// `MIN_ENVIRONMENT_PROGRAMS` small programs, program `i` from seed
/// `1000 + i`, size 1 (for the classification tests below).
fn fixture_cases() -> Vec<Case> {
    use beamtalk_core::test_helpers::class_var_program::gen_program;
    (0..MIN_ENVIRONMENT_PROGRAMS)
        .map(|index| Case {
            index,
            seed: 1000 + index as u64,
            size: 1,
            program: gen_program(1000 + index as u64, 1, Shapes::ENABLED),
        })
        .collect()
}

fn package_of(cases: &[Case]) -> Package {
    let programs: Vec<(usize, Program)> =
        cases.iter().map(|c| (c.index, c.program.clone())).collect();
    render_package(&programs)
}

/// A test runner result line for a failed test method.
fn line(name: &str, msg: &str) -> String {
    format!("  FAIL {TEST_CLASS} {name}: {msg}\n")
}

/// Runner output in which every test of `pkg` failed with `msg`.
fn all_fail(pkg: &Package, msg: &str) -> String {
    pkg.test_names
        .iter()
        .map(|(_, _, n)| line(n, msg))
        .collect()
}

/// A first guess, at the threshold the harness uses for a whole batch.
fn suspect(pkg: &Package, text: &str) -> Option<EnvCause> {
    environment_cause(pkg, text, MIN_ENVIRONMENT_PROGRAMS)
}

const STALE: &str = "setUp failed: Undefined function: bt@stdlib@test_case:new/0";

/// BT-3767: the harness reads failures off the test runner's own
/// `FAIL <class> <method>: <message>` result lines, and classifies an
/// environment failure as a broken batch, not as wrong answers. No BEAM.
#[test]
fn class_var_agreement_classifies_runner_output() {
    let cases = fixture_cases();
    let pkg = package_of(&cases);
    let names: Vec<&str> = pkg.test_names.iter().map(|(_, _, n)| n.as_str()).collect();

    // One wrong answer: found by its result line, not an environment failure.
    let one = format!("Running tests...\n{}", line(names[0], "expected 3, got 4"));
    assert_eq!(failure_message(&one, names[0]), Some("expected 3, got 4"));
    assert_eq!(failure_message(&one, names[1]), None);
    assert_eq!(suspect(&pkg, &one), None);

    // Free text naming a test is not a result line.
    let prose = format!("note: {} failed to FAIL", names[0]);
    assert_eq!(failure_message(&prose, names[0]), None);

    // Every program and spelling failing with the same message, across
    // `MIN_ENVIRONMENT_PROGRAMS` programs.
    let same: String = names.iter().map(|n| line(n, "boom")).collect();
    assert!(suspect(&pkg, &same).is_some_and(|c| c.detail.contains("same message")));
    // ... but below the threshold it may be one codegen bug: not classified.
    let small = package_of(&cases[..MIN_ENVIRONMENT_PROGRAMS - 1]);
    let small_same: String = small
        .test_names
        .iter()
        .map(|(_, _, n)| line(n, "boom"))
        .collect();
    assert_eq!(suspect(&small, &small_same), None);
    // ... but different messages are wrong answers.
    let differ: String = names
        .iter()
        .enumerate()
        .map(|(i, n)| line(n, &format!("expected {i}")))
        .collect();
    assert_eq!(suspect(&pkg, &differ), None);

    // A marker in one program of a mixed batch is that program's failure
    // (the preflight passed): not an environment failure, so it is reported
    // and bisected like any other.
    let one_marker = line(names[0], "Undefined function: beamtalk_x:y/0");
    assert_eq!(suspect(&pkg, &one_marker), None);

    // A runner crash after load: a marker and no result line at all, in a
    // `MIN_ENVIRONMENT_PROGRAMS`-program batch, is an environment failure.
    let crash = "Running tests...\nerror: abi_mismatch: module compiled for ABI 3\n";
    assert!(suspect(&pkg, crash).is_some_and(|c| c.detail.contains("ABI")));
    // ... but a 1-program batch (a bisection sub-batch) is that program's
    // load-time failure: not classified, so the offender search blames it.
    let tiny = package_of(&cases[..1]);
    assert_eq!(suspect(&tiny, crash), None);
    // ... but no marker and no result line is a crash, bisected as usual.
    assert_eq!(suspect(&pkg, "Running tests...\nboom\n"), None);

    // The marker in every spelling of a 1-program batch may be one codegen
    // bug: not classified (`Batch::Tests` reports each message).
    let tiny_marker: String = tiny
        .test_names
        .iter()
        .map(|(_, _, n)| line(n, "Undefined function: beamtalk_x:y/0"))
        .collect();
    assert_eq!(suspect(&tiny, &tiny_marker), None);

    // Every test failed, but only program 2's messages carry a marker: the
    // marker is that program's bug, not the environment's.
    let late_marker: String = pkg
        .test_names
        .iter()
        .map(|(i, _, n)| {
            if *i == 2 {
                line(n, "abi_mismatch: expected 3, runtime 2")
            } else {
                line(n, &format!("expected {i}, got 0"))
            }
        })
        .collect();
    assert_eq!(suspect(&pkg, &late_marker), None);

    // A stale stdlib: the marker in every test of a
    // `MIN_ENVIRONMENT_PROGRAMS`-program batch is an environment failure.
    let stale = all_fail(&pkg, STALE);
    let cause = suspect(&pkg, &stale).expect("environment");
    assert_eq!(cause.kind, EnvKind::Marker("Undefined function"));
    assert!(
        cause
            .detail
            .contains("missing from the runtime or stdlib build"),
        "{cause:?}"
    );
    let headline = environment_headline(&cause.detail);
    assert!(headline.contains("just build-stdlib"), "{headline}");
    assert!(headline.contains("preflight passed"), "{headline}");
    assert!(headline.contains("codegen bug"), "{headline}");
}

/// BT-3767: an environment report carries every repro pair, the marker's own
/// program and the failures earlier batches recorded. No BEAM.
#[test]
fn class_var_agreement_classifies_runner_output_report() {
    let cases = fixture_cases();
    let pkg = package_of(&cases);
    let stale = all_fail(&pkg, STALE);
    let headline = environment_headline(&suspect(&pkg, &stale).expect("environment").detail);

    // The environment report carries every repro pair, the marker program's
    // detail and the failures earlier batches recorded, not only the tail.
    let refs: Vec<&Case> = cases.iter().collect();
    let earlier = [Failed {
        index: 7,
        seed: 4242,
        size: 3,
        failure: Failure::Wrong(Spelling::Open, "expected 1, got 2".to_string()),
    }];
    let report = environment_report(&headline, &stale, &pkg, &refs, &earlier);
    assert!(
        report.contains("failures recorded before this batch: 1")
            && report.contains("program 7 (seed 4242, size 3): wrong answer"),
        "{report}"
    );
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
    // first FAIL line in test order (whether or not the batch was classified).
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
    let report = environment_report("headline", &late_marker, &pkg, &refs, &[]);
    assert!(
        report.contains(&format!(
            "environment-marker program {marked} (seed {}, size 1)",
            1000 + marked
        )),
        "{report}"
    );
}

/// BT-3767: a suspected environment failure is reported only when re-running
/// two different programs of the batch alone both fail the same way; otherwise the batch is
/// classified again without the guess and bisected. No BEAM.
#[test]
fn class_var_agreement_classifies_runner_output_confirm() {
    let cases = fixture_cases();
    let pkg = package_of(&cases);
    let probe = package_of(&cases[cases.len() - 1..]);
    let first_probe = package_of(&cases[..1]);
    let failed_run = |text: String| Run {
        success: false,
        text,
    };

    // A stale stdlib fails the lone program the same way: confirmed.
    let stale = suspect(&pkg, &all_fail(&pkg, STALE)).expect("environment");
    assert!(environment_confirmed(
        &stale,
        &probe,
        &failed_run(all_fail(&probe, STALE))
    ));
    // The lone program passes, answers wrongly, or hits a different marker:
    // one program's bug, not the environment. Not confirmed.
    let passed = Run {
        success: true,
        text: String::new(),
    };
    assert!(!environment_confirmed(&stale, &probe, &passed));
    assert!(!environment_confirmed(
        &stale,
        &probe,
        &failed_run(all_fail(&probe, "expected 3, got 4"))
    ));
    assert!(!environment_confirmed(
        &stale,
        &probe,
        &failed_run(all_fail(&probe, "abi_mismatch: stamp 2"))
    ));

    // A runner crash after load (no result lines) confirmed by the same crash
    // alone; a one-class ABI stamp bug is not (the lone program runs).
    let crash = "Running tests...\nerror: abi_mismatch: module compiled for ABI 3\n";
    let crashed = suspect(&pkg, crash).expect("environment");
    assert!(environment_confirmed(
        &crashed,
        &probe,
        &failed_run(crash.to_string())
    ));
    assert!(!environment_confirmed(
        &crashed,
        &probe,
        &failed_run(all_fail(&probe, "expected 3, got 4"))
    ));

    // One message everywhere: confirmed only by the same message.
    let same = suspect(&pkg, &all_fail(&pkg, "boom")).expect("environment");
    assert!(environment_confirmed(
        &same,
        &probe,
        &failed_run(all_fail(&probe, "boom"))
    ));
    assert!(!environment_confirmed(
        &same,
        &probe,
        &failed_run(all_fail(&probe, "bang"))
    ));

    // Two probes (first and last program, each alone): both must fail the
    // same way. One probe crashing while the other runs cleanly (the one-class
    // ABI stamp bug in a single program) is not an environment failure.
    let crash_run = || failed_run(crash.to_string());
    let clean = || Run {
        success: true,
        text: String::new(),
    };
    let both = vec![
        (first_probe.clone(), crash_run()),
        (probe.clone(), crash_run()),
    ];
    assert!(environment_confirmed_by(&crashed, &both));
    let one_clean = vec![(first_probe.clone(), clean()), (probe.clone(), crash_run())];
    assert!(!environment_confirmed_by(&crashed, &one_clean));
    let other_clean = vec![(first_probe, crash_run()), (probe, clean())];
    assert!(!environment_confirmed_by(&crashed, &other_clean));
    assert!(!environment_confirmed_by(&crashed, &[]));
}

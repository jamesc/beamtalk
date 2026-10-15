// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 0 measurement harness (BT-3841): what does codegen do with
//! each statement-position probe, with the position check out of the way?
//!
//! `beamtalk-core`'s `test_helpers::statement_probes` holds the probe matrix
//! (`adr0131_statement_probes.tsv`: one `t := 0` method per construct kind,
//! per block role, per method context, per write target). This harness
//! compiles every class, value-type and actor cell that is not a `gap` line
//! through the real `beamtalk test` pipeline with the Phase 0 statement check
//! bypassed (`BEAMTALK_ADR0131_MEASURE_BYPASS_PHASE0`, debug builds only),
//! runs it on a BEAM, and records the answer in
//! `adr0131_statement_measured.tsv`:
//!
//! - `right`: the method answers the ADR-expected value
//!   (`STATEMENT_PROBE_KINDS`);
//! - `wrong`: it compiles and answers something else, or raises (a silent
//!   drop of the write);
//! - `erlc`: the module does not compile (a codegen error, a `ThreadedIr`
//!   verifier panic or an `erlc` failure).
//!
//! The Phase 0 pin tests in `beamtalk-core` assert that Phase 0 rejects a cell
//! exactly when this table says it does not answer `right`; this harness
//! keeps that table honest. It is `#[ignore]`d (about a minute on a BEAM):
//!
//! ```text
//! just measure-adr0131-statements          # fail when the table drifts
//! ADR0131_MEASURE_BLESS=1 just measure-adr0131-statements   # rewrite it
//! ```
//!
//! Cells are batched [`CHUNK`] to a class. A batch that does not compile is
//! split in halves (into separate classes) until the offending cells are
//! alone; those cells are `erlc`.

use crate::cli_common;

use beamtalk_core::test_helpers::ADR0131_MEASURE_BYPASS_ENV;
use beamtalk_core::test_helpers::statement_probes::{Matrix, class_source, probe_method};
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::path::Path;
use std::sync::Mutex;

/// Cells per generated class.
const CHUNK: usize = 40;

/// Packages measured at once (each is its own `beamtalk test` and BEAM).
const WORKERS: usize = 4;

/// The test class of every measurement package.
const TEST_CLASS: &str = "Adr0131StatementMeasure";

/// One probe cell.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Cell {
    context: String,
    container: String,
    target: String,
    kind: String,
}

impl Cell {
    fn line(&self, answer: &str) -> String {
        format!(
            "{}\t{}\t{}\t{}\t{answer}",
            self.context, self.container, self.target, self.kind
        )
    }
}

/// Every measured cell of `matrix`.
fn cells(matrix: Matrix) -> Vec<Cell> {
    matrix
        .cells()
        .into_iter()
        .map(|[context, container, target, kind]| Cell {
            context,
            container,
            target,
            kind,
        })
        .collect()
}

/// The raw result of one `beamtalk test` run.
struct Run {
    success: bool,
    text: String,
}

/// A package of one `TestCase` and the fixture classes of `cells`, grouped
/// `chunk` to a class. `names[i]` is the test method of `cells[i]`.
struct Package {
    fixtures: Vec<(String, String)>,
    test: String,
    names: Vec<String>,
}

/// The fixture class of group `group`: `Adr0131ProbeC3`, `...V0`, `...A12`.
fn class_name(context: &str, group: usize) -> String {
    let tag = context[..1].to_ascii_uppercase();
    format!("Adr0131Probe{tag}{group}")
}

/// The fixture file of `class` (the snake case of its name).
fn fixture_file(class: &str) -> String {
    let mut file = String::new();
    let mut prev = ' ';
    for ch in class.chars() {
        if ch.is_ascii_uppercase() && prev.is_ascii_lowercase() {
            file.push('_');
        }
        file.push(ch.to_ascii_lowercase());
        prev = ch;
    }
    format!("{file}.bt")
}

fn render(matrix: Matrix, cells: &[Cell], chunk: usize) -> Package {
    let mut fixtures = Vec::new();
    let mut test = format!(
        "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\n\
         TestCase subclass: {TEST_CLASS}\n\n"
    );
    let mut names = Vec::new();
    let mut group = 0;
    let mut index = 0;
    for context in ["class", "value", "actor"] {
        let of_context: Vec<&Cell> = cells.iter().filter(|c| c.context == context).collect();
        for batch in of_context.chunks(chunk) {
            let class = class_name(context, group);
            let mut methods = Vec::new();
            for (k, cell) in batch.iter().enumerate() {
                let selector = format!("probe{k}:");
                methods.push(probe_method(
                    matrix,
                    context,
                    &selector,
                    &cell.container,
                    &cell.target,
                    &cell.kind,
                ));
                let receiver = match context {
                    "class" => class.clone(),
                    "value" => format!("({class} new)"),
                    _ => format!("({class} spawn)"),
                };
                let name = format!("test{index}");
                let _ = write!(
                    test,
                    "  {name} =>\n    self assert: ({receiver} probe{k}: true) equals: {}\n\n",
                    matrix.expected(&cell.container, &cell.target, &cell.kind)
                );
                names.push(name);
                index += 1;
            }
            fixtures.push((
                fixture_file(&class),
                format!(
                    "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\n{}",
                    class_source(context, &class, &methods)
                ),
            ));
            group += 1;
        }
    }
    Package {
        fixtures,
        test,
        names,
    }
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
         name = \"adr0131_statement_measure\"\n\
         version = \"0.1.0\"\n\
         \n\
         [dependencies]\n",
    )
    .expect("write beamtalk.toml");
    for (name, source) in &pkg.fixtures {
        std::fs::write(fixtures.join(name), source).expect("write fixture");
    }
    std::fs::write(root.join("test").join(fixture_file(TEST_CLASS)), &pkg.test)
        .expect("write test");
}

fn run_package(pkg: &Package) -> Run {
    let dir = tempfile::tempdir().expect("tempdir");
    write_package(dir.path(), pkg);
    let out = cli_common::beamtalk()
        .current_dir(dir.path())
        .env(ADR0131_MEASURE_BYPASS_ENV, "1")
        .args(["test"])
        .output()
        .expect("run beamtalk test");
    Run {
        success: out.status.success(),
        text: format!(
            "{}\n{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ),
    }
}

/// Whether the test runner reported test method `name` as failed.
fn failed(text: &str, name: &str) -> bool {
    let prefix = format!("FAIL {TEST_CLASS} {name}:");
    text.lines().any(|l| l.trim_start().starts_with(&prefix))
}

/// Whether the package compiled and the runner reported results.
fn ran(text: &str) -> bool {
    text.contains("Running tests") && !text.contains("internal:") && !text.contains("panicked at")
}

/// Measures `cells` (one context), recording each cell's answer in `out`.
fn measure(matrix: Matrix, cells: &[Cell], out: &Mutex<BTreeMap<Cell, &'static str>>) {
    if cells.is_empty() {
        return;
    }
    let pkg = render(matrix, cells, CHUNK);
    let run = run_package(&pkg);
    if ran(&run.text) {
        let mut out = out.lock().expect("lock");
        for (cell, name) in cells.iter().zip(&pkg.names) {
            let answer = if failed(&run.text, name) {
                "wrong"
            } else {
                "right"
            };
            out.insert(cell.clone(), answer);
        }
        assert!(
            run.success || pkg.names.iter().any(|n| failed(&run.text, n)),
            "a run that ran but reported no failure did not succeed:\n{}",
            run.text
        );
        return;
    }
    if std::env::var_os("ADR0131_MEASURE_VERBOSE").is_some() {
        let head: Vec<&str> = run.text.lines().take(25).collect();
        eprintln!(
            "--- {} cells did not compile:\n{}",
            cells.len(),
            head.join("\n")
        );
    }
    if cells.len() == 1 {
        out.lock().expect("lock").insert(cells[0].clone(), "erlc");
        return;
    }
    // Not compiling: split in halves, each into classes of their own.
    let mid = cells.len() / 2;
    measure(matrix, &cells[..mid], out);
    measure(matrix, &cells[mid..], out);
}

/// The measured table text for `answers`.
fn table(matrix: Matrix, answers: &BTreeMap<Cell, &'static str>, order: &[Cell]) -> String {
    let (what, third) = match matrix {
        Matrix::Statement => (
            "statement-position probe cells (adr0131_statement_probes.tsv, not `gap`)",
            "write target",
        ),
        Matrix::Operand => (
            "operand-position probe cells (OPERAND_POSITIONS in test_helpers/statement_probes.rs)",
            "operand position",
        ),
    };
    let mut text = format!(
        "# ADR 0131 Phase 0 measured answers (BT-3841). One line per class, value-type and\n\
         # actor {what}: context, block\n\
         # role, {third}, construct kind, answer. `right` is the ADR-expected value,\n\
         # `wrong` a different answer or a raise, `erlc` a module that does not compile,\n\
         # all with the Phase 0 statement check bypassed and run on a BEAM. Regenerate with\n\
         # `ADR0131_MEASURE_BLESS=1 just measure-adr0131-statements`.\n"
    );
    for cell in order {
        let _ = writeln!(text, "{}", cell.line(answers[cell]));
    }
    text
}

fn run_matrix(matrix: Matrix, file: &str) {
    let order = cells(matrix);
    assert!(order.len() > 1000, "only {} cells", order.len());
    let answers = Mutex::new(BTreeMap::new());
    let batches: Vec<Vec<Cell>> = ["class", "value", "actor"]
        .iter()
        .flat_map(|ctx| {
            let of_context: Vec<Cell> = order
                .iter()
                .filter(|c| c.context == *ctx)
                .cloned()
                .collect();
            of_context
                .chunks(CHUNK)
                .map(<[Cell]>::to_vec)
                .collect::<Vec<_>>()
        })
        .collect();
    let next = std::sync::atomic::AtomicUsize::new(0);
    std::thread::scope(|scope| {
        for _ in 0..WORKERS {
            scope.spawn(|| {
                loop {
                    let i = next.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    let Some(batch) = batches.get(i) else { break };
                    measure(matrix, batch, &answers);
                }
            });
        }
    });
    let answers = answers.into_inner().expect("lock");
    assert_eq!(answers.len(), order.len(), "every cell has an answer");

    let mut counts: BTreeMap<(&str, &str), usize> = BTreeMap::new();
    for (cell, answer) in &answers {
        *counts.entry((cell.context.as_str(), answer)).or_default() += 1;
    }
    eprintln!("context\tanswer\tcells");
    for ((context, answer), n) in &counts {
        eprintln!("{context}\t{answer}\t{n}");
    }

    let measured = table(matrix, &answers, &order);
    if std::env::var_os("ADR0131_MEASURE_BLESS").is_some() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../beamtalk-core/src/semantic_analysis/tests")
            .join(file);
        std::fs::write(&path, &measured).expect("write the measured table");
        eprintln!("wrote {}", path.display());
        return;
    }
    assert!(
        measured == matrix.measured_table(),
        "the measured answers drifted from {file}; re-run with ADR0131_MEASURE_BLESS=1 and \
         review the diff:\n{measured}"
    );
}

#[test]
#[ignore = "compiles and runs ~2500 probes on a BEAM; run `just measure-adr0131-statements`"]
fn adr0131_measure_statements() {
    run_matrix(Matrix::Statement, "adr0131_statement_measured.tsv");
}

#[test]
#[ignore = "compiles and runs ~1400 probes on a BEAM; run `just measure-adr0131-statements`"]
fn adr0131_measure_operands() {
    run_matrix(Matrix::Operand, "adr0131_operand_measured.tsv");
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4288: a `^` in the body of a loop or collection operation at the REPL
//! ends the input with its value, as in a method, and the workspace bindings
//! written before it keep their values.
//!
//! A generated corpus: every loop and fold kind whose body is run in place
//! (`beamtalk-core`'s `block_facts::loop_body_returns`), crossed with the
//! position of the `^` in the body (a statement, a conditional arm, an
//! evaluated block, a nested loop, a protected block, a `match:` arm, before a
//! binding write, an assigned value, a body that writes no binding, and one
//! that never fires), and the
//! position of the loop in the input (after a binding write, alone, before
//! another statement, as an operand, in a conditional arm). Every row is
//! interpreted by [`expected`], the reference semantics of a method, and:
//!
//! 1. compiles with the debug `ThreadedIr` verifier on and through `erlc`;
//! 2. with the REPL eval boundary switched off
//!    (`repl_nlr::boundary_fault::without_boundary`), fails with
//!    `VerifyError::LoopReturnWithoutBoundary`, the check that fails on the
//!    repro before the fix;
//! 3. runs on a BEAM against the runtime, in eval and trace mode, and answers
//!    the value and the bindings [`expected`] says.

use beamtalk_codegen::core_erlang::repl_nlr::boundary_fault::without_boundary;
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_repl::codegen::{
    generate_repl_expressions, generate_repl_expressions_surfacing_verifier,
    generate_repl_expressions_traced,
};
use std::collections::HashMap;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};
use std::process::Command;

/// How a loop kind runs its body.
#[derive(Clone, Copy)]
enum Iteration {
    /// `x` is the block parameter, over 1, 2, 3.
    Param,
    /// `x` is a workspace binding the loop counts from 0 to 3, incremented
    /// before the body (`{S}`) in each iteration.
    Binding,
}

/// The loop kinds: the loop with `{S}` for its body, and how `x` is bound.
/// The unary `[...] whileTrue`/`whileFalse` are left out: at the REPL they
/// raise `does_not_understand` whatever their body (BT-4313).
const LOOPS: &[(&str, Iteration)] = &[
    ("#(1, 2, 3) do: [:x | {S}]", Iteration::Param),
    ("#(1, 2, 3) collect: [:x | {S}. x]", Iteration::Param),
    ("#(1, 2, 3) select: [:x | {S}. true]", Iteration::Param),
    ("#(1, 2, 3) reject: [:x | {S}. false]", Iteration::Param),
    (
        "#(1, 2, 3) inject: 0 into: [:a :x | {S}. a + x]",
        Iteration::Param,
    ),
    (
        "#(1, 2, 3) detect: [:x | {S}. false] ifNone: [0]",
        Iteration::Param,
    ),
    ("#(1, 2, 3) count: [:x | {S}. true]", Iteration::Param),
    ("#(1, 2, 3) anySatisfy: [:x | {S}. false]", Iteration::Param),
    ("#(1, 2, 3) allSatisfy: [:x | {S}. true]", Iteration::Param),
    (
        "#(1, 2, 3) do: [:x | {S}] separatedBy: [nil]",
        Iteration::Param,
    ),
    ("1 to: 3 do: [:x | {S}]", Iteration::Param),
    ("1 to: 3 by: 1 do: [:x | {S}]", Iteration::Param),
    ("3 timesRepeat: [x := x + 1. {S}]", Iteration::Binding),
    ("[x < 3] whileTrue: [x := x + 1. {S}]", Iteration::Binding),
    ("[x >= 3] whileFalse: [x := x + 1. {S}]", Iteration::Binding),
];

/// What one position of the `^` does in one iteration: the next `w`, and the
/// value the `^` answers if it fires.
type Step = fn(i64, i64) -> (i64, Option<i64>);

/// The positions of the `^` in the loop body, with their reference semantics.
const POSITIONS: &[(&str, &str, Step)] = &[
    ("statement", "w := w + x. ^w", |x, w| (w + x, Some(w + x))),
    ("arm", "w := w + x. x == 2 ifTrue: [^w]", |x, w| {
        (w + x, (x == 2).then_some(w + x))
    }),
    (
        "two-armed conditional",
        "x == 2 ifTrue: [^w + 100] ifFalse: [w := w + x]",
        |x, w| {
            if x == 2 {
                (w, Some(w + 100))
            } else {
                (w + x, None)
            }
        },
    ),
    (
        "evaluated block",
        "w := w + x. x == 2 ifTrue: [[^w] value]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    (
        "nested loop",
        "w := w + x. #(10) do: [:y | x == 2 ifTrue: [^w + y]]",
        |x, w| (w + x, (x == 2).then_some(w + x + 10)),
    ),
    (
        "protected block",
        "w := w + x. [x == 2 ifTrue: [^w]] on: Error do: [:e | 0]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    (
        "ensure: block",
        "w := w + x. [x == 2 ifTrue: [^w]] ensure: [nil]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    (
        "match: arm",
        "w := w + x. x match: [2 -> ^w; _ -> nil]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    (
        "before a write",
        "x == 2 ifTrue: [^w]. w := w + x",
        |x, w| {
            if x == 2 { (w, Some(w)) } else { (w + x, None) }
        },
    ),
    (
        "assigned value",
        "x == 2 ifTrue: [^w := w + 10]. w := w + x",
        |x, w| {
            if x == 2 {
                (w + 10, Some(w + 10))
            } else {
                (w + x, None)
            }
        },
    ),
    // A body that writes no binding: the throw carries the bindings the
    // loop was entered with.
    ("no write", "x == 2 ifTrue: [^x * 7]", |x, w| {
        (w, (x == 2).then_some(14))
    }),
    ("never fires", "w := w + x. x == 5 ifTrue: [^w]", |x, w| {
        (w + x, (x == 5).then_some(w + x))
    }),
];

/// The positions of the loop (`{L}`) in the input, with the value `w` has
/// before the loop runs and the value of `w` and of the input when the `^`
/// never fires (`None`: the input's value is the loop's own, not checked).
const CONTEXTS: &[(&str, &str, i64, Option<i64>)] = &[
    ("after a write", "w := 5\n{L}", 5, None),
    ("alone", "{L}", 0, None),
    ("before a statement", "{L}\nw := 99", 0, Some(99)),
    ("operand", "#[0, ({L})]", 0, None),
    ("conditional arm", "true ifTrue: [{L}]", 0, None),
];

/// One generated input.
struct Row {
    name: String,
    source: String,
    expected: Expected,
}

/// The reference answer of a row: the input's value (`None` when not
/// checked) and the bindings `w` and `x` after it.
#[derive(Debug, PartialEq, Eq)]
struct Expected {
    fired: bool,
    value: Option<i64>,
    w: i64,
    x: Option<i64>,
}

/// The reference semantics: the method's. The loop runs its body over
/// `x = 1, 2, 3` until the `^` fires; the `^` ends the input with its value,
/// and every binding written before it keeps its value.
fn expected(iteration: Iteration, step: Step, w0: i64, unfired_w: Option<i64>) -> Expected {
    let mut w = w0;
    for x in 1..=3 {
        let (next, fired) = step(x, w);
        w = next;
        if let Some(value) = fired {
            return Expected {
                fired: true,
                value: Some(value),
                w,
                x: matches!(iteration, Iteration::Binding).then_some(x),
            };
        }
    }
    Expected {
        fired: false,
        value: unfired_w,
        w: unfired_w.unwrap_or(w),
        x: matches!(iteration, Iteration::Binding).then_some(3),
    }
}

/// Every generated input the compiler accepts. A row whose loop writes a
/// binding where a top-level expression does not thread the write back yet
/// (BT-3743: a loop that is a literal element) is rejected before it compiles,
/// for that reason and no other, and left out.
fn rows() -> Vec<Row> {
    let rows = all_rows();
    let total = rows.len();
    let accepted: Vec<Row> = rows
        .into_iter()
        .filter(|row| match parse_input(&row.source) {
            Ok(_) => true,
            Err(e) if e.contains("(BT-3743)") => false,
            Err(e) => panic!("{}: rejected:\n{}\n{e}", row.name, row.source),
        })
        .collect();
    assert!(
        accepted.len() + 5 >= total,
        "{} of {total} rows are rejected (BT-3743)",
        total - accepted.len()
    );
    accepted
}

fn all_rows() -> Vec<Row> {
    let mut rows = Vec::new();
    let mut push = |loop_index: usize, position: usize, context: usize| {
        let (lp, iteration) = LOOPS[loop_index];
        let (pos_name, body, step) = POSITIONS[position];
        let (ctx_name, template, w0, unfired_w) = CONTEXTS[context];
        let loop_src = lp.replace("{S}", body);
        let source = template.replace("{L}", &loop_src);
        rows.push(Row {
            name: format!("{lp} / {pos_name} / {ctx_name}"),
            source,
            expected: expected(iteration, step, w0, unfired_w),
        });
    };
    for loop_index in 0..LOOPS.len() {
        // Every position after a binding write in the same input...
        for position in 0..POSITIONS.len() {
            push(loop_index, position, 0);
        }
        // ... and the arm position in every other loop position.
        for context in 1..CONTEXTS.len() {
            push(loop_index, 1, context);
        }
    }
    rows
}

/// The workspace bindings every row starts from.
const KNOWN: &[&str] = &["w", "x"];

/// The input's expressions, or the first message of the compile errors that
/// reject it.
fn parse_input(src: &str) -> Result<Vec<Expression>, String> {
    let (parsed, diags) = parse(lex_with_eof(src));
    assert!(diags.is_empty(), "parse {src}: {diags:?}");
    let analysis = analyse_full(&parsed, AnalysisContext::default().with_known_vars(KNOWN));
    if let Some(error) = analysis
        .diagnostics
        .iter()
        .find(|d| d.severity == Severity::Error)
    {
        return Err(error.message.to_string());
    }
    Ok(parsed
        .expressions
        .into_iter()
        .map(|s| s.expression)
        .collect())
}

fn panic_message(payload: &(dyn std::any::Any + Send)) -> String {
    payload
        .downcast_ref::<String>()
        .cloned()
        .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
        .unwrap_or_default()
}

/// Generates `src` as eval (`trace: false`) or trace module `module`. A
/// verifier finding (a debug panic) or a codegen error is an `Err`.
fn generate(src: &str, module: &str, trace: bool) -> Result<String, String> {
    let expressions = parse_input(src).map_err(|e| format!("rejected: {e}"))?;
    match catch_unwind(AssertUnwindSafe(|| {
        if trace {
            generate_repl_expressions_traced(&expressions, src, module, HashMap::new())
        } else {
            generate_repl_expressions(&expressions, module)
        }
    })) {
        Ok(Ok(code)) => Ok(code),
        Ok(Err(e)) => Err(format!("codegen: {e:?}")),
        Err(payload) => Err(format!("panic: {}", panic_message(payload.as_ref()))),
    }
}

/// The `ThreadedIr` verifier's findings on `src`, in a debug build (where a
/// finding fails generation with a panic) and a release build (where it is
/// an `internal:` warning beside the generated code) alike.
fn verifier_report(src: &str) -> String {
    let expressions = match parse_input(src) {
        Ok(expressions) => expressions,
        Err(e) => return format!("rejected: {e}"),
    };
    match catch_unwind(AssertUnwindSafe(|| {
        generate_repl_expressions_surfacing_verifier(
            &expressions,
            "bt4288_off",
            HashMap::new(),
            None,
            false,
        )
    })) {
        Ok(Ok((_, diagnostics))) => diagnostics
            .iter()
            .map(|d| d.message.to_string())
            .collect::<Vec<_>>()
            .join("\n"),
        Ok(Err(e)) => format!("codegen: {e:?}"),
        Err(payload) => format!("panic: {}", panic_message(payload.as_ref())),
    }
}

fn erlang_available() -> bool {
    Command::new("erlc").arg("-help").output().is_ok()
        && Command::new("erl").arg("-version").output().is_ok()
}

/// Compiles `modules` with `erlc` into `dir`; `Err` with `erlc`'s output.
///
/// In batches: Windows caps a command line at ~32K characters, and this
/// corpus is hundreds of modules, so one `erlc` call with every path
/// overflows it (`Os { code: 206 }`, BT-4224). Batches of 32 keep every
/// invocation to a few kilobytes on every platform.
fn erlc(dir: &Path, modules: &[(String, String)]) -> Result<(), String> {
    // Windows caps a command line at ~32K characters (see below).
    const BATCH: usize = 32;
    let mut files = Vec::new();
    for (name, code) in modules {
        let file = dir.join(format!("{name}.core"));
        std::fs::write(&file, code).expect("write core");
        files.push(file);
    }
    for chunk in files.chunks(BATCH) {
        let output = Command::new("erlc")
            .arg("+from_core")
            .arg("-o")
            .arg(dir)
            .args(chunk)
            .output()
            .expect("run erlc");
        if !output.status.success() {
            return Err(format!(
                "{}{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ));
        }
    }
    Ok(())
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .expect("repo root")
        .to_path_buf()
}

/// The `ebin` directory of every application the runtime build produced,
/// led by the built stdlib's (its `bt@stdlib@*` class modules and the
/// generated `beamtalk_stdlib.app` that loads them, found before the rebar3
/// one). Fails
/// when the runtime or the stdlib is not built, so the execution proof cannot
/// pass vacuously.
fn runtime_ebins() -> Vec<PathBuf> {
    let root = repo_root();
    let lib = root.join("runtime/_build/default/lib");
    let stdlib = root.join("runtime/apps/beamtalk_stdlib/ebin");
    for built in [
        lib.join("beamtalk_runtime/ebin/beamtalk_runtime.app"),
        stdlib.join("beamtalk_stdlib.app"),
        stdlib.join("bt@stdlib@block.beam"),
    ] {
        assert!(
            built.exists(),
            "{} is not built: run `just build` first",
            built.display()
        );
    }
    let mut ebins: Vec<PathBuf> = std::fs::read_dir(&lib)
        .expect("runtime lib dir")
        .filter_map(Result::ok)
        .map(|entry| entry.path().join("ebin"))
        .filter(|ebin| ebin.is_dir())
        .collect();
    ebins.sort();
    ebins.insert(0, stdlib);
    ebins
}

/// Runs `eval/1` of every module in `modules` (compiled into `dir`; `true`
/// for a trace module) on a node with the stdlib started, from the bindings
/// `w => 0, x => 0`, and answers per module the input's value (a trace
/// module's last step's, with the step count) and the bindings `w` and `x`.
///
/// In batches: the `-eval` script lists every module, and one call for the
/// whole corpus approaches the Windows ~32K command-line limit (BT-4224).
/// Batches of 128 keep every invocation to a few kilobytes on every
/// platform.
fn run_on_beam(dir: &Path, modules: &[(String, bool)]) -> HashMap<String, String> {
    const BATCH: usize = 128;
    let mut merged = HashMap::new();
    for chunk in modules.chunks(BATCH) {
        let list = chunk
            .iter()
            .map(|(m, trace)| format!("{{'{m}', {trace}}}"))
            .collect::<Vec<_>>()
            .join(",");
        let script = format!(
            "{{ok, _}} = application:ensure_all_started(beamtalk_stdlib), \
             lists:foreach(fun({{M, Trace}}) -> \
               Out = try {{Trace, M:eval(#{{w => 0, x => 0}})}} of \
                 {{true, {{Steps, B}}}} -> \
                   {{_, V}} = lists:last(Steps), \
                   io_lib:format(\"~w ~w ~w steps=~w\", [V, maps:get(w, B), maps:get(x, B), length(Steps)]); \
                 {{false, {{V, B}}}} -> \
                   io_lib:format(\"~w ~w ~w\", [V, maps:get(w, B), maps:get(x, B)]) \
               catch C:E:St -> io_lib:format(\"error ~w:~0p ~0p\", [C, E, hd(St)]) end, \
               io:format(\"~s => ~s~n\", [M, Out]) \
             end, [{list}]), halt().",
        );
        let mut cmd = Command::new("erl");
        // The node runs in `dir`, so a crash dump never lands in the source tree.
        cmd.current_dir(dir).arg("-noshell").arg("-pa").arg(dir);
        for ebin in runtime_ebins() {
            cmd.arg("-pa").arg(ebin);
        }
        let output = cmd.arg("-eval").arg(script).output().expect("run erl");
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            output.status.success(),
            "erl failed:\n{stdout}{}",
            String::from_utf8_lossy(&output.stderr)
        );
        merged.extend(
            stdout
                .lines()
                .filter_map(|l| l.split_once(" => "))
                .map(|(m, out)| (m.to_string(), out.to_string())),
        );
    }
    merged
}

/// Whether the `erl` output `out` of a module agrees with `expected`.
fn agrees(out: &str, expected: &Expected, trace: bool) -> bool {
    let mut parts = out.split(' ');
    let (Some(value), Some(w), Some(x)) = (parts.next(), parts.next(), parts.next()) else {
        return false;
    };
    let value_ok = expected.value.is_none_or(|v| value == v.to_string());
    let w_ok = w == expected.w.to_string();
    let x_ok = x == expected.x.unwrap_or(0).to_string();
    // A `^` ends a traced input with one step: the input and the `^`'s value.
    let steps_ok = !(trace && expected.fired) || parts.next() == Some("steps=1");
    value_ok && w_ok && x_ok && steps_ok
}

/// Proofs 1 and 2: the corpus compiles through the verifier and `erlc`, and
/// without its boundary every row fails `LoopReturnWithoutBoundary`.
#[test]
fn repl_loop_body_return_corpus_compiles_and_needs_its_boundary() {
    let rows = rows();
    let mut failures = Vec::new();
    let mut modules = Vec::new();
    for (i, row) in rows.iter().enumerate() {
        for trace in [false, true] {
            let module = format!("bt4288_c{}_{i}", u8::from(trace));
            match generate(&row.source, &module, trace) {
                Ok(code) => modules.push((module, code)),
                Err(e) => failures.push(format!("{} (trace: {trace}): {e}", row.name)),
            }
        }
        let report = without_boundary(|| verifier_report(&row.source));
        if !report.contains("LoopReturnWithoutBoundary") {
            failures.push(format!(
                "{}: without the boundary, expected LoopReturnWithoutBoundary, got {report}",
                row.name
            ));
        }
    }
    if erlang_available() {
        let dir = beamtalk_core::test_helpers::unique_temp_dir("bt4288_compile");
        std::fs::create_dir_all(&dir).expect("temp dir");
        if let Err(e) = erlc(&dir, &modules) {
            failures.push(format!("erlc: {e}"));
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(
        failures.is_empty(),
        "{} of {} REPL loop-body `^` row(s) fail:\n\n{}",
        failures.len(),
        rows.len(),
        failures.join("\n\n")
    );
}

/// Proof 3: every row answers, in eval and trace mode, what the method
/// semantics says.
#[test]
fn repl_loop_body_returns_answer_like_a_method() {
    if !erlang_available() {
        eprintln!("erl/erlc not installed: skipping the BEAM run");
        return;
    }
    let rows = rows();
    assert!(
        rows.len() >= 220,
        "the corpus has thinned out: {}",
        rows.len()
    );
    assert!(
        rows.iter().filter(|r| r.expected.fired).count() >= 190,
        "too few rows fire their `^`"
    );
    let mut modules = Vec::new();
    for (i, row) in rows.iter().enumerate() {
        for trace in [false, true] {
            let module = format!("bt4288_r{}_{i}", u8::from(trace));
            let code = generate(&row.source, &module, trace)
                .unwrap_or_else(|e| panic!("{}: {e}", row.name));
            modules.push((module, code));
        }
    }
    let dir = beamtalk_core::test_helpers::unique_temp_dir("bt4288_run");
    std::fs::create_dir_all(&dir).expect("temp dir");
    erlc(&dir, &modules).unwrap_or_else(|e| panic!("erlc: {e}"));
    let names: Vec<(String, bool)> = modules
        .iter()
        .map(|(m, _)| (m.clone(), m.starts_with("bt4288_r1_")))
        .collect();
    let outputs = run_on_beam(&dir, &names);
    let _ = std::fs::remove_dir_all(&dir);
    let mut failures = Vec::new();
    for (i, row) in rows.iter().enumerate() {
        for trace in [false, true] {
            let module = format!("bt4288_r{}_{i}", u8::from(trace));
            let out = outputs.get(&module).map_or("<no output>", String::as_str);
            if !agrees(out, &row.expected, trace) {
                failures.push(format!(
                    "{} (trace: {trace}):\n{}\n  expected {:?}, got {out}",
                    row.name, row.source, row.expected
                ));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} REPL loop-body `^` run(s) disagree with the method semantics:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// A `^` whose loop is held in a closure never throws to the REPL eval's
/// boundary: the closure may run in a later input, after the eval that created
/// it has ended, where nothing catches the throw. As on main, such a loop is
/// rejected (BT-4003); a closure created in a loop body and holding a `^` of
/// its own is accepted and answers its value where it stands, so its code
/// holds no non-local-return throw at all. The repl-protocol case
/// `bt4003_repl_returns.btscript` calls such a closure from a later input.
#[test]
fn a_return_in_a_loop_held_by_a_closure_never_throws_to_the_eval() {
    for src in [
        "b := [#(1, 2) do: [:x | ^x]]",
        "Holder keep: [#(1, 2) do: [:x | ^x]]",
        "true ifTrue: [b := [#(1, 2) do: [:x | ^x]]]",
        "#(1, 2) do: [:x | b := [#(3) do: [:y | ^y]]]",
    ] {
        match parse_input(src) {
            Err(e) if e.contains("loop would ignore it") => {}
            other => panic!("{src}: expected the BT-4003 rejection, got {other:?}"),
        }
    }
    for src in [
        "#(1, 2) do: [:z | b := [:y | ^y * z]]",
        "w := 0\n#(1, 2) do: [:x | w := w + x. b := [:y | ^y + 1]]",
        "#(1, 2) collect: [:x | [:y | ^y]]",
    ] {
        let code = generate(src, "bt4288_closure", false).unwrap_or_else(|e| panic!("{src}: {e}"));
        assert!(
            !code.contains("'$bt_nlr'"),
            "{src}: a closure's `^` compiled to a throw:\n{code}"
        );
    }
}

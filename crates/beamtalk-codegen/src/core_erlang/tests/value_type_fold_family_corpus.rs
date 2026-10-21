// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3827: a value-type `do:` / `collect:` / `inject:into:` whose body writes a field carries
//! `Self` through the fold accumulator (`FoldFamilySlots`). Two proofs over generated corpora,
//! not the repro:
//!
//! 1. **The verifier invariant.** `FoldFamilyNotCarried`: a fold lambda body that advances `Self`
//!    must end in the `Return` that hands the chain's last version out through the accumulator.
//!    The corpus is every fold-writing position of `field_assignment_value_position.rs` x every
//!    statement-level shape; each compiles unbroken with a silent verifier, and with the lambda's
//!    `Return` deliberately dropped ([`LocalRebindFault::DropFoldFamilySlot`]) the verifier must
//!    report it. (Before the lowering threaded the family the same body was a structured
//!    rejection, so there was no output to check; dropping the `Return` reproduces what lifting
//!    that rejection alone would have emitted.)
//! 2. **Execution against an oracle.** [`oracle_programs`] generates fold bodies from a small
//!    statement pool (field writes reading fields and the element, an outer local, a plain
//!    expression), in every fold kind and three value positions. The module is compiled with the
//!    debug verifier on, through `erlc`, and each probe is *run*; its observed `(m, n, t, value)`
//!    must equal what a Rust interpreter computes. A write the lowering drops, or applies in the
//!    wrong order, fails here although it compiles.
//! 3. **Conditional arms carry the fold's `Self` out (BT-3844, BT-3854, BT-3912).** The same
//!    generated fold bodies, placed in a conditional arm — plain or local-threading,
//!    `ifTrue:` / `ifFalse:` / `ifTrue:ifFalse:` (either or both arms), as an arm statement, an
//!    arm assignment or the arm's value, with the conditional as a statement, the method's last
//!    statement or an assigned value ([`arm_programs`]). The arm is a frame whose fold is a
//!    producer, so every program compiles with a silent verifier and, run on a BEAM, agrees with
//!    an oracle (taken arm: the fold's state; other arm / passthrough: `START`).

use super::field_assignment_value_position::{
    VT_FOLD_REJECTING_SHAPES, VT_HEADER, VT_POSITIONS, VT_SHAPES, is_fold_writing_position,
};
use super::{codegen, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;
use crate::core_erlang::{CodegenOptions, generate_module_surfacing_verifier};

/// `(case name, program)` for every statement-level shape x every fold-writing position.
fn verifier_programs() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (shape_name, shape, may_reject) in VT_SHAPES {
        if *may_reject || VT_FOLD_REJECTING_SHAPES.contains(shape_name) {
            continue;
        }
        for (i, (position_name, position)) in VT_POSITIONS.iter().enumerate() {
            if !is_fold_writing_position(position) {
                continue;
            }
            let stmt = position.replace("EXPR", "(self.n := 5)");
            out.push((
                format!("{shape_name} / {position_name}"),
                format!(
                    "{VT_HEADER}\n  probe{i} =>\n{}",
                    shape.replace("STMT", &stmt)
                ),
            ));
        }
    }
    out
}

#[test]
fn the_verifier_corpus_covers_every_fold_selector_in_every_statement_shape() {
    let positions: Vec<&str> = VT_POSITIONS
        .iter()
        .filter(|(_, template)| is_fold_writing_position(template))
        .map(|(name, _)| *name)
        .collect();
    let shapes = VT_SHAPES
        .iter()
        .filter(|(name, _, may_reject)| !may_reject && !VT_FOLD_REJECTING_SHAPES.contains(name))
        .count();
    for selector in ["do:", "collect:", "inject:"] {
        assert!(
            positions
                .iter()
                .any(|name| name.starts_with(&selector[..selector.len() - 1])),
            "no fold-writing position exercises `{selector}`: {positions:?}"
        );
    }
    assert!(
        positions.len() >= 10 && shapes >= 3,
        "{} positions x {shapes} shapes: the corpus has thinned out",
        positions.len()
    );
    assert_eq!(verifier_programs().len(), positions.len() * shapes);
}

#[test]
fn every_unbroken_field_writing_fold_compiles_with_a_silent_verifier() {
    let failures: Vec<String> = verifier_programs()
        .into_iter()
        .filter_map(|(name, src)| {
            verifier_report(&src, None).map(|report| format!("{name}: {report}\n{src}"))
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

#[test]
fn a_fold_lambda_that_drops_its_self_return_is_reported_for_every_program() {
    let mut unreported = Vec::new();
    for (name, src) in verifier_programs() {
        match verifier_report(&src, Some(LocalRebindFault::DropFoldFamilySlot)) {
            Some(report) if report.contains("FoldFamilyNotCarried") => {}
            Some(report) => unreported.push(format!("{name}: wrong report: {report}\n{src}")),
            None => unreported.push(format!("{name}: compiled without a report\n{src}")),
        }
    }
    assert!(unreported.is_empty(), "{}", unreported.join("\n\n"));
}

// ── Execution against an oracle ─────────────────────────────────────────────────────────────

/// One statement of a generated fold body.
#[derive(Clone, Copy, Debug)]
enum Stmt {
    /// `self.m := self.m + e`
    AddToM,
    /// `self.n := e`
    SetNToE,
    /// `self.m := self.n + e`
    SetMFromN,
    /// `t := t + e` (an outer local the fold also threads)
    AddToT,
    /// `self.m + e` (a plain expression, no write)
    ReadM,
}

const POOL: [Stmt; 5] = [
    Stmt::AddToM,
    Stmt::SetNToE,
    Stmt::SetMFromN,
    Stmt::AddToT,
    Stmt::ReadM,
];

impl Stmt {
    fn source(self) -> &'static str {
        match self {
            Self::AddToM => "self.m := self.m + e",
            Self::SetNToE => "self.n := e",
            Self::SetMFromN => "self.m := self.n + e",
            Self::AddToT => "t := t + e",
            Self::ReadM => "self.m + e",
        }
    }

    fn writes_a_field(self) -> bool {
        matches!(self, Self::AddToM | Self::SetNToE | Self::SetMFromN)
    }

    /// Runs the statement on `state`, answering the statement's value.
    fn run(self, e: i64, state: &mut State) -> i64 {
        match self {
            Self::AddToM => {
                state.m += e;
                state.m
            }
            Self::SetNToE => {
                state.n = e;
                e
            }
            Self::SetMFromN => {
                state.m = state.n + e;
                state.m
            }
            Self::AddToT => {
                state.t += e;
                state.t
            }
            Self::ReadM => state.m + e,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct State {
    m: i64,
    n: i64,
    t: i64,
}

#[derive(Clone, Copy, Debug)]
enum Kind {
    Do,
    Collect,
    Inject,
}

const ELEMENTS: [i64; 3] = [1, 2, 3];
const START: State = State { m: 7, n: 20, t: 0 };

impl Kind {
    fn source(self, body: &[Stmt]) -> String {
        let stmts: Vec<&str> = body.iter().map(|s| s.source()).collect();
        match self {
            Self::Do => format!("#(1, 2, 3) do: [:e | {}]", stmts.join(". ")),
            Self::Collect => format!("#(1, 2, 3) collect: [:e | {}]", stmts.join(". ")),
            // The accumulator is the last statement's value plus the element, so the fold's
            // answer depends on every statement having run in order.
            Self::Inject => format!(
                "#(1, 2, 3) inject: 0 into: [:a :e | {}. a + e]",
                stmts.join(". ")
            ),
        }
    }

    /// The Erlang term the fold answers, as `~w` prints it, and the final state.
    fn oracle(self, body: &[Stmt]) -> (String, State) {
        let mut state = START;
        let mut collected: Vec<i64> = Vec::new();
        let mut acc = 0;
        for e in ELEMENTS {
            let mut last = 0;
            for stmt in body {
                last = stmt.run(e, &mut state);
            }
            match self {
                Self::Do => {}
                Self::Collect => collected.push(last),
                Self::Inject => acc += e,
            }
        }
        let value = match self {
            Self::Do => "nil".to_string(),
            Self::Collect => format!("{collected:?}").replace(' ', ""),
            Self::Inject => acc.to_string(),
        };
        (value, state)
    }
}

#[derive(Clone, Copy, Debug)]
enum Position {
    /// `FOLD.` as a statement, then `r := nil`.
    Statement,
    /// `r := FOLD.`
    Assigned,
    /// `r := #(1, FOLD).`
    ListElement,
}

impl Position {
    const ALL: [Self; 3] = [Self::Statement, Self::Assigned, Self::ListElement];

    fn wrap(self, fold: &str) -> String {
        match self {
            Self::Statement => format!("{fold}.\n    r := nil"),
            Self::Assigned => format!("r := {fold}"),
            Self::ListElement => format!("r := #(1, {fold})"),
        }
    }

    fn value(self, fold_value: &str) -> String {
        match self {
            Self::ListElement => format!("[1,{fold_value}]"),
            Self::Statement => "nil".to_string(),
            Self::Assigned => fold_value.to_string(),
        }
    }
}

/// Every body of one or two pool statements with at least one field write, plus three longer
/// hand-picked ones that interleave reads, writes and the outer local.
fn bodies() -> Vec<Vec<Stmt>> {
    let mut out: Vec<Vec<Stmt>> = Vec::new();
    for a in POOL {
        out.push(vec![a]);
        for b in POOL {
            out.push(vec![a, b]);
        }
    }
    out.push(vec![Stmt::AddToM, Stmt::AddToT, Stmt::SetNToE, Stmt::ReadM]);
    out.push(vec![Stmt::SetNToE, Stmt::SetMFromN, Stmt::AddToM]);
    out.push(vec![
        Stmt::AddToT,
        Stmt::AddToM,
        Stmt::AddToT,
        Stmt::SetMFromN,
    ]);
    out.retain(|body| body.iter().any(|s| s.writes_a_field()));
    out
}

/// `(probe name, fold source, expected observation)` for every generated program. The
/// observation is the method's answer `#(m, n, t, r)` printed with `~w`.
fn oracle_programs() -> Vec<(String, String, String)> {
    let mut out = Vec::new();
    for kind in [Kind::Do, Kind::Collect, Kind::Inject] {
        for body in bodies() {
            for position in Position::ALL {
                let name = format!("probe{}", out.len());
                let fold = kind.source(&body);
                let method = format!(
                    "  {name} =>\n    t := 0\n    {}\n    #(self.m, self.n, t, r)\n",
                    position.wrap(&fold)
                );
                let (value, end) = kind.oracle(&body);
                let expected =
                    format!("[{},{},{},{}]", end.m, end.n, end.t, position.value(&value));
                out.push((name, method, expected));
            }
        }
    }
    out
}

/// Compiles `core` with `erlc`, runs every probe of `probes` against the `Self` map
/// `self_map` (an Erlang map expression, e.g. `#{m => 7, n => 20}`) and answers
/// `probe name -> printed result`. A probe that does not answer within five seconds (a loop
/// whose exit write was dropped) prints `timeout`. `None` without `erlc`/`erl`.
///
/// Shared with `while_condition_carrier_corpus.rs` (BT-4002).
pub(super) fn run_probes(
    core: &str,
    probes: &[String],
    self_map: &str,
) -> Option<std::collections::HashMap<String, String>> {
    use std::process::Command;
    if !super::erlc_available() || Command::new("erl").arg("-version").output().is_err() {
        return None;
    }
    // The probes need no Beamtalk runtime (arithmetic, `maps`, list literals), but the module's
    // `on_load` hook registers the class with it: drop the hook so the module loads bare.
    let core = &core.replace("'on_load' = [{'register_class', 0}],", "");
    let dir = tempfile::tempdir().expect("temp dir");
    let module = core
        .strip_prefix("module '")
        .and_then(|rest| rest.split('\'').next())
        .expect("generated Core Erlang starts with its module name")
        .to_string();
    let core_file = dir.path().join(format!("{module}.core"));
    std::fs::write(&core_file, core).expect("write core");
    let compiled = Command::new("erlc")
        .arg("+from_core")
        .arg("-o")
        .arg(dir.path())
        .arg(&core_file)
        .output()
        .expect("erlc runs");
    assert!(
        compiled.status.success(),
        "erlc rejected the oracle module:\n{}\n{}",
        String::from_utf8_lossy(&compiled.stderr),
        String::from_utf8_lossy(&compiled.stdout)
    );
    // `collect:` rebuilds its result with the runtime's `from_list_like/2`, the identity on a
    // list receiver (the only receiver the probes use).
    let stub = dir.path().join("beamtalk_collection.erl");
    std::fs::write(
        &stub,
        "-module(beamtalk_collection).\n-export([from_list_like/2]).\nfrom_list_like(_Receiver, List) -> List.\n",
    )
    .expect("write runtime stub");
    let stubbed = Command::new("erlc")
        .arg("-o")
        .arg(dir.path())
        .arg(&stub)
        .output()
        .expect("erlc runs");
    assert!(
        stubbed.status.success(),
        "{}",
        String::from_utf8_lossy(&stubbed.stdout)
    );
    let names = probes
        .iter()
        .map(|p| format!("'{p}'"))
        .collect::<Vec<_>>()
        .join(",");
    let eval = format!(
        "Self = {self_map}, \
         lists:foreach(fun(F) -> \
           Parent = self(), \
           Pid = spawn(fun() -> Parent ! {{self(), (catch '{module}':F(Self))}} end), \
           R = receive {{Pid, V}} -> V after 5000 -> exit(Pid, kill), timeout end, \
           io:format(\"~s ~w~n\", [F, R]) end, [{names}]), \
         halt()."
    );
    let output = Command::new("erl")
        .current_dir(dir.path())
        .arg("-noshell")
        .arg("-pa")
        .arg(dir.path())
        .arg("-eval")
        .arg(eval)
        .output()
        .expect("erl runs");
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(
        output.status.success() && !stdout.is_empty(),
        "the oracle run produced no output (status {}):\n{}\n{stdout}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    Some(
        stdout
            .lines()
            .filter_map(|line| line.split_once(' '))
            .map(|(name, result)| (name.to_string(), result.to_string()))
            .collect(),
    )
}

#[test]
fn generated_field_writing_folds_agree_with_the_oracle_when_run() {
    let programs = oracle_programs();
    assert!(
        programs.len() >= 150,
        "only {} generated programs: the corpus has thinned out",
        programs.len()
    );
    let methods: Vec<&str> = programs.iter().map(|(_, m, _)| m.as_str()).collect();
    let src = format!("{VT_HEADER}\n{}", methods.join("\n"));
    // Debug codegen: the `ThreadedIr` verifier is on and panics on any finding.
    let core = codegen(&src);
    let names: Vec<String> = programs.iter().map(|(n, _, _)| n.clone()).collect();
    let self_map = format!("#{{m => {}, n => {}}}", START.m, START.n);
    let Some(observed) = run_probes(&core, &names, &self_map) else {
        println!("skipping the oracle run: erlc/erl not available");
        return;
    };
    let mut mismatches = Vec::new();
    for (name, method, expected) in &programs {
        match observed.get(name) {
            Some(got) if got == expected => {}
            other => mismatches.push(format!(
                "{name}: expected {expected}, ran to {other:?}\n{method}"
            )),
        }
    }
    assert!(
        mismatches.is_empty(),
        "{} of {} generated fold programs disagree with the oracle:\n\n{}",
        mismatches.len(),
        programs.len(),
        mismatches.join("\n")
    );
}

// ── Conditional arms (BT-3844) ──────────────────────────────────────────────────────────────

/// How a generated fold `FOLD` sits in a conditional arm. `x` is an outer local, so every form but
/// the bare `FOLD` one makes the arm itself a local-threading arm. The bare form is a plain arm
/// only under the selectors whose arms are all `FOLD`: where the other arm writes `x := 5`
/// ([`ARM_SELECTORS`]), that arm puts the whole conditional on the local-threading lowering.
const ARM_FORMS: [&str; 4] = [
    // the fold alone, as the arm's value (no local write of its own)
    "FOLD",
    // local-threading arm, the fold a statement between two local writes
    "x := 1. FOLD. x := x + 1",
    // local-threading arm, the fold assigned to the local
    "x := FOLD. x",
    // local-threading arm, the fold the arm's value
    "x := 1. FOLD",
];

/// The conditional around one or two arms `ARM`.
const ARM_SELECTORS: [&str; 5] = [
    "c ifTrue: [ARM]",
    "c ifFalse: [ARM]",
    "c ifTrue: [ARM] ifFalse: [x := 5]",
    "c ifTrue: [x := 5] ifFalse: [ARM]",
    "c ifTrue: [ARM] ifFalse: [ARM]",
];

/// Where the conditional `COND` sits in the method.
const ARM_POSITIONS: [&str; 3] = [
    // a statement, the fields and locals read after it
    "COND.\n    #(self.m, self.n, t, x)",
    // the method's last statement (its value is the method's answer)
    "COND",
    // an assigned value, read with the fields after it
    "r := COND.\n    #(self.m, self.n, t, x, r)",
];

/// `(probe name, method source)` for every generated fold body x fold kind x arm form x selector
/// x conditional position.
fn arm_programs() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for kind in [Kind::Do, Kind::Collect, Kind::Inject] {
        for body in bodies() {
            let fold = kind.source(&body);
            for form in ARM_FORMS {
                let arm = form.replace("FOLD", &fold);
                for selector in ARM_SELECTORS {
                    let conditional = selector.replace("ARM", &arm);
                    for position in ARM_POSITIONS {
                        let name = format!("probe{}", out.len());
                        let method = format!(
                            "  {name} =>\n    t := 0\n    x := 0\n    c := self.n > 10\n    {}\n",
                            position.replace("COND", &conditional)
                        );
                        out.push((name, method));
                    }
                }
            }
        }
    }
    out
}

/// Runs one conditional arm `form` of [`ARM_FORMS`] (the fold `kind` over `body`) against
/// `state` and the outer local `x`, answering the arm's value. The arm runs at most once, from
/// [`START`]: the receiver `c` is read before it and nothing else writes first.
fn run_arm(kind: Kind, body: &[Stmt], form: usize, x: &mut String, state: &mut State) -> String {
    let (value, end) = kind.oracle(body);
    match form {
        // the fold alone, as the arm's value
        0 => {
            *state = end;
            value
        }
        // `x := 1. FOLD. x := x + 1`
        1 => {
            *state = end;
            *x = "2".to_string();
            "2".to_string()
        }
        // `x := FOLD. x`
        2 => {
            *state = end;
            *x = value.clone();
            value
        }
        // `x := 1. FOLD`
        _ => {
            *state = end;
            *x = "1".to_string();
            value
        }
    }
}

/// `(probe name, method source, expected observation)` for every generated conditional-arm
/// program (the same loops as [`arm_programs`]): the taken arm's effects are the fold's final
/// state and `x`; the other arm and a passthrough leave [`START`] and answer `nil`.
fn arm_oracle_programs() -> Vec<(String, String, String)> {
    let programs = arm_programs();
    let mut out = Vec::with_capacity(programs.len());
    let mut index = 0;
    for kind in [Kind::Do, Kind::Collect, Kind::Inject] {
        for body in bodies() {
            for form in 0..ARM_FORMS.len() {
                for selector in 0..ARM_SELECTORS.len() {
                    for position in 0..ARM_POSITIONS.len() {
                        let (name, method) = &programs[index];
                        index += 1;
                        let mut state = START;
                        let mut x = "0".to_string();
                        // `c := self.n > 10` is read before any arm runs.
                        let c = state.n > 10;
                        let arm = |x: &mut String, state: &mut State| {
                            run_arm(kind, &body, form, x, state)
                        };
                        let value = match selector {
                            // c ifTrue: [ARM]
                            0 => {
                                if c {
                                    arm(&mut x, &mut state)
                                } else {
                                    "nil".to_string()
                                }
                            }
                            // c ifFalse: [ARM]
                            1 => {
                                if c {
                                    "nil".to_string()
                                } else {
                                    arm(&mut x, &mut state)
                                }
                            }
                            // c ifTrue: [ARM] ifFalse: [x := 5]
                            2 => {
                                if c {
                                    arm(&mut x, &mut state)
                                } else {
                                    x = "5".to_string();
                                    "5".to_string()
                                }
                            }
                            // c ifTrue: [x := 5] ifFalse: [ARM]
                            3 => {
                                if c {
                                    x = "5".to_string();
                                    "5".to_string()
                                } else {
                                    arm(&mut x, &mut state)
                                }
                            }
                            // c ifTrue: [ARM] ifFalse: [ARM]
                            _ => arm(&mut x, &mut state),
                        };
                        let expected = match position {
                            0 => format!("[{},{},{},{}]", state.m, state.n, state.t, x),
                            1 => value,
                            _ => format!("[{},{},{},{},{}]", state.m, state.n, state.t, x, value),
                        };
                        out.push((name.clone(), method.clone(), expected));
                    }
                }
            }
        }
    }
    assert_eq!(index, programs.len());
    out
}

#[test]
fn generated_field_writing_folds_in_conditional_arms_compile_with_a_silent_verifier() {
    let programs = arm_programs();
    assert!(
        programs.len() >= 1000,
        "only {} generated conditional-arm programs: the corpus has thinned out",
        programs.len()
    );
    let failures: Vec<String> = programs
        .iter()
        .filter_map(|(name, method)| {
            let src = format!("{VT_HEADER}\n{method}");
            let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
            let (module, _) = beamtalk_core::source_analysis::parse(tokens);
            match generate_module_surfacing_verifier(&module, CodegenOptions::new("test")) {
                Ok(_) => None,
                Err(error) => Some(format!("{name}: {error:?}\n{method}")),
            }
        })
        .collect();
    assert!(
        failures.is_empty(),
        "{} of {} conditional-arm fold programs do not compile:\n\n{}",
        failures.len(),
        programs.len(),
        failures.join("\n")
    );
}

#[test]
fn generated_field_writing_folds_in_conditional_arms_agree_with_the_oracle_when_run() {
    let programs = arm_oracle_programs();
    let methods: Vec<&str> = programs.iter().map(|(_, m, _)| m.as_str()).collect();
    let src = format!("{VT_HEADER}\n{}", methods.join("\n"));
    // Debug codegen: the `ThreadedIr` verifier is on and panics on any finding.
    let core = codegen(&src);
    let names: Vec<String> = programs.iter().map(|(n, _, _)| n.clone()).collect();
    let self_map = format!("#{{m => {}, n => {}}}", START.m, START.n);
    let Some(observed) = run_probes(&core, &names, &self_map) else {
        println!("skipping the oracle run: erlc/erl not available");
        return;
    };
    let mut mismatches = Vec::new();
    for (name, method, expected) in &programs {
        match observed.get(name) {
            Some(got) if got == expected => {}
            other => mismatches.push(format!(
                "{name}: expected {expected}, ran to {other:?}\n{method}"
            )),
        }
    }
    assert!(
        mismatches.is_empty(),
        "{} of {} conditional-arm fold programs disagree with the oracle:\n\n{}",
        mismatches.len(),
        programs.len(),
        mismatches
            .iter()
            .take(8)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n\n")
    );
}

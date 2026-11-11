// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4116: a fold that threads only outer locals folds a bare value (one
//! local, `do:`) or an N-tuple, in every codegen context — not a `StateAcc`
//! map. Proved over a generated corpus, not the repro: every fold kind x
//! context x body shape
//!
//! 1. compiles with the `ThreadedIr` verifier on (no panic, no
//!    `FoldAccShapeMismatch` / `TupleAccWithFamily`);
//! 2. builds through `erlc`;
//! 3. answers what the same program answers when built with every fold forced
//!    onto the map path ([`LocalRebindFault::ForceStateAccFold`], the
//!    oracle) — `Object` and `Value` instance methods are executed; actor and
//!    class-method contexts are compiled and `erlc`'d only (they need a
//!    running class/actor process);
//!
//! and the new shape's fault ([`LocalRebindFault::BareFoldTupleRepack`]) is
//! reported as `FoldAccShapeMismatch`. The execution half is skipped (unless `BEAMTALK_REQUIRE_RUNTIME` is set, as in CI) when the
//! Erlang runtime has not been built (`runtime/_build`).

use super::{assert_parses, codegen_named, verifier_report};
use crate::core_erlang::threaded_expr::fault::{self, LocalRebindFault};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Folds writing the outer locals named by `BODY`; `W` is a placeholder for
/// the written statement(s).
const FOLDS: [&str; 5] = [
    "#(1, 2, 3) do: [:e | W]",
    "#(1, 2, 3) collect: [:e | W. e]",
    "#{#a => 1, #b => 2} do: [:e | W]",
    "#{#a => 1, #b => 2} keysAndValuesDo: [:key :e | W]",
    "#(1, 2, 3) inject: 0 into: [:a :e | W. a + e]",
];

/// Body statements: one local, two locals, a field read, `on:do:` inside, an
/// escaping block capturing the local.
const BODIES: [&str; 9] = [
    "s := s + e",
    "s := s + e. t := t + 1",
    "s := s + FIELD",
    "s := s + e. #(4) do: [:q | s := s + q]",
    "[s := s + e] on: Error do: [:x | s := 0]",
    "s := s + e. blk := [:z | z + s]",
    // Non-local `^` (early return from the enclosing method) out of the fold.
    "s := s + e. e =:= 2 ifTrue: [^s]",
    "s := s + e. t := t + 1. e =:= 2 ifTrue: [^s + t]",
    "e =:= 3 ifTrue: [^s]. s := s + e",
];

/// Method contexts: (header, whether the program can be executed here).
const CONTEXTS: [(&str, &str, bool); 5] = [
    ("Object subclass: Probe\n  state: n = 7\n\n", "run =>", true),
    ("Value subclass: Probe\n  state: n = 7\n\n", "run =>", true),
    ("Actor subclass: Probe\n  state: n = 7\n\n", "run =>", false),
    (
        "Object subclass: Probe\n  state: n = 7\n\n",
        "class run =>",
        false,
    ),
    ("Value subclass: Probe\n  state: n = 7\n\n", "run =>", true),
];

fn programs() -> Vec<(usize, String)> {
    let mut out = Vec::new();
    for (c, (header, sig, _)) in CONTEXTS.iter().enumerate() {
        for fold in FOLDS {
            for body in BODIES {
                let field = if sig.starts_with("class") {
                    "7"
                } else {
                    "self.n"
                };
                let stmt = fold.replace('W', &body.replace("FIELD", field));
                let nested = if c == 4 {
                    // A fold nested in a fold body (value-type context).
                    format!("#(1, 2) do: [:m | {stmt}. s := s + m]")
                } else {
                    stmt
                };
                out.push((
                    c,
                    format!(
                        "{header}  {sig}\n    s := 0\n    t := 0\n    blk := nil\n    {nested}\n    s + t\n"
                    ),
                ));
            }
        }
    }
    for (_, src) in &out {
        assert_parses(src);
    }
    out
}

fn runtime_ebins() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../runtime/_build/default/lib");
    let Ok(rd) = std::fs::read_dir(&root) else {
        return Vec::new();
    };
    let mut ebins: Vec<PathBuf> = rd
        .flatten()
        .map(|e| e.path().join("ebin"))
        .filter(|p| p.is_dir())
        .collect();
    // The compiled stdlib classes (`bt@stdlib@dictionary`, ...) live here.
    let stdlib =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../../runtime/apps/beamtalk_stdlib/ebin");
    if stdlib.is_dir() {
        ebins.push(stdlib);
    }
    ebins
}

/// One `erlc` run over every written `.core` file (spawning it per file
/// dominates the test otherwise).
fn erlc_all(dir: &Path) -> Result<(), String> {
    let mut cores: Vec<PathBuf> = std::fs::read_dir(dir)
        .map_err(|e| e.to_string())?
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|x| x == "core"))
        .collect();
    cores.sort();
    let out = Command::new("erlc")
        .args(["+from_core", "-o"])
        .arg(dir)
        .args(&cores)
        .output()
        .map_err(|e| e.to_string())?;
    if out.status.success() {
        Ok(())
    } else {
        Err(String::from_utf8_lossy(&out.stdout).to_string()
            + &String::from_utf8_lossy(&out.stderr))
    }
}

#[test]
fn bare_and_tuple_folds_compile_clean_in_every_context() {
    let failures: Vec<String> = programs()
        .into_iter()
        .filter_map(|(_, src)| verifier_report(&src, None).map(|r| format!("{r}\n{src}")))
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n---\n"));
}

/// BT-4641: a `^` out of a fold must answer its value directly, not just
/// agree with the oracle (a failure shared by both builds would agree).
/// Writes `bt4116_pin` (production codegen) and `bt4116_pino` (oracle).
fn write_pinned_modules(dir: &Path) {
    let pinned = "Object subclass: Probe\n  state: n = 7\n\n  run =>\n    s := 0\n    t := 0\n    blk := nil\n    #(1, 2, 3) do: [:e | s := s + e. e =:= 2 ifTrue: [^s]]\n    s + t\n";
    assert_parses(pinned);
    std::fs::write(
        dir.join("bt4116_pin.core"),
        codegen_named(pinned, "bt4116_pin"),
    )
    .unwrap();
    std::fs::write(
        dir.join("bt4116_pino.core"),
        fault::with(LocalRebindFault::ForceStateAccFold, || {
            codegen_named(pinned, "bt4116_pino")
        }),
    )
    .unwrap();
}

#[test]
fn bare_and_tuple_folds_agree_with_the_map_oracle() {
    // BT-4641: CI sets BEAMTALK_REQUIRE_RUNTIME so a missing toolchain or
    // runtime turns this test red instead of silently degrading it.
    let require = std::env::var_os("BEAMTALK_REQUIRE_RUNTIME").is_some();
    if Command::new("erlc").arg("-v").output().is_err() {
        assert!(!require, "BEAMTALK_REQUIRE_RUNTIME set but erlc is missing");
        eprintln!("erlc not available; skipping");
        return;
    }
    let ebins = runtime_ebins();
    let dir = std::env::temp_dir().join(format!("bt4116-agree-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let mut exec: Vec<(String, String)> = Vec::new();
    let mut failures = Vec::new();
    for (i, (c, src)) in programs().into_iter().enumerate() {
        let new = format!("bt4116_n{i}");
        let old = format!("bt4116_o{i}");
        let Ok(new_core) = std::panic::catch_unwind(|| codegen_named(&src, &new)).map_err(|_| ())
        else {
            failures.push(format!("codegen panicked:\n{src}"));
            continue;
        };
        let old_core = fault::with(LocalRebindFault::ForceStateAccFold, || {
            codegen_named(&src, &old)
        });
        for (name, core) in [(&new, &new_core), (&old, &old_core)] {
            std::fs::write(dir.join(format!("{name}.core")), core).unwrap();
        }
        if CONTEXTS[c].2 {
            exec.push((new, old));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n---\n"));
    write_pinned_modules(&dir);
    if let Err(e) = erlc_all(&dir) {
        panic!("erlc rejected a generated module:\n{e}");
    }
    if ebins.is_empty() {
        assert!(
            !require,
            "BEAMTALK_REQUIRE_RUNTIME set but the runtime ebins are missing"
        );
        eprintln!("runtime not built; compile-only");
        return;
    }
    let mut script = String::from(
        "{ok, _} = application:ensure_all_started(beamtalk_stdlib),\nRun = fun(M) -> try {ok, M:run(M:new())} catch C:R -> {caught, C, element(1, case R of {E, _} -> {E}; E -> {E} end)} end end,\n",
    );
    for (n, o) in &exec {
        let _ = writeln!(
            script,
            "case Run({n}) =:= Run({o}) of true -> ok; false -> io:format(\"DISAGREE ~p ~p ~p~n\", [{n}, Run({n}), Run({o})]) end,"
        );
    }
    // Guard against a vacuous agreement (both builds failing alike): most
    // programs must actually answer a value.
    let list = exec
        .iter()
        .map(|(n, _)| n.as_str())
        .collect::<Vec<_>>()
        .join(", ");
    script.push_str(
        &"Ok = length([M || M <- [LIST], element(1, Run(M)) =/= caught]),\n\
          io:format(\"RAN ~p~n\", [Ok]),\n\
          [case Run(M) of {caught, _, undef} -> io:format(\"BROKEN ~p undef~n\", [M]); {caught, _, #{error := Err}} when is_tuple(Err), element(2, Err) =:= class_not_found -> io:format(\"BROKEN ~p class_not_found~n\", [M]); _ -> ok end || M <- [LIST]],\n"
            .replace("LIST", &list),
    );
    script.push_str(
        "case Run(bt4116_pin) of {ok, 3} -> ok; Other -> io:format(\"PINFAIL ~p old=~p~n\", [Other, Run(bt4116_pino)]) end,\n",
    );
    script.push_str("io:format(\"DONE~n\"), halt().");
    let mut cmd = Command::new("erl");
    cmd.arg("-noshell").arg("-pa").arg(&dir);
    for e in &ebins {
        cmd.arg("-pa").arg(e);
    }
    let out = cmd.arg("-eval").arg(script).output().expect("erl runs");
    let text = String::from_utf8_lossy(&out.stdout).to_string();
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        text.contains("DONE"),
        "erl did not finish:\n{text}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(!text.contains("DISAGREE"), "{text}");
    assert!(!text.contains("PINFAIL"), "{text}");
    // Both builds failing alike (a missing module or class) would agree
    // vacuously; the runtime must be complete enough to run every program.
    assert!(!text.contains("BROKEN"), "{text}");
    let ran: usize = text
        .lines()
        .find_map(|l| l.strip_prefix("RAN ")?.trim().parse().ok())
        .expect("RAN line");
    assert!(
        ran * 2 >= exec.len(),
        "only {ran}/{} programs answered:\n{text}",
        exec.len()
    );
}

#[test]
fn a_bare_fold_repacked_as_a_tuple_is_reported() {
    let src = "Object subclass: Probe\n  run =>\n    s := 0.\n    #(1, 2) do: [:e | s := s + e].\n    s\n";
    assert_eq!(verifier_report(src, None), None);
    let report = verifier_report(src, Some(LocalRebindFault::BareFoldTupleRepack))
        .expect("the shape fault must be reported");
    assert!(report.contains("FoldAccShapeMismatch"), "{report}");
}

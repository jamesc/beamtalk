// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0130 Phase 1 (BT-3705): the codegen half of the class-variable
//! agreement property.
//!
//! `beamtalk-core`'s `test_helpers::class_var_program` generates class-method
//! programs over class variables (loops, `on:do:`/`ensure:`/`tryDo:`, stored
//! closures, writing and late-bound self-sends). This file checks, in
//! process and without a BEAM, what every program must satisfy before it can
//! be executed:
//!
//! 1. its rendering parses with no diagnostic (the generator emits valid
//!    Beamtalk, so a parse error is a generator bug);
//! 2. debug codegen with the `ThreadedIr` verifier on neither panics (a debug
//!    build's `debug_assert!`) nor records an `internal:` diagnostic (a
//!    release build's degraded form of the same failure -- ADR 0130 Open
//!    Question 2, "verifier visibility in release builds": the property
//!    treats either as a failure);
//! 3. the Core Erlang is structurally valid;
//! 4. every compiled `on:do:` is a class-variable catch boundary (ADR 0130
//!    §4, BT-3711): a `snapshot/0` immediately before its `try`, and in its
//!    catch the two `$bt_nlr` pass-through arms, then
//!    `restore/1` as the first statement of the non-NLR arm. The in-process
//!    `ThreadedIr` verifier (`CatchWithoutClassVarRestore`) already checks the
//!    node; this re-checks the emitted text of every generated program, so a
//!    renderer that dropped the order would fail here too.
//!
//! The execution half (compile with `erlc`, run on a BEAM, compare the three
//! spellings with the reference interpretation) is
//! `beamtalk-cli/tests/cli/cli_class_var_agreement.rs`; it needs the runtime,
//! this one does not.
//!
//! The codegen property draws [`Shapes::all()`], `local_touch` included
//! (BT-3767): `local_touch`'s failures (BT-3738) show only when the program is
//! executed, and in-process codegen passes for it, so only the BEAM property
//! leaves it out.
//!
//! # Budget (BT-3767)
//!
//! `all_shapes_pass_verified_codegen` runs [`DEFAULT_CODEGEN_CASES`] cases in a
//! plain `cargo test` (every `just test-rust`, on three OSes per PR); setting
//! `PROPTEST_CASES` overrides it. `just test-class-var-corpus` runs the full
//! 512 cases and the nightly `class-var-corpus` job (`fuzz.yml`) 2048.
//! `programs_interpret_and_render_deterministically` never compiles anything
//! and keeps the shared 512-case default. Measured on a Linux dev container
//! (debug build, this file's tests run in parallel): with 512 codegen cases
//! the file took about 25 s, with 64 about 3 s.
//!
//! Only the open and sealed spellings are checked here: the override
//! spelling is two classes in two files, and a class method's lowering
//! depends on the hierarchy the CLI's multi-file pipeline supplies.

use beamtalk_codegen::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_core::test_helpers::class_var_program::{
    Program, Shapes, Spelling, corpus_cases_from_env, corpus_shapes_from_env, normalize_cause,
};
use beamtalk_core::test_helpers::test_support::{
    arb_class_program, core_erlang_structural_issues, draw_class_programs, proptest_config_cases,
    proptest_config_default,
};
use proptest::prelude::*;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// Checks one class's source; `Err` names the first violated property.
fn check_source(name: &str, source: &str) -> Result<(), String> {
    let (module, diagnostics) = parse(lex_with_eof(source));
    if let Some(d) = diagnostics.iter().find(|d| d.severity == Severity::Error) {
        return Err(format!(
            "generator emitted unparseable source: {}",
            d.message
        ));
    }
    let generated = catch_unwind(AssertUnwindSafe(|| {
        generate_module_with_warnings(&module, CodegenOptions::new(name))
    }));
    match generated {
        Err(panic) => {
            let msg = panic
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| panic.downcast_ref::<&str>().map(ToString::to_string))
                .unwrap_or_default();
            Err(format!("codegen panicked (debug verifier): {msg}"))
        }
        // A rejected program (the compiler's own diagnostic for a shape it
        // cannot thread today) is a codegen error, not a verifier failure,
        // but it is still a program the property must be able to run.
        Ok(Err(e)) => Err(format!("codegen rejected the program: {e}")),
        Ok(Ok(out)) => {
            if let Some(w) = out
                .warnings
                .iter()
                .find(|w| w.message.starts_with("internal:"))
            {
                return Err(format!("verifier diagnostic surfaced: {}", w.message));
            }
            let issues = core_erlang_structural_issues(&out.code);
            if !issues.is_empty() {
                return Err(format!("invalid Core Erlang: {}", issues.join("; ")));
            }
            catch_boundary_issues(&out.code)
                .map_or(Ok(()), |issue| Err(format!("catch boundary: {issue}")))
        }
    }
}

/// ADR 0130 §4: the first violation of the `on:do:` catch-boundary order in
/// `code`, or `None`. Each compiled `on:do:` catch is anchored by its
/// `build_stacktrace` wrap (only `on:do:` emits it).
fn catch_boundary_issues(code: &str) -> Option<String> {
    let anchors: Vec<usize> = code
        .match_indices("primop 'build_stacktrace'(")
        .map(|(i, _)| i)
        .collect();
    let snapshots = code
        .matches("call 'beamtalk_class_vars':'snapshot'() in try")
        .count();
    if snapshots != anchors.len() {
        return Some(format!(
            "{} on:do: catches but {snapshots} snapshots before a try",
            anchors.len()
        ));
    }
    for anchor in anchors {
        let Some(catch_at) = code[..anchor].rfind("catch <") else {
            return Some("an on:do: wrap with no preceding catch".to_string());
        };
        let region = &code[catch_at..anchor];
        let nlr_arms: Vec<usize> = region
            .match_indices("{'$bt_nlr', ")
            .map(|(i, _)| i)
            .collect();
        let restore = region.find("do call 'beamtalk_class_vars':'restore'(");
        match (nlr_arms.as_slice(), restore) {
            ([_, second], Some(restore)) if *second < restore => {}
            _ => {
                return Some(format!(
                    "the restore is not first after both NLR arms: {region}"
                ));
            }
        }
    }
    None
}

#[test]
fn catch_boundary_check_reports_malformed_text() {
    let ok = "let S = call 'beamtalk_class_vars':'snapshot'() in try X catch <T, E, K> -> \
        case {T, E} of <{'throw', {'$bt_nlr', A, B, C}}> when 'true' -> r \
        <{'throw', {'$bt_nlr', A, B}}> when 'true' -> r \
        <O> when 'true' -> do call 'beamtalk_class_vars':'restore'(S) \
        let U = primop 'build_stacktrace'(K) in U";
    assert_eq!(catch_boundary_issues(ok), None);
    // A wrap with no catch before it must be reported, not read as clean.
    let no_catch = "let S = call 'beamtalk_class_vars':'snapshot'() in try X \
        let U = primop 'build_stacktrace'(K) in U";
    assert!(catch_boundary_issues(no_catch).is_some_and(|m| m.contains("no preceding catch")));
    // Restore ordered before the second NLR arm.
    let restore_first = ok.replace("<{'throw', {'$bt_nlr', A, B}}> when 'true' -> r <O>", "<O>");
    assert!(catch_boundary_issues(&restore_first).is_some());
    // No snapshot before the try.
    let no_snapshot = ok.replace("let S = call 'beamtalk_class_vars':'snapshot'() in ", "");
    assert!(catch_boundary_issues(&no_snapshot).is_some());
}

fn check_program(index: usize, program: &Program) -> Result<(), String> {
    for spelling in [Spelling::Open, Spelling::Sealed] {
        for class in program.render(index, spelling) {
            check_source(&class.name, &class.source)
                .map_err(|e| format!("{spelling:?}: {e}\n{}", class.source))?;
        }
    }
    Ok(())
}

proptest! {
    #![proptest_config(proptest_config_default())]

    /// Every generated program, in every shape, answers under the reference
    /// interpreter (the generator only emits programs within its step
    /// budget) and renders deterministically.
    #[test]
    fn programs_interpret_and_render_deterministically(
        (seed, size, program) in arb_class_program(Shapes::all())
    ) {
        prop_assert!(program.interpret().is_some(), "seed {seed} size {size}");
        let again = beamtalk_core::test_helpers::class_var_program::gen_program(
            seed, size, Shapes::all());
        prop_assert_eq!(&program, &again);
        for spelling in Spelling::ALL {
            prop_assert_eq!(program.render(0, spelling), again.render(0, spelling));
        }
    }

}

/// Cases `all_shapes_pass_verified_codegen` runs in a plain `cargo test`;
/// see the module doc's budget section.
const DEFAULT_CODEGEN_CASES: u32 = 64;

proptest! {
    #![proptest_config(proptest_config_cases(DEFAULT_CODEGEN_CASES))]

    /// Every shape, `local_touch` included (BT-3767), compiles through debug
    /// codegen with the verifier on: no panic, no `internal:` diagnostic,
    /// valid Core Erlang, every `on:do:` a class-variable catch boundary.
    #[test]
    fn all_shapes_pass_verified_codegen(
        (seed, size, program) in arb_class_program(Shapes::all())
    ) {
        if let Err(e) = check_program(0, &program) {
            return Err(TestCaseError::fail(format!("seed {seed} size {size}: {e}")));
        }
    }
}

/// BT-3737: the programs that failed `enabled_shapes_pass_verified_codegen`
/// (now `all_shapes_pass_verified_codegen`) in CI (an inlined `inject:into:`
/// fold fun leaked its state version into the enclosing `StateAcc` loop).
/// Explicit seeds, so no `.proptest-regressions` file is needed.
#[test]
fn bt_3737_ci_seeds_pass_verified_codegen() {
    use beamtalk_core::test_helpers::class_var_program::gen_program;
    for seed in [
        4_288_439_136_652_914_866_u64,
        12_525_934_088_575_929_805,
        15_420_763_691_539_516_991,
    ] {
        let program = gen_program(seed, 3, Shapes::ENABLED);
        if let Err(e) = check_program(0, &program) {
            panic!("seed {seed}: {e}");
        }
    }
}

/// The first line of a failure, with names and numbers dropped, so equal
/// causes group together in [`measure_failure_rate`].
fn cause(error: &str) -> String {
    let first = error
        .lines()
        .find(|l| l.contains("panicked") || l.contains("rejected") || l.contains("diagnostic"))
        .unwrap_or_else(|| error.lines().next().unwrap_or(""));
    normalize_cause(first)
}

/// Measurement, not a check: draws `CV_CORPUS_CASES` programs (default 512)
/// of `CV_CORPUS_SHAPES` (default all) with a fixed RNG and prints the failure
/// rate and the failures grouped by cause. This is how the BT-3705 PR's
/// numbers were produced: `CV_CORPUS_SHAPES=do,cond,... cargo test -p
/// beamtalk-codegen --test class_var_agreement measure_failure_rate --
/// --ignored --nocapture`.
#[test]
#[ignore = "measurement: prints the failure rate and causes for CV_CORPUS_SHAPES"]
fn measure_failure_rate() {
    let cases = corpus_cases_from_env(512);
    let shapes = corpus_shapes_from_env(Shapes::all());
    let mut by_cause: Vec<(String, usize)> = Vec::new();
    let mut failed = 0;
    for (index, (_, _, program)) in draw_class_programs(shapes, cases).iter().enumerate() {
        if let Err(e) = check_program(index, program) {
            failed += 1;
            let c = cause(&e);
            match by_cause.iter_mut().find(|(k, _)| *k == c) {
                Some((_, n)) => *n += 1,
                None => by_cause.push((c, 1)),
            }
        }
    }
    by_cause.sort_by(|a, b| b.1.cmp(&a.1));
    println!("shapes {shapes:?}: {failed} of {cases} programs fail verified codegen");
    for (c, n) in by_cause {
        println!("  {n:>4} x {c}");
    }
}

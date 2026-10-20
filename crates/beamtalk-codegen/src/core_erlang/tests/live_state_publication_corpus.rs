// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3952 (ADR 0133 amendment): a closure that reads `self.field` and runs at home reads the
//! actor's live state. The obligation has two halves, proved here over a generated corpus of real
//! compiles rather than the one repro (CLAUDE.md: a state-threading fix is general or it is not a
//! fix).
//!
//! **Invariant (publish):** every actor field write, in whatever scope the compiler lowers it,
//! publishes its result to the runtime straight after it is minted
//! (`beamtalk_actor:publish_state/2` for a `State` version, `publish_field/3` for a hybrid loop's
//! rebound field parameter). The corpus is the loop shape x enclosing container x write shape cross
//! product `hybrid_field_rebind_corpus` already uses, so every scope kind that can lower a field
//! write (method root, conditional arm, `whileTrue:`/`whileFalse:`/`to:do:` loop, `do:`/
//! `inject:into:` fold, block value, `on:do:` body and `ensure:` body, hybrid and `StateAcc`
//! threading alike) is reached. The check is on the emitted Core Erlang: each field `maps:put`
//! that mints a `State` version is followed by the publish of that same version, and each hybrid
//! field rebind by the publish of its value.
//!
//! **Invariant (read):** a closure body reads the field through `beamtalk_actor:live_state/2`,
//! never the `State` it captured nor a loop's pre-extracted field parameter.
//!
//! **Invariant (rollback):** an `on:do:` that threads the actor `State` republishes its entry
//! `State` when it catches (`VerifyError::CatchWithoutLiveStateRestore`). The verifier is silent on
//! every unbroken program and fires, under `LocalRebindFault::SkipLiveStateRestore`, on every
//! program whose container threads `State` through an `on:do:`.

use super::hybrid_field_rebind_corpus::{CONTAINERS, HEADER, LOOPS, WRITES};
use super::{assert_compiles_through_erlc, codegen, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;
use regex::Regex;
use std::collections::BTreeMap;

/// `blk` is created before the loop and run right after the write: it must see that write.
const READER: &str = "blk := [self.m].";

/// The method for one `(loop, container, write)`: the closure is created first, the write is
/// followed by running the closure.
fn method(index: usize, lp: &str, container: &str, write: &str) -> String {
    let stmt = container.replace("STMT", &format!("{write}. blk value"));
    let body = lp.replace("BODY", &stmt);
    format!("  probe{index} =>\n    {READER}\n{body}")
}

/// Every `(case name, program)` of the corpus: one program per `(loop, container)` holding a
/// method for every write, as `hybrid_field_rebind_corpus` does, so the corpus costs one `erlc`
/// compile per pair.
fn programs() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (loop_name, lp) in LOOPS {
        for (container_name, container) in CONTAINERS {
            let methods: Vec<String> = WRITES
                .iter()
                .enumerate()
                .map(|(i, (_, write))| method(i, lp, container, write))
                .collect();
            out.push((
                format!("{loop_name} / {container_name}"),
                format!("{HEADER}\n{}", methods.join("\n")),
            ));
        }
    }
    out
}

/// A field write that mints a `State` version, in the emitted Core Erlang: the version and field.
fn state_field_puts(code: &str) -> Vec<(String, String)> {
    let re = Regex::new(r"let (State\w*) = call 'maps':'put'\('([nmk])', ").expect("regex");
    re.captures_iter(code)
        .map(|c| (c[1].to_string(), c[2].to_string()))
        .collect()
}

/// The hybrid field rebinds in the emitted Core Erlang: the value variable and field.
fn hybrid_rebinds(code: &str) -> Vec<(String, String)> {
    let re = Regex::new(r"let _([NMK])Field\d+ = (_Val\d+) in").expect("regex");
    re.captures_iter(code)
        .map(|c| (c[2].to_string(), c[1].to_lowercase()))
        .collect()
}

fn count(map: &mut BTreeMap<String, usize>, key: String) {
    *map.entry(key).or_default() += 1;
}

/// Why `code` breaks the publish or read invariant, or `None`.
fn defect(code: &str) -> Option<String> {
    let mut puts: BTreeMap<String, usize> = BTreeMap::new();
    for (version, _) in state_field_puts(code) {
        count(&mut puts, version);
    }
    for (version, puts_of_version) in &puts {
        let publish = format!("do call 'beamtalk_actor':'publish_state'(Self, {version}) ");
        let publishes = code.matches(&publish).count();
        if publishes != *puts_of_version {
            return Some(format!(
                "{puts_of_version} field put(s) mint {version}, {publishes} publish it"
            ));
        }
    }
    let mut rebinds: BTreeMap<(String, String), usize> = BTreeMap::new();
    for (value, field) in hybrid_rebinds(code) {
        *rebinds.entry((value, field)).or_default() += 1;
    }
    for ((value, field), rebinds_of_value) in &rebinds {
        let publish =
            format!("do call 'beamtalk_actor':'publish_field'(Self, '{field}', {value}) ");
        let publishes = code.matches(&publish).count();
        if publishes != *rebinds_of_value {
            return Some(format!(
                "{rebinds_of_value} hybrid rebind(s) of '{field}' from {value}, {publishes} publish it"
            ));
        }
    }
    // The closure `[self.m]`: live, never the captured State or a loop parameter.
    let live = Regex::new(
        r"fun \(\) -> call 'maps':'get'\('m', call 'beamtalk_actor':'live_state'\(Self, \w+\)\)",
    )
    .expect("regex");
    if !live.is_match(code) {
        return Some("the closure `[self.m]` does not read through live_state".to_string());
    }
    let stale =
        Regex::new(r"fun \(\) -> (_\w*Field\d*|call 'maps':'get'\('m', (State|StateAcc)\w*\))")
            .expect("regex");
    stale
        .find(code)
        .map(|m| format!("the closure reads captured state: {}", m.as_str()))
}

#[test]
fn every_actor_field_write_publishes_and_every_closure_read_is_live() {
    let mut failures: Vec<String> = Vec::new();
    let mut publishes_state = 0;
    let mut publishes_field = 0;
    for (name, src) in programs() {
        let code = codegen(&src);
        assert_compiles_through_erlc("test", &code);
        publishes_state += code.matches("'publish_state'(Self, State").count();
        publishes_field += code.matches("'publish_field'(Self, ").count();
        if let Some(why) = defect(&code) {
            failures.push(format!("{name}: {why}\n{src}"));
        }
    }
    assert!(
        failures.is_empty(),
        "{} case(s) break the live-state invariant:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
    // The corpus is only a proof if it reaches both publication forms.
    assert!(publishes_state > 0, "the corpus never publishes a State");
    assert!(
        publishes_field > 0,
        "the corpus never reaches a hybrid rebind"
    );
}

/// The control: the unbroken corpus is silent under the verifier, including the new
/// `CatchWithoutLiveStateRestore`.
#[test]
fn the_unbroken_corpus_has_a_silent_verifier() {
    let failures: Vec<String> = programs()
        .into_iter()
        .filter_map(|(name, src)| {
            verifier_report(&src, None).map(|report| format!("{name}: {report}\n{src}"))
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

/// A catch that does not republish the entry `State` is reported wherever an `on:do:` threads
/// the actor `State` through its arms, and nowhere else.
#[test]
fn a_catch_that_skips_the_live_state_restore_is_reported_wherever_state_is_threaded() {
    let mut reported = 0;
    let mut wrong = Vec::new();
    for (name, src) in programs() {
        let report = verifier_report(&src, Some(LocalRebindFault::SkipLiveStateRestore));
        let threads_state_through_on_do = name.ends_with("on_do_protected");
        match report {
            Some(report) if report.contains("CatchWithoutLiveStateRestore") => reported += 1,
            Some(report) => wrong.push(format!("{name}: wrong report: {report}\n{src}")),
            None => {
                // No threaded `on:do:` in this program: the fault changes nothing.
                if threads_state_through_on_do {
                    wrong.push(format!(
                        "{name}: the skipped restore went unreported\n{src}"
                    ));
                }
            }
        }
    }
    assert!(wrong.is_empty(), "{}", wrong.join("\n\n"));
    assert!(reported > 0, "the corpus never reaches a threaded on:do:");
}

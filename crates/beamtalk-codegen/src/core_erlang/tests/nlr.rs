// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::*;

#[test]
fn wrap_body_with_nlr_catch_class_method_no_vars_contains_nlr_scaffolding() {
    let mut generator = CoreErlangGenerator::new("nlr_test");
    let token = generator.fresh_temp_var("NlrToken");
    let body = leaf::var("Body".to_string());
    let doc = generator.wrap_body_with_nlr_catch(body, &token, &NlrBoundary::ClassMethod);
    let out = doc.to_pretty_string();
    assert!(
        out.contains("'erlang':'make_ref'()"),
        "missing make_ref: {out}"
    );
    assert!(out.contains("'$bt_nlr'"), "missing bt_nlr tag: {out}");
    assert!(
        out.contains("primop 'raw_raise'"),
        "missing raw_raise: {out}"
    );
    // ClassMethod catch arm yields bare value, not a tuple
    assert!(
        !out.contains("'reply'"),
        "should not contain actor-reply: {out}"
    );
    assert!(
        !out.contains("'class_var_result'"),
        "should not contain class_var_result: {out}"
    );
}

#[test]
fn wrap_body_with_nlr_catch_actor_reply_catch_arm_contains_reply_atom() {
    let mut generator = CoreErlangGenerator::new("nlr_test");
    let token = generator.fresh_temp_var("NlrToken");
    let body = leaf::var("Body".to_string());
    let doc = generator.wrap_body_with_nlr_catch(body, &token, &NlrBoundary::ActorReply);
    let out = doc.to_pretty_string();
    assert!(out.contains("'reply'"), "missing reply atom: {out}");
    assert!(out.contains("'$bt_nlr'"), "missing bt_nlr tag: {out}");
}

#[test]
fn wrap_body_with_nlr_catch_value_type_mints_token_and_answers_bare_value() {
    let mut generator = CoreErlangGenerator::new("nlr_test");
    let token = generator.fresh_temp_var("NlrToken");
    let body = leaf::var("Body".to_string());
    let out = generator
        .wrap_body_with_nlr_catch(body, &token, &NlrBoundary::ValueType)
        .to_pretty_string();
    assert!(
        out.contains("'erlang':'make_ref'()"),
        "missing make_ref: {out}"
    );
    assert!(out.contains("try"), "missing try: {out}");
    assert!(out.contains("'$bt_nlr'"), "missing bt_nlr: {out}");
    assert!(
        out.contains("primop 'raw_raise'"),
        "missing raw_raise: {out}"
    );
    // BT-4503: the ValueType catch arm answers the bare value — neither the
    // actor-reply tuple nor a `{Value, State}` pair.
    assert!(
        !out.contains("'reply'"),
        "should not contain actor-reply: {out}"
    );
    assert!(
        !out.contains("{_NlrVal") && !out.contains("{NlrVal"),
        "should not answer a pair: {out}"
    );
}

/// ADR 0146 (BT-4535): every actor-method NLR boundary (a dispatch clause and
/// a sealed method's standalone function) commits the actor's cell, not the
/// state the throw carries.
const ACTOR_NLR_BOUNDARIES: &str = "Actor subclass: Bt4535Owner
  state: n = 0

  run: peer =>
    blk := [^1]
    self.n := 5
    peer call: blk
    0

  loopExit =>
    #(1, 2, 3) do: [:x | x == 2 ifTrue: [^x]]
    0

sealed Actor subclass: Bt4535Sealed
  state: n = 0

  run: peer =>
    blk := [^1]
    self.n := 5
    peer call: blk
    0
";

#[test]
fn every_actor_nlr_boundary_commits_the_cell() {
    let code = codegen(ACTOR_NLR_BOUNDARIES);
    let catch_arms = code.matches("'$bt_nlr', _CatchTok").count();
    assert!(
        catch_arms >= 3,
        "expected three actor NLR boundaries:\n{code}"
    );
    assert_eq!(
        code.matches("'beamtalk_actor_state':'commit'(Self, _NlrState")
            .count(),
        catch_arms,
        "every actor NLR boundary must commit the cell:\n{code}"
    );
    if erlc_available() {
        assert_compiles_through_erlc("test", &code);
    }
}

/// The debug-build panic message of compiling `src` with `fault` on (a
/// verifier finding), or `None` when the compile succeeds.
#[cfg(debug_assertions)]
fn report_under(fault: crate::core_erlang::nlr::fault::NlrFault, src: &str) -> Option<String> {
    crate::core_erlang::nlr::fault::with_fault(fault, || {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| codegen(src)))
    })
    .err()
    .map(|payload| {
        payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default()
    })
}

/// The verifier rejects an actor NLR boundary that commits the carried state
/// (`VerifyError::ActorNlrCommitBypassesCell`): the boundary before ADR 0146.
#[cfg(debug_assertions)]
#[test]
fn an_actor_nlr_boundary_committing_the_carried_state_is_rejected() {
    use crate::core_erlang::nlr::fault::NlrFault;
    let report = report_under(NlrFault::CommitCarried, ACTOR_NLR_BOUNDARIES);
    assert!(
        report
            .as_deref()
            .is_some_and(|r| r.contains("ActorNlrCommitBypassesCell")),
        "expected ActorNlrCommitBypassesCell, got {report:?}"
    );
}

/// BT-4535: the `^` value shapes a conditional arm lowers to `ThreadedIr`
/// (BT-3495's field write, BT-3374's dispatching self-send) and the generic
/// `Return` lowering (any other value), in every conditional an actor method
/// can hold, with a receiver that is and is not statically Boolean.
fn arm_return_corpus() -> Vec<(String, String)> {
    let values = [
        ("field_write", "^(self.count := self.count + 10)"),
        ("self_send", "^(self bump)"),
        ("plain", "^self.count"),
        ("literal", "^7"),
    ];
    let conditionals = [
        ("if_true", "{recv} ifTrue: [{ret}]"),
        ("if_false", "{recv} ifFalse: [{ret}]"),
        (
            "if_true_if_false",
            "{recv} ifTrue: [{ret}] ifFalse: [self.count := 1]",
        ),
        (
            "if_false_if_true",
            "{recv} ifFalse: [self.count := 1] ifTrue: [{ret}]",
        ),
    ];
    let receivers = [("param", "flag"), ("compare", "(flag == true)")];
    let mut cells = Vec::new();
    for (vname, value) in values {
        for (cname, conditional) in conditionals {
            for (rname, recv) in receivers {
                let stmt = conditional.replace("{recv}", recv).replace("{ret}", value);
                let src = format!(
                    "Actor subclass: Bt4535Arm\n  state: count = 0\n\n  bump => self.count := self.count + 1\n\n  \
                     run: flag =>\n    self.count := self.count + 5\n    {stmt}\n    self.count := self.count - 1\n    self.count\n"
                );
                cells.push((format!("{vname}/{cname}/{rname}"), src));
            }
        }
    }
    cells
}

/// Every `$bt_nlr` throw of a method's own `^` in `code` (its token is the
/// method's `_NlrToken`; a self-dispatch relay re-throws another's) either
/// carries the cell (a closure: `view/2`, a loop's `take/0`) or is
/// preceded by the store of the state it carries: the cell is current
/// whenever a `^` starts.
fn every_throw_stores_its_state(code: &str) -> std::result::Result<usize, String> {
    let mut stored = 0;
    let throw = "call 'erlang':'throw'({'$bt_nlr', _NlrToken";
    for (at, _) in code.match_indices(throw) {
        // The 4-tuple's elements, split at depth 0.
        let rest = &code[at + throw.len() - "_NlrToken".len()..];
        let mut depth = 0i32;
        let mut parts = vec![String::new()];
        for c in rest.chars() {
            match c {
                '(' | '{' | '[' => depth += 1,
                ')' | '}' | ']' if depth == 0 => break,
                ')' | '}' | ']' => depth -= 1,
                ',' if depth == 0 => {
                    parts.push(String::new());
                    continue;
                }
                _ => {}
            }
            if let Some(last) = parts.last_mut() {
                last.push(c);
            }
        }
        let slot = parts.get(2).map_or("", |s| s.trim());
        if slot.contains("'view'(") || slot.contains("'take'()") {
            continue;
        }
        let before = code[..at].trim_end();
        let before = before.strip_suffix('=').map_or(before, str::trim_end);
        let before = match before.rsplit_once("let ") {
            Some((head, var)) if !var.contains(' ') => head.trim_end(),
            _ => before,
        };
        let store = format!("let _ = call 'beamtalk_actor_state':'store'({slot}) in");
        if !before.ends_with(&store) {
            return Err(format!("a throw of `{slot}` does not store it first"));
        }
        stored += 1;
    }
    Ok(stored)
}

#[test]
fn every_actor_arm_return_stores_its_state_before_the_throw() {
    // The oracle itself: a bare throw fails it, a stored one passes.
    let bare = "let _NlrThrow@1 = call 'erlang':'throw'({'$bt_nlr', _NlrToken@1, V, State2}) in ";
    assert!(every_throw_stores_its_state(bare).is_err());
    let stored = format!("let _ = call 'beamtalk_actor_state':'store'(State2) in {bare}");
    assert_eq!(every_throw_stores_its_state(&stored), Ok(1));

    let mut failures = Vec::new();
    let mut checked = 0;
    let cells = arm_return_corpus();
    for (label, src) in &cells {
        let code = codegen(src);
        match every_throw_stores_its_state(&code) {
            Ok(n) => checked += n,
            Err(why) => failures.push(format!("{label}: {why}\n{code}")),
        }
    }
    assert_eq!(cells.len(), 32);
    assert!(checked >= 8, "too few stored throws checked: {checked}");
    assert!(
        failures.is_empty(),
        "{} cell(s) throw without storing:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// The verifier rejects an arm `^` that throws without storing its state
/// (`VerifyError::NlrThrowWithoutCellStore`): BT-3374's and BT-3495's arm
/// throws before BT-4535. It fires exactly on the cells whose `^` an arm
/// lowers to an `NlrThrow` node.
#[cfg(debug_assertions)]
#[test]
fn an_arm_return_without_its_cell_store_is_rejected() {
    use crate::core_erlang::nlr::fault::NlrFault;
    let mut fired = 0;
    for (label, src) in arm_return_corpus() {
        let report = report_under(NlrFault::ThrowWithoutCellStore, &src);
        match report {
            Some(r) if r.contains("NlrThrowWithoutCellStore") => fired += 1,
            Some(other) => panic!("{label}: unexpected report {other}"),
            None => {}
        }
    }
    assert!(fired >= 3, "too few arm throws: {fired}");
}

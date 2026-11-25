// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Hostile deep or long input must not overflow the compiler port's stack
//! (BT-5181).
//!
//! The port compiles on one thread with [`STACK_SIZE`]; a stack overflow
//! aborts the port and every compile the workspace has in flight. Each test
//! compiles on a thread of that size, through the parser's limits: a
//! 100k-term chain (capped by the parser), a chain just under the cap, and
//! nesting and chains combined just inside the parser's depth limit.

use super::*;

/// Runs `request` through `handler` on a thread with the port's stack size.
fn on_port_stack(request: Map, handler: fn(&Map) -> Term) -> Term {
    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move || handler(&request))
        .expect("spawn compile thread")
        .join()
        .expect("compile thread panicked")
}

fn compile_class(body: &str) -> Term {
    let source = format!("Object subclass: Deep\n  run: a =>\n    {body}\n");
    on_port_stack(compile_request(&source), handle_compile)
}

fn compile_expression(source: &str) -> Term {
    on_port_stack(
        compile_expression_request(source),
        handle_compile_expression,
    )
}

/// Every string in `response` (diagnostic messages among them), joined.
fn diagnostic_text(response: &Term) -> String {
    fn collect(term: &Term, out: &mut String) {
        match term {
            Term::Map(map) => map.map.iter().for_each(|(k, v)| {
                collect(k, out);
                collect(v, out);
            }),
            Term::List(list) => list.elements.iter().for_each(|e| collect(e, out)),
            other => {
                if let Some(text) = term_to_string(other) {
                    out.push_str(&text);
                    out.push('\n');
                }
            }
        }
    }
    let mut out = String::new();
    collect(response, &mut out);
    out
}

#[test]
fn hundred_thousand_term_chain_is_rejected_without_overflow() {
    let chain = "a".to_string() + &" + a".repeat(100_000);
    let response = compile_class(&chain);
    assert_eq!(response_status(&response).as_deref(), Some("error"));
    assert!(diagnostic_text(&response).contains("too long"));

    let response = compile_expression(&chain);
    assert_eq!(response_status(&response).as_deref(), Some("error"));
    assert!(diagnostic_text(&response).contains("too long"));
}

#[test]
fn chain_just_under_the_cap_compiles() {
    let chain = "a".to_string() + &" + a".repeat(505);
    let response = compile_class(&chain);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "{}",
        diagnostic_text(&response)
            .chars()
            .take(2000)
            .collect::<String>()
    );
}

#[test]
fn message_chain_just_under_the_cap_compiles() {
    let chain = "a".to_string() + &" printString".repeat(505);
    let response = compile_class(&chain);
    assert_eq!(response_status(&response).as_deref(), Some("ok"));
}

#[test]
fn nesting_and_chains_just_inside_the_limits_compile() {
    let mut source = String::new();
    for _ in 0..60 {
        source.push('(');
    }
    source.push('a');
    for _ in 0..60 {
        source.push_str(&" + a".repeat(6));
        source.push(')');
    }
    let response = compile_class(&source);
    assert_eq!(response_status(&response).as_deref(), Some("ok"));
}

#[test]
fn deeply_nested_literal_is_rejected_without_overflow() {
    let source = "#(".repeat(5_000) + "1" + &")".repeat(5_000);
    let response = compile_class(&source);
    assert_eq!(response_status(&response).as_deref(), Some("error"));
    assert!(diagnostic_text(&response).contains("nesting is too deep"));
}

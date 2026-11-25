// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Hostile deep or long input must not overflow the language server's stack
//! (BT-5181).
//!
//! The LSP answers requests on tokio worker threads, which have 2 MiB stacks.
//! A stack overflow aborts the whole server, so each test runs the
//! language service on a thread of that size over input the parser bounds:
//! a 100k-term chain (capped by the parser), a chain just under the cap, and
//! nesting and chains combined up to the parser's depth limit.

use super::common::*;

/// A tokio worker thread's stack.
const LSP_THREAD_STACK: usize = 2 * 1024 * 1024;

/// Opens `source` and runs the requests an editor sends for it, on a thread
/// with the LSP's stack size; answers the diagnostics.
fn analyse_on_lsp_stack(source: String) -> Vec<String> {
    std::thread::Builder::new()
        .stack_size(LSP_THREAD_STACK)
        .spawn(move || {
            let mut service = SimpleLanguageService::new();
            let file = Utf8PathBuf::from("deep.bt");
            let end = u32::try_from(source.len()).unwrap_or(u32::MAX);
            service.update_file(file.clone(), source);
            let diagnostics = service.diagnostics(&file);
            let _ = service.document_symbols(&file);
            let _ = service.folding_ranges(&file);
            let _ = service.code_actions(&file, 0, end);
            for column in [0, 40, 400] {
                let position = Position::new(2, column);
                let _ = service.hover(&file, position);
                let _ = service.completions(&file, position);
                let _ = service.goto_definition(&file, position);
            }
            service.remove_file(&file);
            diagnostics
                .into_iter()
                .map(|d| d.message.to_string())
                .collect()
        })
        .expect("spawn analysis thread")
        .join()
        .expect("analysis thread panicked")
}

/// `body` as the body of a method in a class, so every pass that runs on
/// method bodies (type checking, linting, validators) sees it.
fn in_method(body: &str) -> String {
    format!("Object subclass: Deep\n  run: a =>\n    {body}\n")
}

#[test]
fn hundred_thousand_term_chain_does_not_overflow() {
    let chain = "a".to_string() + &" + a".repeat(100_000);
    let diagnostics = analyse_on_lsp_stack(in_method(&chain));
    assert!(
        diagnostics.iter().any(|m| m.contains("too long")),
        "{:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

#[test]
fn hundred_thousand_message_chain_does_not_overflow() {
    let chain = "a".to_string() + &" foo".repeat(100_000);
    let diagnostics = analyse_on_lsp_stack(in_method(&chain));
    assert!(
        diagnostics.iter().any(|m| m.contains("too long")),
        "{:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

#[test]
fn chain_just_under_the_cap_does_not_overflow() {
    // About the deepest flat chain the parser accepts (it caps a chain at
    // 512 links).
    let chain = "a".to_string() + &" + a".repeat(505);
    let diagnostics = analyse_on_lsp_stack(in_method(&chain));
    assert!(
        !diagnostics.iter().any(|m| m.contains("too long")),
        "{:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

#[test]
fn unary_chain_just_under_the_cap_does_not_overflow() {
    let chain = "a".to_string() + &" printString".repeat(505);
    analyse_on_lsp_stack(in_method(&chain));
}

#[test]
fn nesting_and_chains_at_the_depth_limit_do_not_overflow() {
    // Parenthesised levels, each a chain whose first operand is the next
    // level: as deep as the parser lets through, through both mechanisms.
    let mut source = String::new();
    for _ in 0..60 {
        source.push('(');
    }
    source.push('a');
    for _ in 0..60 {
        source.push_str(&" + a".repeat(6));
        source.push(')');
    }
    let diagnostics = analyse_on_lsp_stack(in_method(&source));
    assert!(
        !diagnostics
            .iter()
            .any(|m| m.contains("too long") || m.contains("too deep")),
        "this input is meant to sit just inside the limits: {:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

#[test]
fn nested_blocks_and_literals_at_the_nesting_limit_do_not_overflow() {
    let source = "[".repeat(30) + &"#(".repeat(30) + "a" + &")".repeat(30) + &"]".repeat(30);
    analyse_on_lsp_stack(in_method(&source));
}

#[test]
fn deeply_nested_input_past_the_limit_does_not_overflow() {
    let source = "#(".repeat(5_000) + "1" + &")".repeat(5_000);
    let diagnostics = analyse_on_lsp_stack(in_method(&source));
    assert!(
        diagnostics
            .iter()
            .any(|m| m.contains("nesting is too deep")),
        "{:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

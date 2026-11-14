// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4356: a block-evaluation send's line annotation sits on the `apply`
//! (or `call`) node that transfers control, not only on the `let`/`case`
//! wrapper the send lowers to.
//!
//! The OTP compiler takes a call's stack-frame location from the annotation
//! on the call node itself; one on an enclosing `let`/`case` never reaches
//! it. A `block value` on a non-literal receiver lowers to `let _ValRecv =
//! Recv in case is_function(_ValRecv, 0) of 'true' -> apply _ValRecv () ...`,
//! so before this fix the frame of the method evaluating the block (e.g.
//! `Integer>>timesRepeat:`) carried no `file:line` at all.
//! `stdlib/test/block_call_site_line_test.bt` checks the runtime frames end
//! to end; these tests pin the emitted Core Erlang.

use super::*;
use crate::core_erlang::threaded_ir;

const PATH: &str = "runner.bt";

/// Compiles `src` with its source text and path attached, as the CLI build
/// does, so sends carry `[Line, {'file', Path}]` annotations.
fn codegen_with_lines(src: &str) -> String {
    let (module, diagnostics) = parse_normalised(src);
    assert!(
        diagnostics
            .iter()
            .all(|d| d.severity != beamtalk_core::source_analysis::Severity::Error),
        "fixture failed to parse: {diagnostics:?}"
    );
    generate_module(
        &module,
        CodegenOptions::new("runner")
            .with_source(src)
            .with_source_path_opt(Some(PATH)),
    )
    .expect("codegen should succeed")
}

/// The annotation suffix a node on `line` of [`PATH`] ends with.
fn line_anno(line: u32) -> String {
    format!(" -| [{line}, {{'file', \"{PATH}\"}}] )")
}

/// Every occurrence of `node` (an `apply`/`call` prefix) in `code` must be
/// the first thing inside an annotation wrapper `( node ... -| [line, ...] )`.
fn assert_every_node_annotated(code: &str, node: &str, line: u32) {
    let occurrences: Vec<usize> = code.match_indices(node).map(|(at, _)| at).collect();
    assert!(!occurrences.is_empty(), "expected `{node}` in:\n{code}");
    let anno = line_anno(line);
    for at in occurrences {
        assert!(
            code[..at].ends_with("( "),
            "`{node}` at byte {at} is not wrapped in its own line annotation:\n{code}"
        );
        let rest = &code[at..];
        let close = rest
            .find(" -| [")
            .unwrap_or_else(|| panic!("no annotation after `{node}`:\n{code}"));
        assert!(
            rest[close..].starts_with(&anno),
            "`{node}` at byte {at} should be annotated with line {line}, got `{}`:\n{code}",
            &rest[close..close + anno.len().min(rest.len() - close)]
        );
    }
}

#[test]
fn a_unary_value_send_annotates_its_apply_node() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk =>\n",
        "    blk value\n",
        "    nil\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "apply _ValRecv", 3);
    assert_every_node_annotated(&code, "call 'beamtalk_message_dispatch':'send'(_ValRecv", 3);
}

#[test]
fn a_keyword_value_send_annotates_its_apply_node() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk with: x =>\n",
        "    blk value: x\n",
        "    nil\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "apply _ValRecv", 3);
    assert_every_node_annotated(&code, "call 'beamtalk_message_dispatch':'send'(_ValRecv", 3);
}

#[test]
fn a_value_with_arguments_send_annotates_its_apply_node() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk with: args =>\n",
        "    blk valueWithArguments: args\n",
        "    nil\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "call 'erlang':'apply'(_ValRecv", 3);
    assert_every_node_annotated(&code, "call 'beamtalk_message_dispatch':'send'(_ValRecv", 3);
}

/// The send's line, not the method's: a send on a later line of the body
/// annotates its apply with that line.
#[test]
fn the_apply_carries_the_send_line_not_the_method_line() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk =>\n",
        "    x := 1\n",
        "    y := 2\n",
        "    blk value\n",
        "    x + y\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "apply _ValRecv", 5);
}

/// The annotated output is still well-formed Core Erlang.
#[test]
fn annotated_block_sends_compile_through_erlc() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: blk =>\n",
        "    blk value\n",
        "    nil\n",
        "  run: blk with: x =>\n",
        "    blk value: x\n",
        "    nil\n",
        "  run: blk withAll: args =>\n",
        "    blk valueWithArguments: args\n",
        "    nil\n",
    );
    assert_compiles_through_erlc("runner", &codegen_with_lines(src));
}

// ---------------------------------------------------------------------------
// BT-4366: the invariant, over a corpus
// ---------------------------------------------------------------------------

/// Prefixes of the nodes that transfer control for a send: the class-side
/// and instance-side runtime dispatch, and an `apply` of a variable (a block,
/// handler, cleanup or other user fun). A `letrec` loop's entry
/// `apply '_LoopN'/0` is covered separately by [`loop_entry_applies`].
const DISPATCH_CALL_PREFIXES: [&str; 3] = [
    "call 'beamtalk_object_class':'class_send'(",
    "call 'beamtalk_message_dispatch':'send'(",
    "apply _",
];

/// Whether a `letrec` function name is a loop's: the generic fallbacks'
/// gensym'd `_LoopN` (BT-4392) or the static name a `ThreadedIr`-lowered
/// loop carries (BT-4401). Both come from the producers' own constants
/// (BT-4405), so a new static loop name cannot be silently missed.
fn is_loop_fn(name: &str) -> bool {
    name.strip_prefix('_')
        .is_some_and(|rest| rest.starts_with(threaded_ir::LOOP_FN_STEM))
        || threaded_ir::STATIC_LOOP_FNS.contains(&name)
}

/// Byte offsets of each `letrec` loop's entry `apply 'FnName'/N` (BT-4392,
/// BT-4401).
///
/// A loop's recursive back-edge sits in the loop fun's own body and stays
/// bare: it is a tail call, and the frame it would name is the loop's own.
/// Only the entry call, the one frame the method making the send reports,
/// carries the send's line. A `ThreadedIr` loop's name is static (not unique
/// per module), so the entry is found structurally: the `apply` following the
/// `in` that closes the `letrec` binding, counting the `let`/`letrec`
/// keywords the binding's body opens.
fn loop_entry_applies(code: &str) -> Vec<usize> {
    const LETREC: &str = "letrec '";
    let bytes = code.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_' || b == b'@';
    let mut entries = Vec::new();
    for (at, _) in code.match_indices(LETREC) {
        let name_start = at + LETREC.len();
        // A malformed site must fail loudly rather than silently drop out.
        let name_len = code[name_start..]
            .find('\'')
            .unwrap_or_else(|| panic!("letrec without a closing name quote at byte {at}"));
        if !is_loop_fn(&code[name_start..name_start + name_len]) {
            continue;
        }
        let mut pending = 1usize;
        let mut i = name_start + name_len + 1;
        while i < bytes.len() {
            let lit_end = skip_literal(bytes, i);
            if lit_end != i {
                i = lit_end + 1;
                continue;
            }
            if bytes[i].is_ascii_alphabetic() && !is_ident(bytes[i - 1]) {
                let end = (i..bytes.len())
                    .find(|&j| !is_ident(bytes[j]))
                    .unwrap_or(bytes.len());
                match &code[i..end] {
                    "let" | "letrec" => pending += 1,
                    "in" => {
                        pending -= 1;
                        if pending == 0 {
                            let rest = code[end..].trim_start();
                            let rest = rest.strip_prefix("( ").unwrap_or(rest).trim_start();
                            if rest.starts_with("apply ") {
                                entries.push(code.len() - rest.len());
                            }
                            break;
                        }
                    }
                    _ => {}
                }
                i = end;
                continue;
            }
            i += 1;
        }
    }
    entries.sort_unstable();
    entries
}

/// Advances `i` past a quoted atom/string or character literal starting at
/// `bytes[i]`, returning the index of its last byte; `i` unchanged otherwise.
fn skip_literal(bytes: &[u8], mut i: usize) -> usize {
    match bytes[i] {
        quote @ (b'\'' | b'"') => {
            i += 1;
            while i < bytes.len() && bytes[i] != quote {
                if bytes[i] == b'\\' {
                    i += 1;
                }
                i += 1;
            }
            i
        }
        b'$' => {
            i + if bytes.get(i + 1) == Some(&b'\\') {
                2
            } else {
                1
            }
        }
        _ => i,
    }
}

/// The `(start, end)` byte ranges of every parenthesised node carrying a
/// source-line annotation (`( node -| [Line, ...] )`) in `code`. Atoms,
/// strings and character literals are skipped, so a `(` inside one never
/// opens a node.
fn annotated_ranges(code: &str) -> Vec<(usize, usize)> {
    let bytes = code.as_bytes();
    let mut stack: Vec<(usize, bool)> = Vec::new();
    let mut ranges = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        i = skip_literal(bytes, i);
        match bytes[i] {
            b'(' => stack.push((i, false)),
            b')' => {
                if let Some((open, true)) = stack.pop() {
                    ranges.push((open, i));
                }
            }
            b'-' if code[i..].starts_with("-| [")
                && bytes.get(i + 4).is_some_and(u8::is_ascii_digit) =>
            {
                if let Some(top) = stack.last_mut() {
                    top.1 = true;
                }
            }
            _ => {}
        }
        i += 1;
    }
    ranges
}

/// Every dispatch call in `code` that sits inside an annotated node without
/// carrying an annotation itself: a `call`/`apply` the OTP compiler will give
/// no stack-frame location, because the annotation that names its send's
/// line is on an enclosing `let`/`case`. Returns `(byte offset, snippet)`.
fn unannotated_dispatch_calls(code: &str) -> Vec<(usize, String)> {
    let ranges = annotated_ranges(code);
    let starts: std::collections::HashSet<usize> = ranges.iter().map(|r| r.0).collect();
    let mut found = Vec::new();
    let sites = DISPATCH_CALL_PREFIXES
        .iter()
        .flat_map(|prefix| code.match_indices(prefix).map(|(at, _)| at))
        .chain(loop_entry_applies(code));
    for at in sites {
        let own = at >= 2 && &code[at - 2..at] == "( " && starts.contains(&(at - 2));
        if own {
            continue;
        }
        // A method's own `( fun (..) -> .. -| [Line] )` annotation names the
        // method's line, not a send: a call in a fixed-shape primitive body
        // (`perform:withArguments:timeout:`) has no send to annotate.
        let inside_a_send =
            |&&(s, e): &&(usize, usize)| s < at && at < e && !code[s..].starts_with("( fun (");
        if ranges.iter().any(|r| inside_a_send(&r)) {
            let snippet: String = code[at..].chars().take(60).collect();
            found.push((at, snippet));
        }
    }
    found.sort();
    found
}

#[test]
fn the_scanner_flags_a_call_whose_annotation_sits_on_a_let() {
    let bad = "( let _P = call 'm':'f'() in call 'beamtalk_object_class':'class_send'(_P, 'x', []) -| [3, {'file', \"a.bt\"}] )";
    assert_eq!(unannotated_dispatch_calls(bad).len(), 1);
    let good = "( let _P = call 'm':'f'() in ( call 'beamtalk_object_class':'class_send'(_P, 'x', []) -| [3, {'file', \"a.bt\"}] ) -| [3, {'file', \"a.bt\"}] )";
    assert!(unannotated_dispatch_calls(good).is_empty());
    let bare = "let _P = call 'm':'f'() in call 'beamtalk_object_class':'class_send'(_P, 'x', [])";
    assert!(unannotated_dispatch_calls(bare).is_empty());
}

/// The invariant over the real corpus: no lowering leaves a dispatch call
/// without its own line inside an annotated send, so a new lowering that
/// nests its call in a `let`/`case` cannot silently regress stack traces.
///
/// BT-4392: measured at ~13s in a debug build (full codegen over the corpus,
/// in parallel with the other fast-tier tests), well inside the fast tier's
/// budget, so it stays there rather than moving to the slow tier.
#[test]
fn no_dispatch_call_is_unannotated_inside_an_annotated_send_over_the_corpus() {
    let root = super::repo_root();
    let sources = super::corpus_files(&root, &["stdlib/src", "stdlib/test", "examples"]);
    assert!(sources.len() > 50, "corpus unexpectedly small");
    let mut violations = Vec::new();
    for (i, path) in sources.iter().enumerate() {
        let Ok(source) = std::fs::read_to_string(path) else {
            continue;
        };
        let (module, _) = super::parse_normalised(&source);
        let stdlib = path.starts_with(root.join("stdlib/src"));
        let options = CodegenOptions::new(&format!("call_site_corpus_{i}"))
            .with_stdlib_mode(stdlib)
            .with_source(&source)
            .with_source_path_opt(Some("corpus.bt"));
        // A fixture meant to fail codegen has no Core to check.
        let Ok(core) = generate_module(&module, options) else {
            continue;
        };
        for (at, snippet) in unannotated_dispatch_calls(&core) {
            violations.push(format!("{} @{at}: {snippet}", path.display()));
        }
    }
    assert!(
        violations.is_empty(),
        "{} dispatch call(s) lack their own line annotation:\n{}",
        violations.len(),
        violations.join("\n")
    );
}

/// One method per lowering BT-4366 audited, each nesting its control-
/// transferring call inside a `let`/`case`/`try`/`letrec`.
const AUDITED_LOWERINGS: &str = concat!(
    "Object subclass: Runner\n",
    "  classSend => Integer to: 3 do: [:i | i]\n",
    "  classSendValue => Tracing setContext: 1\n",
    "  ifNotNil: x blk: b => x ifNotNil: b\n",
    "  ifNilNotNil: x nil: n blk: b => x ifNil: n ifNotNil: b\n",
    "  onDo: blk handler: h => blk on: Error do: h\n",
    "  ensure: blk cleanup: c => blk ensure: c\n",
    "  whileTrue: c body: b => c whileTrue: b\n",
    "  repeat: b => b repeat\n",
    "  cascade: x => x foo; bar\n",
    "  listOp: xs blk: b => xs do: b\n",
    "  detect: xs blk: b none: n => xs detect: b ifNone: n\n",
    "  binary: a with: b => a + b\n",
    "  interp: a => \"x {a} y\"\n",
);

#[test]
fn every_audited_lowering_annotates_its_dispatch_call() {
    let code = codegen_with_lines(AUDITED_LOWERINGS);
    let missed = unannotated_dispatch_calls(&code);
    assert!(
        missed.is_empty(),
        "dispatch calls without their own line annotation: {missed:#?}\n{code}"
    );
}

/// The class send is the node the issue names: `class_send` sits inside the
/// `whereis_class` `let`, and carries the send's own line.
#[test]
fn a_class_send_annotates_the_class_send_node() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run =>\n",
        "    Integer to: 3 do: [:i | i]\n",
        "    nil\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "call 'beamtalk_object_class':'class_send'(", 3);
}

/// `ifNotNil:` on a non-literal block: the `apply` is inside the nil-test
/// `case`, and carries the send's line.
#[test]
fn an_if_not_nil_send_annotates_its_apply_node() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: x blk: b =>\n",
        "    x ifNotNil: b\n",
        "    nil\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "apply _NotNilBlk", 3);
}

/// `on:do:` and `ensure:` on non-literal blocks: the protected block, handler
/// and cleanup applies are inside the `try`/`catch`.
#[test]
fn on_do_and_ensure_sends_annotate_their_apply_nodes() {
    let on_do = concat!(
        "Object subclass: Runner\n",
        "  run: b handler: h =>\n",
        "    b on: Error do: h\n",
        "    nil\n",
    );
    let code = codegen_with_lines(on_do);
    assert_every_node_annotated(&code, "apply _BlockFun", 3);
    assert_every_node_annotated(&code, "apply _HandlerFun", 3);

    let ensure = concat!(
        "Object subclass: Runner\n",
        "  run: b cleanup: c =>\n",
        "    b ensure: c\n",
        "    nil\n",
    );
    let code = codegen_with_lines(ensure);
    assert_every_node_annotated(&code, "apply _BlockFun", 3);
    assert_every_node_annotated(&code, "apply _CleanupFun", 3);
}

/// `whileTrue:` on non-literal blocks: both applies are inside the loop's
/// `letrec`.
#[test]
fn a_while_true_send_annotates_its_apply_nodes() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: c body: b =>\n",
        "    c whileTrue: b\n",
        "    nil\n",
    );
    let code = codegen_with_lines(src);
    assert_every_node_annotated(&code, "apply _CondFun", 3);
    assert_every_node_annotated(&code, "apply _BodyFun", 3);
}

/// The annotated output is still well-formed Core Erlang.
#[test]
fn annotated_audited_lowerings_compile_through_erlc() {
    assert_compiles_through_erlc("runner", &codegen_with_lines(AUDITED_LOWERINGS));
}

/// BT-4392: each message of a multi-line cascade carries its own line, not
/// the receiver's.
#[test]
fn each_cascade_message_carries_its_own_line() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: x =>\n",
        "    x foo;\n",
        "      bar;\n",
        "      baz\n",
    );
    let code = codegen_with_lines(src);
    for (selector, line) in [("foo", 3), ("bar", 4), ("baz", 5)] {
        let needle = format!(", '{selector}', [])");
        let at = code
            .find(&needle)
            .unwrap_or_else(|| panic!("no `{selector}` send in:\n{code}"));
        let rest = &code[at + needle.len()..];
        assert!(
            rest.starts_with(&line_anno(line)),
            "`{selector}` should carry line {line}, got `{}`:\n{code}",
            &rest[..rest.len().min(40)]
        );
    }
    assert_compiles_through_erlc("runner", &code);
}

/// BT-4392: the loop-entry `apply '_LoopN'/0` of the generic `whileTrue:` and
/// `repeat` fallbacks carries the send's line, as the condition/body applies.
#[test]
fn generic_loop_fallbacks_annotate_their_entry_apply() {
    let while_src = concat!(
        "Object subclass: Runner\n",
        "  run: c body: b =>\n",
        "    c whileTrue: b\n",
        "    nil\n",
    );
    let repeat_src = concat!(
        "Object subclass: Runner\n",
        "  run: b =>\n",
        "    b repeat\n",
        "    nil\n",
    );
    for src in [while_src, repeat_src] {
        let code = codegen_with_lines(src);
        let entries = loop_entry_applies(&code);
        assert!(!entries.is_empty(), "expected a loop in:\n{code}");
        assert!(
            unannotated_dispatch_calls(&code).is_empty(),
            "loop entry apply left unannotated:\n{code}"
        );
    }
}

#[test]
fn the_scanner_flags_an_unannotated_loop_entry_but_not_back_edges() {
    let anno = "-| [3, {'file', \"a.bt\"}]";
    let bad = format!(
        "( letrec '_Loop1'/0 = fun () -> apply '_Loop1'/0 () in apply '_Loop1'/0 () {anno} )"
    );
    assert_eq!(unannotated_dispatch_calls(&bad).len(), 1);
    let good = format!(
        "( letrec '_Loop1'/0 = fun () -> apply '_Loop1'/0 () in ( apply '_Loop1'/0 () {anno} ) {anno} )"
    );
    assert!(unannotated_dispatch_calls(&good).is_empty());
}

/// BT-4401: the entry `apply 'FnName'/N` of a `ThreadedIr`-lowered loop
/// (`ConditionalLoop`: the outer local `sum` is threaded through each loop)
/// carries the send's line, on the loop's own source line. Before the line was
/// carried on the node, the emitter left this apply bare.
#[test]
fn threaded_ir_loops_annotate_their_entry_apply() {
    let src = concat!(
        "Object subclass: Runner\n",
        "  run: n =>\n",
        "    sum := 0.\n",
        "    1 to: n do: [:i | sum := sum + i].\n",
        "    [sum > 0] whileTrue: [sum := sum - 1].\n",
        "    n timesRepeat: [sum := sum + 1].\n",
        "    sum\n",
    );
    let code = codegen_with_lines(src);
    assert_loop_entry_lines(&code, &[4, 5, 6]);
    assert!(unannotated_dispatch_calls(&code).is_empty(), "{code}");
    assert_compiles_through_erlc("runner", &code);
}

/// Every `letrec` loop entry in `code`, in source order, is wrapped in its own
/// annotation naming the matching line of `lines` — one entry per line.
fn assert_loop_entry_lines(code: &str, lines: &[u32]) {
    let entries = loop_entry_applies(code);
    assert_eq!(
        entries.len(),
        lines.len(),
        "expected {} loop entries in:\n{code}",
        lines.len()
    );
    for (at, &line) in entries.into_iter().zip(lines) {
        let rest = &code[at..];
        let close = rest.find(") -|").expect("annotation after loop entry");
        assert!(
            code[..at].ends_with("( ") && rest[close + 1..].starts_with(&line_anno(line)),
            "loop entry should carry line {line}, got `{}`:\n{code}",
            &rest[..rest.len().min(80)]
        );
    }
}

/// BT-4401: the same for the `StateAcc` loops of an actor mutating its state
/// (the other `ConditionalLoop` lowerings), in a multi-line method.
#[test]
fn threaded_ir_state_loops_annotate_their_entry_apply() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: n :: Integer = 0\n",
        "  bump: k =>\n",
        "    1 to: k do: [:i | self.n := self.n + i].\n",
        "    [self.n > 100] whileTrue: [self.n := self.n - 1].\n",
        "    k timesRepeat: [self.n := self.n + 1].\n",
        "    self.n\n",
    );
    let code = codegen_with_lines(src);
    assert_loop_entry_lines(&code, &[4, 5, 6]);
    assert!(unannotated_dispatch_calls(&code).is_empty(), "{code}");
    assert_compiles_through_erlc("runner", &code);
}

#[test]
fn the_scanner_finds_static_named_loop_entries_not_back_edges() {
    let anno = "-| [3, {'file', \"a.bt\"}]";
    // A body `let` chain before the back-edge must not end the binding early.
    let bad = format!(
        "( letrec 'while'/1 = fun (S) -> let X = 1 in apply 'while'/1 (X) in apply 'while'/1 (S) {anno} )"
    );
    assert_eq!(unannotated_dispatch_calls(&bad).len(), 1);
    let good = format!(
        "( letrec 'while'/1 = fun (S) -> let X = 1 in apply 'while'/1 (X) in ( apply 'while'/1 (S) {anno} ) {anno} )"
    );
    assert!(unannotated_dispatch_calls(&good).is_empty());
    assert_eq!(loop_entry_applies(&good).len(), 1);
}

/// BT-4850: an actor method's self-send puts its line on the `safe_dispatch`
/// call and on the `reraise`/`raise` calls of its error clauses, so the frame of
/// the sending method reads `Class>>selector (file.bt:N)`, not a line-less
/// `dispatch`. The raising frame of a does-not-understand is the `raise` call.
#[test]
fn a_self_send_annotates_its_dispatch_and_error_calls_bt_4850() {
    let code = codegen_with_lines(concat!(
        "Actor subclass: Srv\n",
        "  state: value = 0\n\n",
        "  setup =>\n",
        "    self.value := 42\n\n",
        "  doWork =>\n",
        "    self setup\n",
        "    self.value\n",
    ));
    // `self setup` is on line 8 of the source.
    assert_every_node_annotated(&code, "call _ClassMod", 8);
    assert_every_node_annotated(&code, "call 'beamtalk_exception_handler':'reraise'(_SDErrorType", 8);
    assert_every_node_annotated(&code, "call 'beamtalk_error':'raise'(_SDErrorPlain", 8);
}

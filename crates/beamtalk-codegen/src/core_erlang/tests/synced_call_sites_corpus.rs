// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0137 Phase 2a (BT-4020): the invariant behind `UnbracketedUserCodeSend`,
//! enumerated by code generator site.
//!
//! In an actor instance method, no call that may
//! run user code (`send_user_code`) may be rendered into a `Document` outside a
//! `ThreadedIr` frame: it must be a `SyncedCall`. This probe compiles the
//! generated and statement-position corpora in count-only mode (the generator
//! records every call compiled outside a bracket window, with the `file:function`
//! of the lowering that compiled it) and compares the set of sites with
//! [`ALLOWED_SITES`]:
//!
//! - a site that is **not** listed is a new violation and fails the test, with
//!   an example program;
//! - a listed site the corpus no longer reaches fails the test too, so fixing a
//!   site requires removing it from the list.
//!
//! The list is the work queue of BT-4020: each entry is routed through the IR
//! in the node that owns it, until the list is empty.
//!
//! `BT4020_PROGRAMS=<n>` raises the number of generated programs (default 1024;
//! `BT4020_SITES_DUMP=1` prints every site with its count and an example).

use crate::core_erlang::dropped_assignment_probe::{PRELUDE, corpus};
use crate::core_erlang::synced_call::{SITE_LOG, SiteRecord, count_only};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::MessageSelector;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_core::test_helpers::test_support::draw_programs;
use beamtalk_core::unparse::unparse_module;
use std::collections::BTreeMap;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// The code generator sites known to compile a user-code call outside every
/// bracket (BT-4020). Empty is the goal.
const ALLOWED_SITES: &[&str] = &[];

/// Programs per generated module.
const BATCH: usize = 128;

fn programs() -> usize {
    std::env::var("BT4020_PROGRAMS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1024)
}

/// The generated programs as actor-class sources, `BATCH` run methods each.
fn generated_sources() -> Vec<String> {
    generated_sources_of(programs())
}

/// [`generated_sources`] for `n` programs. Shared with `paren_transparency`.
pub(super) fn generated_sources_of(n: usize) -> Vec<String> {
    let mut drawn = draw_programs("GenSitesProbe", n);
    let mut sources = Vec::new();
    while !drawn.is_empty() {
        let rest = drawn.split_off(drawn.len().min(BATCH));
        let mut batch = std::mem::replace(&mut drawn, rest);
        let mut module = batch.remove(0);
        for (i, program) in batch.into_iter().enumerate() {
            let mut run = program.classes[0].methods[0].clone();
            run.selector = MessageSelector::Unary(format!("run{}", i + 1).into());
            module.classes[0].methods.push(run);
        }
        sources.push(unparse_module(&module).replacen(
            "Object subclass: GenSitesProbe",
            "Actor subclass: GenSitesProbe",
            1,
        ));
    }
    sources
}

/// The statement-position corpus of `dropped_assignment_probe`, as actor methods.
pub(super) fn statement_sources() -> Vec<String> {
    corpus()
        .into_iter()
        .map(|body| {
            let mut indented = String::new();
            for line in format!("{PRELUDE}\n{body}").lines() {
                indented.push_str("    ");
                indented.push_str(line);
                indented.push('\n');
            }
            format!("Actor subclass: AgreeActor\n  state: n = 0\n\n  go =>\n{indented}")
        })
        .collect()
}

/// User-code expressions: calls that may run user code, in several shapes.
const PAYLOADS: &[&str] = &[
    "o foo",
    "o foo: 1",
    "a + b",
    "a < b",
    "self printString",
    "self bar",
    "o foo: (a + b)",
    "xs size",
    "o foo: (a < b)",
];

/// Constructs a payload `@P` can sit in: every position of every inlined
/// construct the code generator lowers specially, nested ones included.
const CONSTRUCTS: &[&str] = &[
    "@P",
    "x := @P",
    "self.n := @P",
    "^ @P",
    "a ifTrue: [@P] ifFalse: [0]",
    "a ifTrue: [@P]",
    "a ifFalse: [@P]",
    "(a ifTrue: [1] ifFalse: [2]) + (@P)",
    "(@P) ifTrue: [1] ifFalse: [2]",
    "a ifNil: [@P] ifNotNil: [:v | v]",
    "a and: [@P]",
    "(@P) and: [a]",
    "a or: [@P]",
    "[@P. a] whileTrue: [b]",
    "[a] whileTrue: [@P]",
    "[a] whileFalse: [@P]",
    "[@P] whileFalse: [b]",
    "1 to: 3 do: [:i | @P]",
    "1 to: 3 by: 2 do: [:i | @P]",
    "3 timesRepeat: [@P]",
    "xs do: [:e | @P]",
    "xs collect: [:e | @P]",
    "xs select: [:e | @P]",
    "xs reject: [:e | @P]",
    "xs inject: 0 into: [:acc :e | @P]",
    "xs detect: [:e | @P] ifNone: [nil]",
    "xs anySatisfy: [:e | @P]",
    "xs count: [:e | @P]",
    "xs doWithKey: [:k :e | @P]",
    "[@P] on: Error do: [:e | @P]",
    "[a] on: Error do: [:e | @P]",
    "[@P] ensure: [@P]",
    "[@P] value",
    "[:p | @P] value: 1",
    "[:p | p] value: (@P)",
    "d at: #k ifAbsent: [@P]",
    "d keysAndValuesDo: [:k :v | @P]",
    "o foo: (@P)",
    "a ifTrue: [1] ifFalse: [^0] printString",
    "(a ifTrue: [1] ifFalse: [2]) < (@P)",
    "[a] whileTrue: [b] printString",
    "(@P) foo",
    "#(1, (@P))",
    "#{#k => (@P)}",
    "o foo; bar: (@P)",
    "x := xs collect: [:e | @P]. x",
    "xs do: [:e | a ifTrue: [@P]]",
    "[a] whileTrue: [xs do: [:e | @P]]",
    "xs do: [:e | [@P] on: Error do: [:z | 0]]",
    "1 to: 3 do: [:i | xs do: [:e | @P]]",
    "xs do: [:e | (x := @P) > 0]",
    "[a] whileTrue: [(x := @P) > 0]",
    "1 to: 3 do: [:i | (x := @P) > 0]",
    "xs do: [:e | self.n := @P]",
    "[a] whileTrue: [self.n := @P]",
    "a ifTrue: [self.n := @P] ifFalse: [self.n := 0]",
    "[self.n := @P] on: Error do: [:e | 0]",
];

/// Statements ahead of the construct, so it is not always the first one.
const PREFIXES: &[&str] = &["", "t := 0.", "self.n := self.n + 1.", "x := o foo."];

/// The construct matrix, every payload x construct x prefix, as actor methods.
pub(super) fn construct_sources() -> Vec<String> {
    let mut out = Vec::new();
    for payload in PAYLOADS {
        for construct in CONSTRUCTS {
            for prefix in PREFIXES {
                let stmt = construct.replace("@P", payload);
                let body = format!("{prefix}\n    {stmt}");
                out.push(format!(
                    "Actor subclass: ConstructProbe\n  state: n = 0\n\n  bar => 1\n\n  go: o with: a with: b over: xs and: d =>\n    {body}\n"
                ));
            }
        }
    }
    out
}

thread_local! {
    /// Sources that did not parse (a probe that parses nothing proves nothing).
    static PARSE_FAILURES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

thread_local! {
    /// Sources codegen refused or panicked on.
    static COMPILE_FAILURES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Every gap the count-mode compile of `src` recorded.
fn gaps_of(src: &str) -> Vec<SiteRecord> {
    let _count = count_only();
    SITE_LOG.with(|log| log.borrow_mut().clear());
    let (module, diagnostics) = parse(lex_with_eof(src));
    if let Some(error) = diagnostics
        .iter()
        .find(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
    {
        PARSE_FAILURES.with(|n| n.set(n.get() + 1));
        if std::env::var_os("BT4020_PARSE_DUMP").is_some() {
            println!("PARSE {error:?}\n{src}");
        }
    }
    // A program another check rejects, or codegen refuses, records nothing.
    let compiled = catch_unwind(AssertUnwindSafe(|| {
        generate_module_with_warnings(&module, CodegenOptions::new("probe"))
    }));
    if !matches!(compiled, Ok(Ok(_))) {
        COMPILE_FAILURES.with(|n| n.set(n.get() + 1));
        if std::env::var_os("BT4020_PARSE_DUMP").is_some() {
            println!("COMPILE FAIL\n{src}");
        }
    }
    SITE_LOG.with(|log| std::mem::take(&mut *log.borrow_mut()))
}

/// site -> (count, selector, region, an example program).
fn observed_sites() -> BTreeMap<String, (usize, String, &'static str, String)> {
    let mut sites: BTreeMap<String, (usize, String, &'static str, String)> = BTreeMap::new();
    for src in generated_sources()
        .into_iter()
        .chain(statement_sources())
        .chain(construct_sources())
    {
        let records = gaps_of(&src);
        if records.is_empty() {
            continue;
        }
        // A batch holds many methods; the example is the whole source, trimmed.
        let example: String = src.lines().take(40).collect::<Vec<_>>().join("\n");
        for r in records {
            let example = format!("via {}\n{example}", r.chain);
            let entry = sites.entry(r.site.clone()).or_insert((
                0,
                r.selector.clone(),
                r.region.label(),
                example,
            ));
            entry.0 += 1;
        }
    }
    sites
}

#[test]
fn every_user_code_call_outside_the_ir_is_at_a_listed_site() {
    let sites = observed_sites();
    let total = construct_sources().len();
    let refused = COMPILE_FAILURES.with(std::cell::Cell::get);
    assert!(
        refused * 4 < total,
        "the corpora must compile: codegen refused {refused} sources ({total} constructs)"
    );
    let unparsed = PARSE_FAILURES.with(std::cell::Cell::get);
    assert!(
        unparsed * 4 < total,
        "the construct matrix must parse: {unparsed} of the sources did not ({total} constructs)"
    );
    if std::env::var_os("BT4020_SITES_DUMP").is_some() {
        for (site, (count, selector, region, example)) in &sites {
            println!(
                "SITE {count:>5}  {site}  ({region}, e.g. `{selector}`)\n      {}",
                example.lines().next().unwrap_or("")
            );
        }
    }
    let new: Vec<String> = sites
        .iter()
        .filter(|(site, _)| !ALLOWED_SITES.contains(&site.as_str()))
        .map(|(site, (count, selector, region, example))| {
            format!("{site}: {count} call(s), e.g. `{selector}` in a {region} region\n{example}")
        })
        .collect();
    assert!(
        new.is_empty(),
        "BT-4020: a user-code call is compiled outside a SyncedCall at a site not on the allow-list \
         (route it through the IR in the node that owns it):\n{}",
        new.join("\n---\n")
    );
    let stale: Vec<&&str> = ALLOWED_SITES
        .iter()
        .filter(|site| !sites.contains_key(**site))
        .collect();
    assert!(
        stale.is_empty(),
        "BT-4020: allow-listed sites the corpus no longer reaches (remove them): {stale:?}"
    );
}

/// The construct matrix compiled through `erlc`: a bracket must never produce
/// an unbound `State` version or any other `erlc` failure; failures are listed
/// with their source.
#[test]
fn the_construct_matrix_compiles_through_erlc() {
    if !super::erlc_available() {
        return;
    }
    let compile = |module: &beamtalk_core::ast::Module| {
        catch_unwind(AssertUnwindSafe(|| {
            crate::core_erlang::generate_module(module, CodegenOptions::new("probe"))
        }))
        .ok()
        .and_then(Result::ok)
    };
    let through_erlc = |code: &str| {
        catch_unwind(AssertUnwindSafe(|| {
            super::assert_compiles_through_erlc("probe", code);
        }))
        .is_ok()
    };
    // Every `BT4020_ERLC_STRIDE`-th construct (default 8): a full run is ~12 minutes.
    let stride = std::env::var("BT4020_ERLC_STRIDE")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(8);
    let mut failures: Vec<String> = Vec::new();
    for src in construct_sources().into_iter().step_by(stride) {
        let (module, _) = parse(lex_with_eof(&src));
        let Some(code) = compile(&module) else {
            continue;
        };
        if !through_erlc(&code) {
            failures.push(src);
        }
    }
    assert!(
        failures.is_empty(),
        "BT-4020: {} construct(s) do not compile through erlc:\n{}",
        failures.len(),
        failures.join("\n---\n")
    );
}

#[test]
fn the_probe_sees_a_missing_bracket() {
    // Fault injection: with brackets suppressed every user-code call is a gap, and
    // the probe names the site that compiled it.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  poke: o =>\n    o foo.\n    self.n\n";
    assert!(gaps_of(src).is_empty());
    crate::core_erlang::synced_call::FAULT_NO_BRACKETS.with(|f| f.set(true));
    let gaps = gaps_of(src);
    crate::core_erlang::synced_call::FAULT_NO_BRACKETS.with(|f| f.set(false));
    assert!(
        gaps.iter()
            .any(|g| g.selector == "foo" && g.site.contains(".rs:")),
        "{gaps:?}"
    );
}

/// Dev aid, a no-op unless `BT4020_DUMP_SRC=<file>` and `BT4020_DUMP_OUT=<file>` are set:
/// compiles the actor class in the first file and writes its
/// Core Erlang to the second, to read the code a corpus gap or miscompile produced.
#[test]
fn dump_a_source() {
    let (Some(src), Some(out)) = (
        std::env::var_os("BT4020_DUMP_SRC"),
        std::env::var_os("BT4020_DUMP_OUT"),
    ) else {
        return;
    };
    let src = std::fs::read_to_string(src).expect("BT4020_DUMP_SRC is readable");
    let (module, _) = parse(lex_with_eof(&src));
    let code = crate::core_erlang::generate_module(&module, CodegenOptions::new("probe"))
        .expect("codegen");
    std::fs::write(out, code).expect("BT4020_DUMP_OUT is writable");
}

#[test]
fn an_evaluated_read_only_literal_block_is_bracketed() {
    let src =
        "Actor subclass: Probe\n  state: n = 2\n\n  run =>\n    [self.n] value\n    self.n * 100\n";
    let gaps = gaps_of(src);
    assert!(gaps.is_empty(), "{gaps:#?}");
}

#[test]
fn the_runtime_dispatch_fallback_in_a_threaded_list_op_is_bracketed() {
    // A fold that threads `State` (it writes a field) and sends to an unknown receiver:
    // the list op's runtime-dispatch fallback and the send are both calls.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  run: xs with: o =>\n    xs do: [:x | self.n := self.n + 1. o process: x].\n    xs collect: [:x | self.n := self.n + 1. o process: x]\n";
    let gaps = gaps_of(src);
    assert!(gaps.is_empty(), "{gaps:#?}");
}

#[test]
fn error_sent_to_a_class_receiver_is_not_a_gap() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  run =>\n    self.n := 1.\n    Object error: \"boom\"\n";
    let gaps = gaps_of(src);
    assert!(gaps.is_empty(), "{gaps:#?}");
}

#[test]
fn a_send_scrutinee_of_a_threaded_match_is_bracketed() {
    // BT-4146 (nightly fuzzing, 2026-10-25): `t := 0` continued by `+x
    // match: [...]` parses as `t := ((0 + x) match: [...])` — a `match:`
    // whose scrutinee is a send that may run user code (the `+` coerces
    // through a user `plusFromNumber:` when `x` is not statically
    // numeric). The scrutinee must compile inside a bracket window like
    // any other send.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  run: x =>\n    t := 0\n    t := 0 + x match: [\n      1 -> (t := 11);\n      _ -> (t := 20)\n    ]\n    t\n";
    let gaps = gaps_of(src);
    assert!(gaps.is_empty(), "{gaps:#?}");
}

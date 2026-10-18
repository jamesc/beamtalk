// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0132 Phase 1d (BT-3872): the threaded entry of a value-type method.
//!
//! A value-type instance method with a threading parameter compiles to one body
//! over `Acc` (`'__t2_<sel>'/n+2`) and the plain `'<sel>'/n+1` as a wrapper over
//! it, sharing Phase 1c's lowering (`threaded_entry.rs`). These tests pin:
//!
//! - the emitted shapes (golden text), `'dispatch'/3` still calling the plain
//!   function, and a method without a threading parameter emitted as before;
//! - the value-type body being the class-method body for the same source, but
//!   for its receiver (`Self` / `ClassSelf`);
//! - the export list, the definitions, `'tier2_methods'` and the index being one
//!   answer, over fixtures, the runtime fixture and the stdlib;
//! - `SelfVt` beside `Acc` in a body that writes a field (`[SelfVt, Acc]`), over
//!   a generated corpus of use shapes × field-writing constructs, each checked
//!   with `assert_acc_linear`, the verifier (one `MethodBody` per body) and
//!   `erlc`;
//! - the shapes the index refuses (a `^` block, a use holding another
//!   parameter's use) compiling as plain methods, and the loud refusal of a use
//!   inside a construct.

use super::threaded_class_entry::{
    Entries, assert_acc_linear, entries_with, function_text, repo_path, try_codegen,
};
use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::selector_mangler::threaded_fn_name;
use crate::core_erlang::threaded_entry::forced_positions;
use crate::core_erlang::{CodeGenError, CodegenOptions, generate_module};
use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;
use std::collections::BTreeSet;
use std::fmt::Write as _;

fn instance_entries(code: &str) -> Entries {
    entries_with(code, "__t2_", "tier2_methods")
}

/// A `Value subclass:` (sealed: its self-sends bind statically) and an open
/// `Object subclass:` value type.
const VALUE_FIXTURE: &str = "sealed Value subclass: SealedValue
  field: k = 1

  twice: b => b value. b value

  each: items do: b => items do: b

  twiceVia: b => self twice: b

  firstPositive: b in: items =>
    b value.
    items do: [:x | x > 0 ifTrue: [^x]].
    nil

  answer => 42
";

const OBJECT_FIXTURE: &str = "Object subclass: OpenObject
  twice: b => b value. b value

  sealed sealedTwice: b => b value. b value

  viaOpen: b => self twice: b

  viaSealed: b => self sealedTwice: b

  run: a with: b => self sealedTwice: a. b value

  foo: x bar: y => x value. y

  both: a with: b => self foo: a bar: (b value)
";

/// `function_text` without the source-line annotations and with runs of
/// whitespace collapsed, so two bodies compare by their code alone.
fn normalized(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(i) = rest.find(" -| [") {
        out.push_str(&rest[..i]);
        let after = &rest[i + 5..];
        // `[line, {'file', "..."}]`: skip to the matching `]`.
        let mut depth = 1usize;
        let mut end = after.len();
        for (j, c) in after.char_indices() {
            match c {
                '[' => depth += 1,
                ']' => {
                    depth -= 1;
                    if depth == 0 {
                        end = j + 1;
                        break;
                    }
                }
                _ => {}
            }
        }
        rest = &after[end..];
    }
    out.push_str(rest);
    out.split_whitespace().collect::<Vec<_>>().join(" ")
}

// ─── The emitted shapes ────────────────────────────────────────────────────

#[test]
fn a_direct_invocation_is_a_body_over_acc_with_a_plain_wrapper() {
    let code = codegen(VALUE_FIXTURE);
    assert_eq!(
        normalized(function_text(&code, "'__t2_twice:'/3")),
        "'__t2_twice:'/3 = fun (Self, _b1, Acc@0) -> \
         let _Th3 = call 'beamtalk_dispatch':'call_threaded'(_b1, [], Acc@0) in \
         let _ThV4 = call 'erlang':'element'(1, _Th3) in \
         let Acc@1 = call 'erlang':'element'(2, _Th3) in \
         let _seq2 = _ThV4 in \
         let _Th6 = call 'beamtalk_dispatch':'call_threaded'(_b1, [], Acc@1) in \
         let _ThV7 = call 'erlang':'element'(1, _Th6) in \
         let Acc@2 = call 'erlang':'element'(2, _Th6) in \
         let _seq5 = _ThV7 in \
         let _ThAnswer8 = _seq5 in {_ThAnswer8, Acc@2}"
    );
    // The plain function keeps today's name and arity and wraps the body with
    // the `plain` sentinel (a Tier 2 block is refused, not run).
    assert_eq!(
        normalized(function_text(&code, "'twice:'/2")),
        "'twice:'/2 = fun (Self, _b1) -> \
         call 'erlang':'element'(1, apply '__t2_twice:'/3 (Self, _b1, 'plain'))"
    );
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn dispatch_keeps_calling_the_plain_function() {
    let code = codegen(VALUE_FIXTURE);
    let dispatch = function_text(&code, "'dispatch'/3");
    assert!(dispatch.contains("'test':'twice:'(Self, "), "{dispatch}");
    assert!(!dispatch.contains("__t2_"), "{dispatch}");
}

#[test]
fn a_method_without_a_threading_parameter_is_emitted_exactly_as_before() {
    let code = codegen(VALUE_FIXTURE);
    let alone = codegen(
        "sealed Value subclass: SealedValue
  field: k = 1

  answer => 42
",
    );
    assert_eq!(
        function_text(&alone, "'answer'/1"),
        function_text(&code, "'answer'/1")
    );
    assert!(!alone.contains("__t2_"), "{alone}");
    assert!(alone.contains("'tier2_methods' => ~{}~"), "{alone}");
}

#[test]
fn the_callable_of_a_collection_message_folds_over_acc() {
    let code = codegen(VALUE_FIXTURE);
    let each = function_text(&code, "'__t2_each:do:'/4");
    assert!(each.contains("fun (Self, _items"), "{each}");
    assert!(
        each.contains("call 'beamtalk_dispatch':'call_threaded'(_Callable"),
        "{each}"
    );
    assert!(each.contains("'lists':'foldl'"), "{each}");
    assert!(
        each.contains("let Acc@1 = call 'erlang':'element'(2, "),
        "{each}"
    );
}

#[test]
fn a_statically_bound_pass_on_calls_the_callee_entry_and_a_late_bound_one_is_plain() {
    let value = codegen(VALUE_FIXTURE);
    // A sealed class: `self twice: b` binds statically.
    let via = function_text(&value, "'__t2_twiceVia:'/3");
    assert!(
        via.contains("call 'test':'__t2_twice:'(Self, _Arg"),
        "{via}"
    );
    assert!(
        via.contains("let Acc@1 = call 'erlang':'element'(2, "),
        "{via}"
    );

    let object = codegen(OBJECT_FIXTURE);
    // A sealed method of an open class binds statically too.
    let sealed = function_text(&object, "'__t2_viaSealed:'/3");
    assert!(
        sealed.contains("call 'test':'__t2_sealedTwice:'(Self, _Arg"),
        "{sealed}"
    );
    // An open method may be overridden: today's late-bound send, `Acc`
    // unchanged (ADR 0132 Phase 2 resolves it at run time).
    let open = function_text(&object, "'__t2_viaOpen:'/3");
    assert!(
        open.contains("'beamtalk_message_dispatch':'send'(Self, 'twice:'"),
        "{open}"
    );
    assert!(open.contains("Acc@0}"), "{open}");
    assert!(!open.contains("Acc@1"), "{open}");
    assert_compiles_through_erlc("test", &object);
}

#[test]
fn the_value_type_body_is_the_class_method_body_but_for_its_receiver() {
    // One method source on each side of the same class: the shared lowering
    // gives one body text, modulo the receiver (`ClassSelf` / `Self`).
    for body in [
        "b value. b value",
        "items do: b",
        "r := b value: 3. r * 2",
        "(b value: 1) + (b value: 2)",
        "b value: 1; value: 2",
        "(b value: 1) > 0 ifTrue: [10] ifFalse: [20]",
        "items inject: 0 into: b",
        // (A root `^` differs only by the value-type method's own early-return
        // boundary, `let _EarlyResultN = V in`; the corpus below covers it.)
    ] {
        let class_code = codegen(&format!(
            "sealed Object subclass: Same\n  class sealed m: b in: items => {body}\n"
        ));
        let value_code = codegen(&format!(
            "sealed Object subclass: Same\n  m: b in: items => {body}\n"
        ));
        let class_body = normalized(function_text(&class_code, "'__t2_class_m:in:'/4"));
        let value_body = normalized(function_text(&value_code, "'__t2_m:in:'/4"));
        let strip = |text: &str| {
            let start = text.find("->").expect("fun header") + 2;
            text[start..]
                .trim_end_matches(" )")
                .trim()
                .replace("ClassSelf", "Self")
        };
        assert_eq!(strip(&class_body), strip(&value_body), "`{body}`");
    }
}

// ─── Exports, definitions, metadata and the index are one answer ────────────

/// `ThreadingIndex`'s instance-side answer for `src`.
fn index_instance_side(src: &str) -> BTreeSet<(String, Vec<usize>, usize)> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    ClassHierarchy::extract_class_infos(&module)
        .into_iter()
        .flat_map(|class| class.methods)
        .filter(|m| !m.threading_params.is_empty())
        .map(|m| (m.selector.to_string(), m.threading_params, m.arity))
        .collect()
}

/// The module defines, exports and advertises exactly the index's
/// instance-side entries, and no entry holds the NLR catch.
fn assert_entries_equal_the_index(src: &str, code: &str) {
    let index = index_instance_side(src);
    let entries = instance_entries(code);
    let advertised: BTreeSet<(String, String, Vec<usize>)> = index
        .iter()
        .map(|(sel, positions, _)| (sel.clone(), threaded_fn_name(sel), positions.clone()))
        .collect();
    assert_eq!(entries.advertised, advertised, "advertised\n{code}");
    let arity: BTreeSet<(String, usize)> = index
        .iter()
        .map(|(sel, _, arity)| (threaded_fn_name(sel), arity + 2))
        .collect();
    assert_eq!(entries.defined, arity, "defined\n{code}");
    assert_eq!(entries.exported, arity, "exported\n{code}");
    for (entry, arity) in &entries.defined {
        let text = function_text(code, &format!("'{entry}'/{arity}"));
        assert!(!text.contains("_NlrToken"), "{entry} holds an NLR catch");
    }
}

#[test]
fn tier2_methods_equals_the_threading_index_for_the_fixtures() {
    for src in [VALUE_FIXTURE, OBJECT_FIXTURE] {
        let code = codegen(src);
        assert_entries_equal_the_index(src, &code);
        assert_compiles_through_erlc("test", &code);
    }
    // `firstPositive:in:` holds a `^` block: no entry (`facts::body_has_block_nlr`).
    assert_eq!(index_instance_side(VALUE_FIXTURE).len(), 3);
    assert!(!codegen(VALUE_FIXTURE).contains("'__t2_firstPositive:in:'"));
}

#[test]
fn tier2_methods_equals_the_threading_index_for_the_runtime_fixture() {
    let src = std::fs::read_to_string(repo_path(
        "runtime/apps/beamtalk_runtime/test_fixtures/threaded_value_entries_fixture.bt",
    ))
    .expect("fixture");
    let code = codegen(&src);
    assert_entries_equal_the_index(&src, &code);
    assert!(index_instance_side(&src).len() >= 8);
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn an_annotated_entry_has_a_spec_over_the_acc_map() {
    let code = codegen(
        "sealed Object subclass: Typed
  twice: b :: Block -> Integer =>
    b value.
    b value
",
    );
    // The plain function's spec is unchanged; the entry's takes a map `Self`
    // first and `Acc` last, and answers `{Result, Acc1}`.
    assert!(code.contains("{'twice:', 2}"), "{code}");
    assert!(
        code.contains(
            "{'__t2_twice:', 3}, [{'type', 0, 'fun', [{'type', 0, 'product', \
             [{'type', 0, 'map', 'any'}, "
        ),
        "{code}"
    );
    assert!(
        code.contains(
            "{'type', 0, 'union', [{'type', 0, 'map', 'any'}, {'atom', 0, 'plain'}]}]}, \
             {'type', 0, 'tuple', [{'type', 0, 'integer', []}, {'type', 0, 'union', "
        ),
        "{code}"
    );
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn every_stdlib_instance_entry_is_defined_exported_and_advertised() {
    let dir = repo_path("stdlib/src");
    let mut compiled = 0usize;
    let mut entry_count = 0usize;
    for entry in std::fs::read_dir(&dir).expect("stdlib/src") {
        let path = entry.expect("dir entry").path();
        if path.extension().is_none_or(|e| e != "bt") {
            continue;
        }
        let src = std::fs::read_to_string(&path).expect("stdlib source");
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let options = CodegenOptions::new("bt@stdlib@probe").with_stdlib_mode(true);
        let Ok(generated) = generate_module(&module, options) else {
            continue;
        };
        compiled += 1;
        let entries = instance_entries(&generated);
        let advertised: BTreeSet<(String, usize)> = entries
            .advertised
            .iter()
            .map(|(_, f, _)| {
                let Some((_, arity)) = entries.defined.iter().find(|(name, _)| name == f) else {
                    panic!("{}: advertised `{f}` is not defined", path.display())
                };
                (f.clone(), *arity)
            })
            .collect();
        assert_eq!(entries.defined, advertised, "{}", path.display());
        assert_eq!(entries.exported, advertised, "{}", path.display());
        entry_count += advertised.len();
    }
    assert!(compiled > 80, "compiled only {compiled} stdlib modules");
    // `True>>ifTrue:`, `Object>>ifNotNil:`, `ChangeLog>>do:`, ...
    assert!(
        entry_count >= 15,
        "only {entry_count} stdlib instance entries"
    );
}

// ─── Shapes the index refuses compile as plain methods ─────────────────────

#[test]
fn a_use_holding_another_parameters_use_compiles_as_a_plain_method() {
    let code = codegen(OBJECT_FIXTURE);
    assert!(!code.contains("'__t2_both:with:'"), "{code}");
    assert!(code.contains("'both:with:'/3 = "), "{code}");
    // Two uses side by side still thread both.
    assert!(code.contains("'__t2_run:with:'/4 = "), "{code}");
    assert_entries_equal_the_index(OBJECT_FIXTURE, &code);
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn a_method_with_a_block_that_does_a_return_has_no_entry_and_compiles() {
    let src = "sealed Object subclass: Stale
  apply: b to: x => b value: x
  arg: b =>
    b value: [:x | ^x].
    nil
  fwd: b =>
    self apply: b to: [:x | ^x].
    nil
  late: b items: items =>
    b value.
    items do: [:x | ^x].
    nil
  inMatchArm: b x: x =>
    b value.
    x match: [1 -> ^0; _ -> 1]
";
    let code = codegen(src);
    for selector in ["arg:", "fwd:", "late:items:", "inMatchArm:x:"] {
        assert!(
            !code.contains(&format!("'__t2_{selector}'")),
            "`{selector}` must not get a threaded entry:\n{code}"
        );
    }
    assert!(code.contains("'__t2_apply:to:'"), "{code}");
    assert_entries_equal_the_index(src, &code);
    assert_compiles_through_erlc("test", &code);
}

// ─── SelfVt beside Acc ─────────────────────────────────────────────────────

/// One way `b` is used at the method root, with its minimum `Acc` steps (a
/// forward to the class side is a late-bound send, `Acc` unchanged).
const USES: &[(&str, usize)] = &[
    ("b value", 1),
    ("b value: 1", 1),
    ("b valueWithArguments: #(1, 2)", 1),
    ("x := b value: 1", 1),
    ("x := b value. x + 1", 1),
    ("(b value: 1) + (b value: 2)", 2),
    ("self note: (b value: 1)", 1),
    ("b value: 1; value: 2", 2),
    ("items do: b", 1),
    ("items collect: b", 1),
    ("items select: b", 1),
    ("items detect: b ifNone: [0]", 1),
    ("items count: b", 1),
    ("r := items inject: 0 into: b. r", 1),
    ("self twice: b", 1),
    ("ValueCorpus twice: b", 0),
    ("self.n := b value. self.n", 1),
    ("self.n := self.n + (b value: self.n). self.n", 1),
    (
        "(b value: 1) > 0 ifTrue: [self note: 1] ifFalse: [self note: 2]",
        1,
    ),
    ("(b value) ifTrue: [self note: 1]", 1),
    ("(b value) ifNil: [0] ifNotNil: [:v | v]", 1),
    ("^b value: 1", 1),
    ("^(b value: self.n) + (b value: 2)", 2),
];

/// Constructs that do not use `b`, most of them writing a field (`SelfVt`),
/// lowered exactly as in any value-type method wherever they sit around the use.
const AROUND: &[&str] = &[
    "self.n := self.n + 1",
    "#(1, 2) do: [:i | self.n := self.n + i]",
    "1 to: 3 do: [:i | self.n := self.n + i]",
    "[self.n < 3] whileTrue: [self.n := self.n + 1]",
    "f ifTrue: [self.n := 1] ifFalse: [self.n := 2]",
    "[self.n := 5] on: Error do: [:e | 0]",
    "t := 0. #(1, 2) do: [:i | t := t + i]",
    "k := items collect: [:y | y + self.n]. k size",
    "self.total := items inject: 0 into: [:a :y | a + y]",
];

fn corpus_class() -> (String, Vec<(String, usize)>) {
    let mut methods = String::new();
    let mut expected = Vec::new();
    for (u, (use_src, steps)) in USES.iter().enumerate() {
        let is_return = use_src.starts_with('^');
        let mut variants: Vec<(String, String)> = vec![("alone".into(), (*use_src).into())];
        for (a, around) in AROUND.iter().enumerate() {
            variants.push((format!("before{a}"), format!("{around}.\n    {use_src}")));
            if !is_return {
                variants.push((format!("after{a}"), format!("{use_src}.\n    {around}")));
                variants.push((
                    format!("between{a}"),
                    format!("{use_src}.\n    {around}.\n    {use_src}"),
                ));
            }
        }
        for (name, body) in variants {
            let copies = if name.starts_with("between") { 2 } else { 1 };
            let selector = format!("m{u}{name}");
            let _ = write!(
                methods,
                "  {selector}: b items: items flag: f =>\n    {body}\n\n"
            );
            expected.push((format!("{selector}:items:flag:"), steps * copies));
        }
    }
    let src = format!(
        "TestCase subclass: ValueCorpus
  field: n = 0
  field: total = 0

  sealed note: x => x

  sealed twice: b => b value. b value

  class sealed twice: b => b value. b value

{methods}"
    );
    (src, expected)
}

#[test]
fn every_use_shape_beside_every_field_writing_construct_threads_self_and_acc() {
    let (src, expected) = corpus_class();
    let code = try_codegen(&src).unwrap_or_else(|error| {
        let at = match &error {
            CodeGenError::UnsupportedFeature {
                span: Some(span), ..
            } => src[span.start() as usize..span.end() as usize].to_string(),
            _ => String::new(),
        };
        panic!("the corpus must compile: {error}\n{at}")
    });
    let index: BTreeSet<String> = index_instance_side(&src)
        .into_iter()
        .map(|(sel, _, _)| sel)
        .collect();
    let mut self_threaded = 0usize;
    for (selector, steps) in &expected {
        assert!(index.contains(selector), "{selector} must thread");
        let arity = selector.matches(':').count() + 2;
        let entry = threaded_fn_name(selector);
        let text = function_text(&code, &format!("'{entry}'/{arity}"));
        assert_acc_linear(text, selector);
        let binds = text.matches("let Acc@").count();
        assert!(
            binds >= *steps,
            "{selector}: expected at least {steps} Acc steps, saw {binds}\n{text}"
        );
        // A field write threads `Self` beside `Acc` in the same body.
        self_threaded += usize::from(text.contains("let Self1 ="));
        let plain = function_text(&code, &format!("'{selector}'/{}", arity - 1));
        assert!(
            plain.contains(&format!("apply '{entry}'/{arity}")),
            "{plain}"
        );
    }
    assert!(
        self_threaded * 2 > expected.len(),
        "most corpus bodies write a field ({self_threaded}/{})",
        expected.len()
    );
    assert_entries_equal_the_index(&src, &code);
    // Every body was verified as one `MethodBody` over `[SelfVt, Acc]` (a
    // violation hard-fails a debug build), and the module is valid Core Erlang.
    assert_compiles_through_erlc("test", &code);
}

// ─── The loud refusal of every use the predicate does not admit ────────────

/// Compiles `body` forced to have a threading parameter `b` (position 0) in a
/// value type that writes a field, and returns the refusal.
fn refusal_for(body: &str) -> String {
    refusal_in(&format!(
        "TestCase subclass: Forced\n  field: n = 0\n  run: b items: items flag: f =>\n    {body}\n"
    ))
}

/// Compiles `src` with `run:items:flag:` forced to thread `b` (position 0) and
/// returns the refusal, or the verifier's report (a hard failure in a debug
/// build).
fn refusal_in(src: &str) -> String {
    let result = forced_positions::with(&[("run:items:flag:", &[0])], || {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| try_codegen(src)))
    });
    match result {
        Ok(Ok(code)) => panic!(
            "compiled without a refusal:\n{}",
            function_text(&code, "'__t2_run:items:flag:'/5")
        ),
        Ok(Err(error)) => format!("{error}"),
        Err(payload) => payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default(),
    }
}

#[test]
fn a_return_block_forced_into_a_threaded_body_is_reported_by_the_verifier() {
    // The index never admits it (`facts::body_has_block_nlr`); forced past it,
    // the body's one `MethodBody` verification is the backstop, whether the
    // body threads `[Acc]` or `[SelfVt, Acc]`.
    for (header, write) in [
        ("Value subclass: Nlr\n  field: k = 0\n", ""),
        (
            "TestCase subclass: Nlr\n  field: n = 0\n",
            "self.n := 1.\n    ",
        ),
    ] {
        let message = refusal_in(&format!(
            "{header}  run: b items: items flag: f =>\n    {write}b value.\n    \
             items do: [:x | ^x].\n    nil\n"
        ));
        assert!(message.contains("NlrInThreadedBody"), "{message}");
    }
}

#[test]
fn a_use_inside_a_construct_is_refused_never_dropped() {
    for body in [
        "items do: [:y | self.n := self.n + (b value: y)]",
        "items do: [:y | b value: y]",
        "1 to: 3 do: [:i | self.n := b value: i]",
        "[self.n < 3] whileTrue: [b value. self.n := self.n + 1]",
        "f ifTrue: [self.n := b value] ifFalse: [0]",
        "f ifTrue: [b value]",
        "[b value. self.n := 1] on: Error do: [:e | 0]",
        "[self.n := 1] on: Error do: [:e | b value]",
        "[b value] ensure: [self.n := 1]",
        "g := [b value]. g value",
        "x := b. x value",
    ] {
        let message = refusal_for(body);
        assert!(
            message.contains("threading block parameter")
                || message.contains("ConstructFamilyDropped"),
            "`{body}` was refused with an unrelated message: {message}"
        );
    }
}

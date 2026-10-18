// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0132 Phase 1c (BT-3871): the threaded entry of a class method.
//!
//! A class method with a threading parameter (`MethodInfo::threading_params`,
//! computed once by `beamtalk-core`'s `ThreadingIndex`) compiles to one body
//! over `Acc` (`'__t2_class_<sel>'/n+2`) and the plain `'class_<sel>'/n+1` as a
//! wrapper over it. These tests pin:
//!
//! - the emitted shapes (golden text) and that a method without a threading
//!   parameter is emitted exactly as before;
//! - the metadata, the export list, the definitions and the specs all being
//!   one answer, equal to the index's, over a fixture and over the stdlib;
//! - the NLR row as Phase 1c ships it: a method whose body holds a block that
//!   does `^` has no threaded entry and compiles as before (its `^` could
//!   answer a stale `Acc`; carrying the live one is Phase 3);
//! - `Acc`'s spelling never colliding with a Beamtalk local;
//! - `Acc` being a family of a construct only where a use is (the
//!   generalisation of the single-family consumers: no construct a threaded
//!   body contains carries or drops it silently), over a generated corpus of
//!   use shapes × surrounding constructs;
//! - the loud refusal of every use the predicate does not admit (forced past
//!   the predicate by `forced_positions`).

use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::threaded_entry::forced_positions;
use crate::core_erlang::{CodeGenError, CodegenOptions, generate_module};
use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;
use std::collections::BTreeSet;
use std::fmt::Write as _;

/// `generate_module` over `src` as a single module named `test`.
pub(super) fn try_codegen(src: &str) -> Result<String, CodeGenError> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    generate_module(&module, CodegenOptions::new("test"))
}

/// The text of the module-level function that starts with `header` (e.g.
/// `'__t2_class_twice:'/3`), up to the next top-level function.
pub(super) fn function_text<'a>(code: &'a str, header: &str) -> &'a str {
    let start = code
        .find(&format!("\n{header} = fun"))
        .unwrap_or_else(|| panic!("no function `{header}` in:\n{code}"))
        + 1;
    let rest = &code[start..];
    let end = rest.find("\n\n'").map_or(rest.len(), |i| i);
    &rest[..end]
}

const FIXTURE: &str = "sealed Object subclass: ThreadedFixture
  class sealed twice: b => b value. b value

  class sealed each: items do: b => items do: b

  class sealed twiceVia: b => self twice: b

  class sealed callThen: b firstPositiveIn: items =>
    b value.
    items do: [:x | x > 0 ifTrue: [^x]].
    nil

  class sealed answer => 42

  class sealed plain: n => n + 1
";

// ─── The emitted shapes ────────────────────────────────────────────────────

#[test]
fn a_direct_invocation_is_a_body_over_acc_with_a_plain_wrapper() {
    let code = codegen(FIXTURE);
    // The body: each invocation is `call_threaded/3` with the current `Acc`
    // version, destructured into the value and the next version; the answer is
    // `{Result, AccN}`.
    assert_eq!(
        function_text(&code, "'__t2_class_twice:'/3"),
        "'__t2_class_twice:'/3 = fun (ClassSelf, _b1, Acc@0) ->\n    \
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
    // The plain entry: today's name and arity, over the body with the plain-entry
    // sentinel naming the method (no caller map; a Tier 2 block is refused, not
    // run).
    assert_eq!(
        function_text(&code, "'class_twice:'/2"),
        "'class_twice:'/2 = fun (ClassSelf, _b1) ->\n    \
         call 'erlang':'element'(1, apply '__t2_class_twice:'/3 \
         (ClassSelf, _b1, {'plain', 'ThreadedFixture class', 'twice:'}))"
    );
}

#[test]
fn the_callable_of_a_collection_message_folds_over_acc_gated_by_call_threaded() {
    let code = codegen(FIXTURE);
    let each = function_text(&code, "'__t2_class_each:do:'/4");
    // The ADR 0128 fold, seeded from the entry's `Acc`, with the one tier gate
    // and no inline arity discrimination.
    assert!(each.contains("= fun (ClassSelf, _items"), "{each}");
    assert!(each.contains("Acc@0) ->"), "{each}");
    assert!(
        each.contains("call 'beamtalk_dispatch':'call_threaded'(_Callable"),
        "{each}"
    );
    assert!(!each.contains("'is_function'"), "{each}");
    assert!(each.contains("'lists':'foldl'"), "{each}");
    assert!(
        each.contains("let Acc@1 = call 'erlang':'element'(2, "),
        "{each}"
    );
    assert!(
        each.ends_with("{_ThAnswer") || each.contains(", Acc@1}"),
        "{each}"
    );
}

#[test]
fn a_same_class_pass_on_calls_the_callee_entry_with_the_current_acc() {
    let code = codegen(FIXTURE);
    let via = function_text(&code, "'__t2_class_twiceVia:'/3");
    assert!(
        via.contains("call 'test':'__t2_class_twice:'(ClassSelf, _Arg"),
        "{via}"
    );
    assert!(via.contains(", Acc@0)"), "{via}");
    assert!(
        via.contains("let Acc@1 = call 'erlang':'element'(2, "),
        "{via}"
    );
    // The plain wrapper still calls only its own body.
    let plain = function_text(&code, "'class_twiceVia:'/2");
    assert!(plain.contains("apply '__t2_class_twiceVia:'/3"), "{plain}");
}

#[test]
fn a_method_without_a_threading_parameter_is_emitted_exactly_as_before() {
    let code = codegen(FIXTURE);
    assert_eq!(
        function_text(&code, "'class_answer'/1"),
        "'class_answer'/1 = fun (ClassSelf) ->\n    42"
    );
    assert!(!code.contains("__t2_class_answer"), "{code}");
    assert!(!code.contains("__t2_class_plain"), "{code}");
    // Same text as a module that has no threaded method at all.
    let alone = codegen(
        "sealed Object subclass: ThreadedFixture
  class sealed answer => 42
",
    );
    assert_eq!(
        function_text(&alone, "'class_answer'/1"),
        function_text(&code, "'class_answer'/1")
    );
    assert!(!alone.contains("__t2_class"), "{alone}");
}

#[test]
fn a_block_parameter_that_is_stored_has_no_entry() {
    let code = codegen(
        "sealed Object subclass: Keeper
  class sealed keep: b => #(1, 2) collect: [:x | b]
  class sealed hold: b => #(b)
",
    );
    assert!(!code.contains("__t2_class"), "{code}");
    assert!(code.contains("'tier2_class_methods' => ~{}~"), "{code}");
}

// ─── Metadata, exports, definitions and specs are one answer ───────────────

/// The `(selector, entry function, arity)` triples the module defines, exports
/// and advertises, so each can be compared with the others and the index.
pub(super) struct Entries {
    pub(super) defined: BTreeSet<(String, usize)>,
    pub(super) exported: BTreeSet<(String, usize)>,
    pub(super) advertised: BTreeSet<(String, String, Vec<usize>)>,
}

fn entries_of(code: &str) -> Entries {
    entries_with(code, "__t2_class_", "tier2_class_methods")
}

/// The entries named `<prefix><sel>` the module defines and exports, and those
/// its `__beamtalk_meta/0` lists under `meta_key`. A class-side entry's prefix
/// (`__t2_class_`) is itself prefixed by the instance side's (`__t2_`), so an
/// instance-side scan excludes the class side's names.
pub(super) fn entries_with(code: &str, prefix: &str, meta_key: &str) -> Entries {
    let other_side = |name: &str| prefix == "__t2_" && name.starts_with("class_");
    let mut defined = BTreeSet::new();
    for line in code.lines() {
        // A value-type function is annotated: `'f'/N = ( fun ...`.
        if let Some(rest) = line.strip_prefix(&format!("'{prefix}"))
            && let Some((name, tail)) = rest.split_once("'/")
            && let Some((arity, after)) = tail.split_once(' ')
            && (after.starts_with("= fun") || after.starts_with("= ( fun"))
            && !other_side(name)
        {
            defined.insert((format!("{prefix}{name}"), arity.parse().unwrap()));
        }
    }
    let header_end = code.find("attributes").expect("module header");
    let header = &code[..header_end];
    let mut exported = BTreeSet::new();
    for part in header.split(", ") {
        let part = part.trim_start_matches(['[', ' ']);
        if let Some(rest) = part.strip_prefix(&format!("'{prefix}"))
            && let Some((name, arity)) = rest.split_once("'/")
            && !other_side(name)
        {
            let arity: String = arity.chars().take_while(char::is_ascii_digit).collect();
            exported.insert((format!("{prefix}{name}"), arity.parse().unwrap()));
        }
    }
    let mut advertised = BTreeSet::new();
    let open = format!("'{meta_key}' => ~{{");
    if let Some(start) = code.find(&open) {
        let body = &code[start + open.len()..];
        let body = &body[..body.find("}~").unwrap_or(0)];
        // 'sel' => {'entry', [0, 1]}, ...
        for entry in body.split("}, ") {
            let entry = entry.trim_end_matches('}');
            let Some((selector, rest)) = entry.split_once("' => {'") else {
                continue;
            };
            let selector = selector.trim_start_matches('\'').to_string();
            let (entry_fn, positions) = rest.split_once("', [").unwrap();
            let positions: Vec<usize> = positions
                .trim_end_matches(']')
                .split(", ")
                .filter(|p| !p.is_empty())
                .map(|p| p.parse().unwrap())
                .collect();
            advertised.insert((selector, entry_fn.to_string(), positions));
        }
    }
    Entries {
        defined,
        exported,
        advertised,
    }
}

/// `ThreadingIndex`'s answer for `src`'s class-side methods, by way of the
/// cross-file `ClassInfo` (`MethodInfo::threading_params`).
fn index_class_side(src: &str) -> BTreeSet<(String, Vec<usize>, usize)> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    ClassHierarchy::extract_class_infos(&module)
        .into_iter()
        .flat_map(|class| class.class_methods)
        .filter(|m| !m.threading_params.is_empty())
        .map(|m| (m.selector.to_string(), m.threading_params, m.arity))
        .collect()
}

/// Asserts that, for `src`, the entries the module defines, exports and
/// advertises are exactly the index's.
fn assert_entries_equal_the_index(src: &str, code: &str) {
    let index = index_class_side(src);
    let entries = entries_of(code);
    let expected_advertised: BTreeSet<(String, String, Vec<usize>)> = index
        .iter()
        .map(|(sel, positions, _)| {
            (
                sel.clone(),
                crate::core_erlang::selector_mangler::threaded_class_fn_name(sel),
                positions.clone(),
            )
        })
        .collect();
    assert_eq!(
        entries.advertised, expected_advertised,
        "advertised\n{code}"
    );
    // The entry takes `ClassSelf`, the arguments and `Acc`.
    let expected_arity: BTreeSet<(String, usize)> = index
        .iter()
        .map(|(sel, _, arity)| {
            (
                crate::core_erlang::selector_mangler::threaded_class_fn_name(sel),
                arity + 2,
            )
        })
        .collect();
    assert_eq!(entries.defined, expected_arity, "defined\n{code}");
    assert_eq!(entries.exported, expected_arity, "exported\n{code}");
    assert_no_entry_needs_nlr(src, code);
}

/// The index and codegen's NLR decision cannot drift: no method the index
/// marks threading is one codegen wraps in an NLR catch (`needs_nlr`, from
/// `facts::body_has_block_nlr`), and no emitted threaded entry holds one.
fn assert_no_entry_needs_nlr(src: &str, code: &str) {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let threading: BTreeSet<String> = index_class_side(src)
        .into_iter()
        .map(|(sel, _, _)| sel)
        .collect();
    for class in &module.classes {
        for method in &class.class_methods {
            let selector = method.selector.name().to_string();
            if threading.contains(&selector) {
                assert!(
                    !beamtalk_core::semantic_analysis::facts::body_has_block_nlr(&method.body),
                    "`{selector}` threads but needs the NLR catch"
                );
                let entry = crate::core_erlang::selector_mangler::threaded_class_fn_name(&selector);
                let header = format!("'{entry}'/{}", method.parameters.len() + 2);
                let text = function_text(code, &header);
                // The method's own NLR boundary mints `_NlrToken` (an
                // `on:do:` merely re-raises a `'$bt_nlr'` throw).
                assert!(
                    !text.contains("_NlrToken"),
                    "`{selector}`'s threaded entry holds an NLR catch:\n{text}"
                );
            }
        }
    }
}

#[test]
fn tier2_class_methods_equals_the_threading_index_for_a_fixture_class() {
    let code = codegen(FIXTURE);
    assert_entries_equal_the_index(FIXTURE, &code);
    // `callThen:firstPositiveIn:` holds a `^` block: no entry (`facts::body_has_block_nlr`).
    assert_eq!(index_class_side(FIXTURE).len(), 3);
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn tier2_class_methods_equals_the_threading_index_for_the_runtime_fixture() {
    let src = std::fs::read_to_string(repo_path(
        "runtime/apps/beamtalk_runtime/test_fixtures/threaded_class_entries_fixture.bt",
    ))
    .expect("fixture");
    let code = codegen(&src);
    assert_entries_equal_the_index(&src, &code);
    assert!(index_class_side(&src).len() >= 9);
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn every_module_kind_emits_exports_and_entries_alike() {
    // The exports, definitions and metadata are built per module kind; the
    // entry must be exported wherever it is defined (an unexported entry would
    // never resolve at run time).
    for (kind, header) in [
        ("object", "sealed Object subclass: Kinds"),
        ("value", "Value subclass: Kinds"),
        ("actor", "Actor subclass: Kinds"),
        ("sealed actor", "sealed Actor subclass: Kinds"),
    ] {
        let src = format!(
            "{header}
  state: n = 0

  class sealed twice: b => b value. b value

  class sealed each: items do: b => items do: b
"
        );
        let code = codegen(&src);
        assert_entries_equal_the_index(&src, &code);
        assert_eq!(entries_of(&code).defined.len(), 2, "{kind}\n{code}");
        assert_compiles_through_erlc("test", &code);
    }
}

pub(super) fn repo_path(rel: &str) -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(std::path::Path::parent)
        .expect("repo root")
        .join(rel)
}

#[test]
fn an_annotated_entry_has_a_spec_over_the_acc_map() {
    let src = "sealed Object subclass: Typed
  class sealed twice: b :: Block -> Integer =>
    b value.
    b value
";
    let code = codegen(src);
    // The plain wrapper's spec is unchanged; the entry's takes `Acc` last and
    // answers `{Result, Acc1}`.
    assert!(code.contains("{'class_twice:', 2}"), "{code}");
    assert!(
        code.contains(
            "{'__t2_class_twice:', 3}, [{'type', 0, 'fun', [{'type', 0, 'product', \
             [{'type', 0, 'any', []}, "
        ),
        "{code}"
    );
    // `Acc` is the caller's map or the plain-entry sentinel of the plain wrapper.
    assert!(
        code.contains(
            "{'type', 0, 'union', [{'type', 0, 'map', 'any'}, {'type', 0, 'tuple', \
             [{'atom', 0, 'plain'}, {'type', 0, 'atom', []}, {'type', 0, 'atom', []}]}]}]}, \
             {'type', 0, 'tuple', [{'type', 0, 'integer', []}, {'type', 0, 'union', "
        ),
        "{code}"
    );
    // erlc type-checks the attribute shape.
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn every_stdlib_class_method_entry_is_defined_exported_advertised_and_in_the_index() {
    let dir = repo_path("stdlib/src");
    let mut compiled = 0usize;
    let mut with_entries = 0usize;
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
        // The stdlib compile builds its index with `stdlib_spawns_block`, so the
        // module's own answer is the one `generate_module` used; compare the
        // module's definitions with its advertised map and its exports.
        let entries = entries_of(&generated);
        let advertised: BTreeSet<(String, usize)> = entries
            .advertised
            .iter()
            .map(|(_, entry_fn, _)| entry_fn.clone())
            .map(|f| {
                let Some((_, arity)) = entries.defined.iter().find(|(name, _)| *name == f) else {
                    panic!("{}: advertised `{f}` is not defined", path.display())
                };
                let arity = *arity;
                (f, arity)
            })
            .collect();
        assert_eq!(entries.defined, advertised, "{}", path.display());
        assert_eq!(entries.exported, advertised, "{}", path.display());
        entry_count += advertised.len();
        with_entries += usize::from(!advertised.is_empty());
    }
    assert!(compiled > 80, "compiled only {compiled} stdlib modules");
    println!("{with_entries} stdlib modules carry {entry_count} class-side threaded entries");
}

// ─── The NLR row ───────────────────────────────────────────────────────────

/// A class whose every method other than `apply:to:` has a block literal that
/// does `^` (`facts::body_has_block_nlr`): an argument of the use itself
/// (invocation, forward, fold handler), a stored block, a block kept in a
/// collection, a block chosen by a conditional, and a `^` that runs in place.
/// None gets a threaded entry (its `^` could answer a stale `Acc`); each
/// compiles as a plain method, exactly as before ADR 0132.
const NLR_BLOCKS: &str = "sealed Object subclass: Stale
  class sealed apply: b to: x => b value: x
  class sealed arg: b =>
    b value: [:x | ^x].
    nil
  class sealed fwd: b =>
    self apply: b to: [:x | ^x].
    nil
  class sealed fold: b items: items =>
    items detect: b ifNone: [^0].
    nil
  class sealed stale: b items: items =>
    f := [:x | ^x].
    b value.
    f value: 1
  class sealed kept: b items: items =>
    k := items collect: [:i | [^i]].
    b value.
    k first value
  class sealed chosen: b flag: c =>
    f := c ifTrue: [[^1]] ifFalse: [[^2]].
    b value.
    f value
  class sealed inPlace: b flag: c =>
    c ifTrue: [^0].
    b value
  class sealed late: b items: items =>
    b value.
    items do: [:x | ^x].
    nil
  class sealed inMatchArm: b x: x =>
    b value.
    x match: [1 -> ^0; _ -> 1]
";

#[test]
fn a_method_with_a_block_that_does_a_return_has_no_entry_and_compiles() {
    let code = codegen(NLR_BLOCKS);
    for selector in [
        "arg:",
        "fwd:",
        "fold:items:",
        "stale:items:",
        "kept:items:",
        "chosen:flag:",
        "inPlace:flag:",
        "late:items:",
        "inMatchArm:x:",
    ] {
        assert!(
            !code.contains(&format!("'__t2_class_{selector}'")),
            "`{selector}` must not get a threaded entry:\n{code}"
        );
        assert!(code.contains(&format!("'class_{selector}'")), "{code}");
    }
    // The callee the forward reaches still threads.
    assert!(code.contains("'__t2_class_apply:to:'"), "{code}");
    assert_entries_equal_the_index(NLR_BLOCKS, &code);
    assert_compiles_through_erlc("test", &code);
}

// ─── Acc's spelling ────────────────────────────────────────────────────────

#[test]
fn acc_never_collides_with_a_beamtalk_local_named_acc() {
    // `to_core_var` capitalises a local: `acc` -> `Acc`, `acc0` -> `Acc0`,
    // `acc1` -> `Acc1`; the body's `Acc@N` is none of them.
    let src = "sealed Object subclass: Collide
  class sealed collide: b =>
    acc := 1.
    acc0 := 2.
    acc1 := 3.
    b value.
    r := b value: acc + acc0 + acc1.
    acc2 := r.
    b value.
    acc + acc0 + acc1 + acc2
";
    let code = codegen(src);
    let body = function_text(&code, "'__t2_class_collide:'/3");
    for local in ["Acc =", "Acc0 =", "Acc1 =", "Acc2 ="] {
        assert!(body.contains(&format!("let {local}")), "{local}\n{body}");
    }
    for version in ["Acc@0", "Acc@1", "Acc@2", "Acc@3"] {
        assert!(body.contains(version), "{version}\n{body}");
    }
    // The spellings are disjoint as sets of names.
    assert!(
        !body.contains("Acc@ ") && !body.contains("let Acc@0"),
        "{body}"
    );
    assert_compiles_through_erlc("test", &code);
}

// ─── Acc is a family of a construct only where a use is ────────────────────

#[test]
fn acc_is_a_family_of_a_body_only_where_it_holds_a_use() {
    use crate::core_erlang::CoreErlangGenerator;
    use crate::core_erlang::threaded_ir::VersionPrefix;
    use beamtalk_core::ast::{Block, Expression};

    // `[b value]` holds a use of the threading parameter `b`; `[1 + 2]` none.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(
        "Object subclass: Probe
  class go: b =>
    [b value] value.
    [1 + 2] value
",
    );
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let method = &module.classes[0].class_methods[0];
    let blocks: Vec<&Block> = method
        .body
        .iter()
        .map(|s| match &s.expression {
            Expression::MessageSend { receiver, .. } => match receiver.as_ref() {
                Expression::Block(block) => block,
                other => panic!("not a block: {other:?}"),
            },
            other => panic!("not a send: {other:?}"),
        })
        .collect();

    let mut generator = CoreErlangGenerator::new("probe");
    generator.set_in_class_method(true);
    generator.begin_threaded_body(method, &[0]);
    let eligible = generator.eligible_families();
    assert_eq!(eligible, vec![VersionPrefix::Acc]);
    assert_eq!(
        generator
            .body_threaded_families(blocks[0], &eligible)
            .as_slice(),
        &[VersionPrefix::Acc],
        "a construct holding a use carries Acc"
    );
    assert!(
        generator
            .body_threaded_families(blocks[1], &eligible)
            .as_slice()
            .is_empty(),
        "a construct holding no use is lowered as in a method with no threading parameter"
    );
    generator.end_threaded_body();
    assert!(
        !generator.eligible_families().contains(&VersionPrefix::Acc),
        "the next method inherits no Acc"
    );
}

// ─── The generated corpus: use shapes × surrounding constructs ─────────────

/// One way a threading parameter `b` is used at the method root, with the
/// number of `call_threaded/3` steps it makes (a collection HOM is one).
const USES: &[(&str, usize)] = &[
    ("b value", 1),
    ("b value: 1", 1),
    ("b value: 1 value: 2", 1),
    ("b valueWithArguments: #(1, 2)", 1),
    ("x := b value: 1", 1),
    ("x := b value. x + 1", 1),
    ("(b value: 1) + (b value: 2)", 2),
    ("self note: (b value: 1)", 1),
    ("self note: (b value: 1) with: (b value: 2)", 2),
    ("b value: 1; value: 2", 2),
    ("items do: b", 1),
    ("items collect: b", 1),
    ("items select: b", 1),
    ("items detect: b", 1),
    ("items detect: b ifNone: [0]", 1),
    ("items count: b", 1),
    ("items anySatisfy: b", 1),
    ("items allSatisfy: b", 1),
    ("r := items inject: 0 into: b. r", 1),
    ("self twice: b", 1),
    ("ThreadedCorpus twice: b", 1),
    ("self.total := b value. self.total", 1),
    (
        "(b value: 1) > 0 ifTrue: [self note: 1] ifFalse: [self note: 2]",
        1,
    ),
    ("(b value) ifTrue: [self note: 1]", 1),
    ("((b value: 1) > 0) and: [f]", 1),
    ("(b value: 1) isNil ifTrue: [self note: 1]", 1),
    ("(b value) ifNil: [0] ifNotNil: [:v | v]", 1),
    ("^b value: 1", 1),
    ("^(b value: 1) + (b value: 2)", 2),
];

/// Constructs that do not use `b` and are lowered exactly as in any class
/// method, wherever they sit around the use.
const AROUND: &[&str] = &[
    "#(1, 2) do: [:i | i]",
    "1 to: 3 do: [:i | i]",
    "[n < 3] whileTrue: [n := n + 1]",
    "[1 / 0] on: Error do: [:e | 0]",
    "[nil] ensure: [nil]",
    "f ifTrue: [self note: 1] ifFalse: [self note: 2]",
    // (A block that does `^` gives the method no entry at all: `NLR_BLOCKS`.)
    "n := n + 1",
    "total := 0. #(1, 2) do: [:i | total := total + i]",
    "k := items collect: [:y | y + 1]. k size",
];

fn corpus_class() -> (String, Vec<(String, usize)>) {
    let mut methods = String::new();
    let mut expected = Vec::new();
    for (u, (use_src, steps)) in USES.iter().enumerate() {
        let is_return = use_src.starts_with('^');
        // The use alone, then with each construct before and after it (a `^`
        // use ends the method, so nothing follows it).
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
            // The method must not end in a bare `x := ...` (it answers the value).
            let selector = format!("m{u}{name}");
            let _ = write!(
                methods,
                "  class sealed {selector}: b items: items flag: f =>\n    n := 0.\n    {body}\n\n"
            );
            expected.push((format!("{selector}:items:flag:"), steps * copies));
        }
    }
    let src = format!(
        "Object subclass: ThreadedCorpus
  classState: total = 0

  class sealed note: x => x

  class sealed note: x with: y => x

  class sealed twice: b => b value. b value

{methods}"
    );
    (src, expected)
}

/// Every `Acc@N` bound in `text` is bound once, in order from 1, before any use
/// of it, and the answer tuple carries the last one bound.
pub(super) fn assert_acc_linear(text: &str, what: &str) {
    let mut bound: Vec<usize> = Vec::new();
    let mut rest = text;
    let mut offset = 0usize;
    let mut last_answer: Option<usize> = None;
    while let Some(i) = rest.find("Acc@") {
        let after = &rest[i + 4..];
        let digits: String = after.chars().take_while(char::is_ascii_digit).collect();
        let version: usize = digits.parse().unwrap();
        let binding = text[..offset + i].ends_with("let ");
        if binding {
            assert_eq!(
                version,
                bound.len() + 1,
                "{what}: Acc@{version} bound out of order (bound so far: {bound:?})\n{text}"
            );
            bound.push(version);
        } else {
            assert!(
                version == 0 || bound.contains(&version),
                "{what}: Acc@{version} used before it is bound (bound: {bound:?})\n{text}"
            );
            let after_digits = &after[digits.len()..];
            if after_digits.starts_with('}') && text[..offset + i].contains("{_ThAnswer") {
                last_answer = Some(version);
            }
        }
        offset += i + 4 + digits.len();
        rest = &after[digits.len()..];
    }
    if let Some(answer) = last_answer {
        // `{_ThAnswerN, Acc@k}` closes the body: the last version bound on the
        // normal path (a `^` may throw an earlier one).
        assert_eq!(
            answer,
            bound.len(),
            "{what}: the answer must carry the last Acc version\n{text}"
        );
    }
}

#[test]
fn every_use_shape_beside_every_construct_threads_acc_linearly() {
    let (src, expected) = corpus_class();
    let code = try_codegen(&src).unwrap_or_else(|error| {
        let at = match &error {
            CodeGenError::UnsupportedFeature {
                span: Some(span), ..
            } => {
                let (start, end) = (span.start() as usize, span.end() as usize);
                let line_start = src[..start].rfind("class sealed").unwrap_or(0);
                format!(
                    "{} ... [{}]",
                    &src[line_start..end].lines().next().unwrap_or(""),
                    &src[start..end]
                )
            }
            _ => String::new(),
        };
        panic!("the corpus must compile: {error}\n{at}")
    });
    for (selector, steps) in &expected {
        let entry = crate::core_erlang::selector_mangler::threaded_class_fn_name(selector);
        let arity = selector.matches(':').count() + 2;
        let header = format!("'{entry}'/{arity}");
        let text = function_text(&code, &header);
        assert_acc_linear(text, selector);
        // One `Acc` step per use: the `call_threaded/3` calls of the body plus
        // the forwards' entry calls (a collection HOM's fold counts as one).
        let binds = text.matches("let Acc@").count();
        assert!(
            binds >= *steps,
            "{selector}: expected at least {steps} Acc steps, saw {binds}\n{text}"
        );
        // The plain entry is the wrapper over it.
        let plain = function_text(&code, &format!("'class_{selector}'/{}", arity - 1));
        assert!(
            plain.contains(&format!("apply '{entry}'/{arity}")),
            "{plain}"
        );
    }
    assert_no_entry_needs_nlr(&src, &code);
    // The verifier ran over every body (a violation hard-fails a debug build),
    // and the whole module is valid Core Erlang.
    assert_compiles_through_erlc("test", &code);
}

// ─── The loud refusal of every use the predicate does not admit ────────────

/// Compiles `method` forced to have a threading parameter `b` at position 0
/// (bypassing the predicate) and returns the refusal.
fn refusal_for(body: &str) -> String {
    let src = format!(
        "Object subclass: Forced\n  class sealed run: b items: items flag: f =>\n    n := 0.\n    {body}\n"
    );
    let result = forced_positions::with(&[("run:items:flag:", &[0])], || {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| try_codegen(&src)))
    });
    match result {
        Ok(Ok(code)) => panic!(
            "`{body}` compiled without a refusal:\n{}",
            function_text(&code, "'__t2_class_run:items:flag:'/5")
        ),
        Ok(Err(error)) => format!("{error}"),
        // A verifier backstop reports through the shared helper, which is a
        // hard failure in a debug build.
        Err(payload) => payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default(),
    }
}

#[test]
fn a_use_inside_a_construct_is_refused_never_dropped() {
    for body in [
        "items do: [:y | b value: y]",
        "items collect: [:y | b value: y]",
        "items inject: 0 into: [:a :y | b value: a]",
        "1 to: 3 do: [:i | b value: i]",
        "[n < 3] whileTrue: [b value. n := n + 1]",
        "f ifTrue: [b value] ifFalse: [0]",
        "f ifTrue: [b value]",
        "[b value] on: Error do: [:e | 0]",
        "[nil] on: Error do: [:e | b value]",
        "[b value] ensure: [nil]",
        "[nil] ensure: [b value]",
        "g := [b value]. g value",
        "[b value] value",
        "k := [:y | b value: y]. items do: k",
        "total := 0. items do: [:y | total := total + (b value: y)]",
    ] {
        let message = refusal_for(body);
        assert!(
            message.contains("threading block parameter")
                || message.contains("ConstructFamilyDropped"),
            "`{body}` was refused with an unrelated message: {message}"
        );
    }
}

#[test]
fn a_bare_mention_of_a_threading_parameter_is_refused() {
    for body in ["x := b. x value", "#(1, b)", "^b"] {
        let message = refusal_for(body);
        assert!(
            message.contains("threading block parameter"),
            "`{body}`: {message}"
        );
    }
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4232: no new `Expression::unwrap_parens()` call lands where the
//! send-operand normalisation pass (`ast::normalize::peel_operand_parens`,
//! BT-4059) already guarantees there is no wrapper.
//!
//! Analysis and codegen see a tree whose send receivers, send arguments and
//! cascade-message arguments carry no `Parenthesized` wrapper (codegen checks
//! this in test builds: `normalize::assert_operands_normalised`). A peel on
//! such an operand is dead weight, and it hides the invariant: the next site
//! that forgets it works only by accident. Telling "this `unwrap_parens()`
//! peels a send operand" from "this one peels a statement" needs the type of
//! the expression at the call, which a source scan cannot see, so this guard
//! is the ratchet the issue allows instead: a per-file ceiling on
//! `unwrap_parens()` calls in non-test code that may only go down.
//!
//! - A file over its ceiling fails. Remove the new call if it peels a send
//!   receiver, send argument or cascade-message argument: match the operand
//!   directly.
//! - A file under its ceiling fails too, so that a removal lowers the
//!   ceiling and the slot cannot be reused silently.
//! - A file not in [`CEILINGS`] has a ceiling of 0.
//!
//! **Justifying a new call.** Raise a ceiling only for a peel in one of the
//! positions the pass leaves alone, and say which in the commit message:
//!
//! - a statement, an assignment value, a `^` value, a block body's
//!   expression, a match scrutinee or arm body, or a predicate called on
//!   several of those positions at once;
//! - a `(super)` receiver or argument, which the pass keeps whole because
//!   `(super) foo` is not a super send;
//! - a cascade's folded first send or its shared receiver
//!   (`ast_walker::cascade_shared_receiver`, a `SendRef` receiver from
//!   `walk_sends`/`cascade_sends`, or the dispatch receiver the type checker's
//!   `infer_cascade` hands `infer_message_send_with_receiver_ty`): `(a foo);
//!   bar` cascades onto the result of `a foo`, so that wrapper stays.
//!
//! "The pass already ran" is no longer a matter of which caller you are
//! (BT-4238): every `semantic_analysis` entry that a parsed module reaches
//! reads the operand-normalised tree, through `ast::normalize::normalised` or
//! the pass itself:
//!
//! - `analyse_full` (its own clone) and codegen's entries (BT-4059);
//! - `TypeChecker::check_module` / `check_module_with_protocols_and_aliases`,
//!   so `infer_types`, `infer_types_and_returns(_with_aliases)` and
//!   `infer_method_return_types` (the language service's hover, completion,
//!   signature help and go-to-definition, MCP and CLI type coverage);
//! - `local_threading_diagnostics`;
//! - `beamtalk-lint`'s `run_lint_passes`, which hands every pass but
//!   `unnecessary_parens` the normalised module (so `check_effect_free_statements`,
//!   `block_facts` and the span joins of `dead_block_assignment` /
//!   `dead_returned_assignment` read one tree);
//! - `class_var_census::parse_corpus_source`, `ffi_sites_query` and
//!   `method_source_walker` (peeled at parse).
//!
//! **Raw-tree readers that remain**, none of which reaches a peel this guard
//! counts: the `unnecessary_parens` lint and the formatter/`unparse`, whose
//! subject is the parentheses as written; the language service's position
//! lookups, which walk the parsed tree and look a node's type up through
//! `queries::type_of_parsed` / `normalize::canonical_operand`; and
//! `build_stdlib`'s metadata pass, which calls
//! `semantic_analysis::analyze_initialize_assigns` / `has_dynamic_field_writer`
//! on the parsed stdlib class (they walk `Parenthesized` themselves and use
//! no operand peel).
//!
//! The scanned trees are codegen, the REPL's codegen and `beamtalk-core`
//! (where `semantic_analysis` lives). `ast::normalize` itself, and the crates
//! that read the parsed tree for positions (`beamtalk-lint`,
//! `beamtalk-language-service`), are not scanned.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// The scanned source roots, relative to the workspace's `crates/`.
const ROOTS: &[&str] = &[
    "beamtalk-codegen/src",
    "beamtalk-repl/src",
    "beamtalk-core/src",
];

/// Files never scanned: the pass itself.
const EXEMPT: &[&str] = &["beamtalk-core/src/ast/normalize.rs"];

/// The ceiling of `unwrap_parens()` calls in each file's non-test code, with
/// the position category its remaining calls fall in (module docs).
const CEILINGS: &[(&str, usize)] = &[
    // Statement, assignment-value, return-value, match-arm and construct
    // peels; `(super)` checks; cascade shared receivers
    // (`tier2_merge_call_name`, `sequence_children` of a cascade receiver).
    ("beamtalk-codegen/src/core_erlang/actor_state_access.rs", 1),
    (
        "beamtalk-codegen/src/core_erlang/control_flow/analysis.rs",
        6,
    ),
    (
        "beamtalk-codegen/src/core_erlang/control_flow/conditionals.rs",
        4,
    ),
    (
        "beamtalk-codegen/src/core_erlang/control_flow/exception_handling.rs",
        1,
    ),
    ("beamtalk-codegen/src/core_erlang/expr_shape.rs", 2),
    ("beamtalk-codegen/src/core_erlang/field_types.rs", 1),
    ("beamtalk-codegen/src/core_erlang/gen_server/methods.rs", 9),
    ("beamtalk-codegen/src/core_erlang/generator/accessors.rs", 2),
    ("beamtalk-codegen/src/core_erlang/intrinsics.rs", 1),
    ("beamtalk-codegen/src/core_erlang/mod.rs", 1),
    (
        "beamtalk-codegen/src/core_erlang/patterns/match_lowering.rs",
        5,
    ),
    ("beamtalk-codegen/src/core_erlang/sequencing.rs", 3),
    ("beamtalk-codegen/src/core_erlang/synced_call.rs", 8),
    ("beamtalk-codegen/src/core_erlang/threaded_expr.rs", 14),
    ("beamtalk-codegen/src/core_erlang/threading_analysis.rs", 3),
    ("beamtalk-codegen/src/core_erlang/util.rs", 9),
    ("beamtalk-codegen/src/core_erlang/value_type_codegen.rs", 11),
    // Expression helpers in the AST leaf (`as_literal_bool`).
    ("beamtalk-core/src/ast/expression.rs", 1),
    // A cascade shared receiver: `erlang_module_of_receiver` is handed
    // `(Erlang lists)` as the shared receiver of `(Erlang lists); f: x`.
    ("beamtalk-core/src/ffi_receiver.rs", 1),
    // Predicates called on a statement, an assignment or return value and an
    // operand alike (`local_threading_construct`, the site rules, `match_arms`,
    // `repl_*_sites`, `LoopBodyContext`), assignment and literal-element
    // escape operands, a field-write target's receiver, and `(super)`
    // (`is_self_or_super`, `receiver_statically_not_dictionary`).
    ("beamtalk-core/src/semantic_analysis/block_facts.rs", 35),
    // A block literal stashed for a declared `Block(...)` return or local
    // (a return / assignment value).
    (
        "beamtalk-core/src/semantic_analysis/type_checker/inference/blocks.rs",
        1,
    ),
    // A method body's last statement: `ends_with_field_write` (BT-4235).
    (
        "beamtalk-core/src/semantic_analysis/type_checker/inference/mod.rs",
        1,
    ),
    // A `match:` scrutinee.
    (
        "beamtalk-core/src/semantic_analysis/type_checker/inference/flow.rs",
        1,
    ),
    // Cascade shared receivers (`infer_cascade`'s dispatch receiver) and
    // `(super)`: `is_class_side_receiver`.
    (
        "beamtalk-core/src/semantic_analysis/type_checker/inference/send/mod.rs",
        1,
    ),
    // A cascade shared receiver reaching the class-side check.
    (
        "beamtalk-core/src/semantic_analysis/type_checker/inference/send/receiver.rs",
        1,
    ),
    // `classify`, read on assignment values, receivers and arguments alike;
    // a block body's statement.
    (
        "beamtalk-core/src/semantic_analysis/type_checker/known_remote.rs",
        2,
    ),
    // Narrowing reads a send's receiver, which may be a cascade shared
    // receiver; the compound-guard walk also reads a block's statement.
    (
        "beamtalk-core/src/semantic_analysis/type_checker/narrowing/mod.rs",
        1,
    ),
    (
        "beamtalk-core/src/semantic_analysis/type_checker/narrowing/refine.rs",
        3,
    ),
    (
        "beamtalk-core/src/semantic_analysis/type_checker/narrowing/rules/singleton_eq.rs",
        1,
    ),
    (
        "beamtalk-core/src/semantic_analysis/type_checker/validation.rs",
        1,
    ),
    // `SendRef` receivers (cascade shared receivers) and an assignment value.
    (
        "beamtalk-core/src/semantic_analysis/validators/class_state_abroad.rs",
        4,
    ),
    // Shared predicates over statements, assignment values and receivers, and
    // the cascade's folded first send.
    (
        "beamtalk-core/src/semantic_analysis/validators/lint_validators.rs",
        9,
    ),
    // Constructs judged in any position (`expr` itself), the `Parenthesized`
    // arm, statements, cascade receivers (`nlr_cause`'s `check_send`, a
    // cascade's shared receiver) and `(super)`.
    (
        "beamtalk-core/src/semantic_analysis/validators/local_threading.rs",
        17,
    ),
    // `send_user_code`'s `is_block`, read on a cascade's shared receiver
    // (`record_send_target` for each later cascade message) and arguments
    // alike.
    ("beamtalk-core/src/state_threading_selectors.rs", 1),
];

fn crates_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crate dir has a parent")
        .to_path_buf()
}

fn rust_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = std::fs::read_dir(dir).unwrap_or_else(|e| panic!("read {}: {e}", dir.display()));
    for entry in entries {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            // Test-only trees.
            if path.file_name().is_some_and(|n| n == "tests") {
                continue;
            }
            rust_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if name == "tests.rs" || name.ends_with("_tests.rs") {
                continue;
            }
            out.push(path);
        }
    }
}

/// The number of `.unwrap_parens()` calls in `src`'s non-test code: line
/// comments and `#[cfg(test)]` modules (inline ones, by brace depth) are
/// left out.
fn count_calls(src: &str) -> usize {
    let mut count = 0;
    let mut lines = src.lines().peekable();
    while let Some(line) = lines.next() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("#[cfg(test)]")
            && lines
                .peek()
                .is_some_and(|next| is_mod_line(next.trim_start()))
        {
            let header = lines.next().unwrap_or_default();
            if header.trim_end().ends_with(';') {
                continue;
            }
            let mut depth = brace_delta(header);
            while depth > 0 {
                let Some(inner) = lines.next() else { break };
                depth += brace_delta(inner);
            }
            continue;
        }
        let code = trimmed.split("//").next().unwrap_or("");
        count += code.matches(".unwrap_parens()").count();
    }
    count
}

fn is_mod_line(line: &str) -> bool {
    line.starts_with("mod ") || line.starts_with("pub mod ") || line.starts_with("pub(crate) mod ")
}

fn brace_delta(line: &str) -> i64 {
    let code = line.split("//").next().unwrap_or("");
    let opens = code.matches('{').count();
    let closes = code.matches('}').count();
    i64::try_from(opens).unwrap_or(0) - i64::try_from(closes).unwrap_or(0)
}

/// Every scanned file with at least one call, keyed by its path relative to
/// `crates/`.
fn current_counts() -> BTreeMap<String, usize> {
    let crates = crates_dir();
    let mut files = Vec::new();
    for root in ROOTS {
        rust_files(&crates.join(root), &mut files);
    }
    let mut counts = BTreeMap::new();
    for file in files {
        let rel = file
            .strip_prefix(&crates)
            .expect("under crates/")
            .to_string_lossy()
            .replace('\\', "/");
        if EXEMPT.contains(&rel.as_str()) {
            continue;
        }
        let src = std::fs::read_to_string(&file)
            .unwrap_or_else(|e| panic!("read {}: {e}", file.display()));
        let n = count_calls(&src);
        if n > 0 {
            counts.insert(rel, n);
        }
    }
    counts
}

#[test]
fn unwrap_parens_calls_stay_within_their_per_file_ceiling() {
    let ceilings: BTreeMap<&str, usize> = CEILINGS.iter().copied().collect();
    assert_eq!(ceilings.len(), CEILINGS.len(), "duplicate CEILINGS entry");
    let counts = current_counts();
    let mut problems = Vec::new();
    for (file, &n) in &counts {
        let ceiling = ceilings.get(file.as_str()).copied().unwrap_or(0);
        if n > ceiling {
            problems.push(format!(
                "{file}: {n} `unwrap_parens()` calls, ceiling {ceiling}. A send \
                 receiver, send argument or cascade-message argument is already \
                 peeled by `ast::normalize` (BT-4059): match it directly. Raise \
                 the ceiling only for a position the pass keeps (see this \
                 module's docs)."
            ));
        } else if n < ceiling {
            problems.push(format!(
                "{file}: {n} `unwrap_parens()` calls, ceiling {ceiling}: lower \
                 the ceiling to {n} (it only goes down)."
            ));
        }
    }
    for (file, &ceiling) in &ceilings {
        if !counts.contains_key(*file) {
            problems.push(format!(
                "{file}: no `unwrap_parens()` calls left (or the file moved), \
                 ceiling {ceiling}: remove its CEILINGS entry."
            ));
        }
    }
    assert!(
        problems.is_empty(),
        "unwrap_parens() ceiling check (BT-4232):\n{}",
        problems.join("\n")
    );
}

#[test]
fn the_counter_skips_comments_and_test_modules() {
    let src = "fn a(e: &Expression) { e.unwrap_parens(); } // x.unwrap_parens()\n\
               // e.unwrap_parens()\n\
               fn b() { x.unwrap_parens().unwrap_parens(); }\n\
               #[cfg(test)]\n\
               mod tests {\n\
                   fn c() { if x { e.unwrap_parens(); } }\n\
               }\n\
               fn d() { e.unwrap_parens(); }\n\
               #[cfg(test)]\n\
               mod more;\n\
               fn f() { e.unwrap_parens(); }\n";
    assert_eq!(count_calls(src), 5);
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Stack-safety tests for every recursive parse path (BT-5181).
//!
//! Each test lexes and parses hostile input on a thread with a deliberately
//! small stack. A path that recurses without the nesting guard overflows that
//! stack, which aborts the whole test process, so a missing guard cannot pass
//! silently. The deeply nested inputs must report the "nesting is too deep"
//! diagnostic; the long flat chains must report the "too long" diagnostic and
//! leave an AST shallow enough to walk and drop.

use super::*;
use crate::ast::{Expression, Module};
use crate::ast_walker::{for_each_child, for_each_root_expr};

/// Stack for the parse thread: far below the 2 MiB a default test thread has,
/// so an unguarded path overflows quickly instead of happening to fit.
const SMALL_STACK: usize = 256 * 1024;

/// Nesting levels for the deep inputs: well past `MAX_NESTING_DEPTH`.
const DEEP: usize = 5_000;

/// Terms in the long flat chains (the issue's 100k-term chain).
const LONG: usize = 100_000;

/// Lexes and parses `source` on a small-stack thread; answers the
/// diagnostics and the greatest expression depth in the module.
fn parse_on_small_stack(source: String) -> (Vec<Diagnostic>, usize) {
    std::thread::Builder::new()
        .stack_size(SMALL_STACK)
        .spawn(move || {
            let tokens = lex_with_eof(&source);
            let (module, diagnostics) = parse(tokens);
            let depth = module_depth(&module);
            (diagnostics, depth)
        })
        .expect("spawn parse thread")
        .join()
        .expect("parse thread panicked")
}

/// The greatest expression depth in `module`, measured without recursion.
fn module_depth(module: &Module) -> usize {
    let mut roots: Vec<&Expression> = Vec::new();
    for_each_root_expr(module, |e| roots.push(e));
    let mut max = 0;
    let mut stack: Vec<(&Expression, usize)> = roots.into_iter().map(|e| (e, 1)).collect();
    while let Some((expr, depth)) = stack.pop() {
        max = max.max(depth);
        for_each_child(expr, &mut |child| stack.push((child, depth + 1)));
    }
    max
}

fn assert_too_deep(source: String) {
    let (diagnostics, _) = parse_on_small_stack(source);
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("nesting is too deep")),
        "expected the nesting-too-deep diagnostic, got: {:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

/// Asserts `diagnostics` reports the chain-too-long diagnostic exactly once:
/// the links past the limit are consumed without being reported again.
fn assert_reported_too_long_once(diagnostics: &[Diagnostic]) {
    let count = diagnostics
        .iter()
        .filter(|d| d.message.contains("too long"))
        .count();
    assert_eq!(
        count,
        1,
        "expected one chain-too-long diagnostic, got {count}: {:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

fn assert_too_long(source: String) {
    let (diagnostics, depth) = parse_on_small_stack(source);
    assert_reported_too_long_once(&diagnostics);
    assert!(
        depth <= MAX_EXPRESSION_DEPTH,
        "a capped chain must leave a shallow AST, got depth {depth}"
    );
}

/// `open` repeated `DEEP` times, then `middle`, then `close` repeated.
fn nested(open: &str, middle: &str, close: &str) -> String {
    open.repeat(DEEP) + middle + &close.repeat(DEEP)
}

// ── Collection literals ─────────────────────────────────────────────────────

#[test]
fn nested_list_literals_are_bounded() {
    assert_too_deep(nested("#(", "1", ")"));
}

#[test]
fn nested_array_literals_are_bounded() {
    assert_too_deep(nested("#[", "1", "]"));
}

#[test]
fn nested_tuple_literals_are_bounded() {
    assert_too_deep(format!("x := {}", nested("{", "1", "}")));
}

#[test]
fn nested_tuple_literal_statement_is_bounded() {
    assert_too_deep(nested("{", "1", "}"));
}

#[test]
fn nested_map_literal_values_are_bounded() {
    assert_too_deep(nested("#{#a => ", "1", "}"));
}

#[test]
fn nested_map_literal_keys_are_bounded() {
    assert_too_deep(nested("#{", "1", " => 1}"));
}

#[test]
fn nested_list_tails_are_bounded() {
    assert_too_deep(nested("#(1 | ", "#()", ")"));
}

#[test]
fn nested_literals_in_keyword_arguments_are_bounded() {
    assert_too_deep(nested("a at: #(", "1", ")"));
}

#[test]
fn nested_binary_constructions_are_bounded() {
    assert_too_deep(nested("<<(", "1", ")>>"));
}

// ── Operators ───────────────────────────────────────────────────────────────

#[test]
fn right_associative_power_chain_is_bounded() {
    assert_too_deep("2 ** ".repeat(DEEP) + "2");
}

// ── Type annotations ────────────────────────────────────────────────────────

#[test]
fn nested_generic_types_are_bounded() {
    assert_too_deep(format!("x :: {} := 1", nested("Foo(", "Integer", ")")));
}

#[test]
fn nested_parenthesised_types_are_bounded() {
    assert_too_deep(format!("x :: {} := 1", nested("(", "Integer", ")")));
}

#[test]
fn nested_shape_types_are_bounded() {
    assert_too_deep(format!("x :: {} := 1", nested("#{#a => ", "Integer", "}")));
}

#[test]
fn nested_parameter_type_in_method_header_is_bounded() {
    // The header lookahead (`skip_type_operand` and friends) recurses over
    // the parameter's type before the method is parsed.
    assert_too_deep(format!(
        "Object subclass: Foo\n  bar: x :: {} => x",
        nested("Foo(", "Integer", ")")
    ));
}

#[test]
fn nested_grouped_parameter_type_in_method_header_is_bounded() {
    assert_too_deep(format!(
        "Object subclass: Foo\n  bar: x :: {} => x",
        nested("(", "Integer", ")")
    ));
}

#[test]
fn nested_return_type_in_method_header_is_bounded() {
    assert_too_deep(format!(
        "Object subclass: Foo\n  bar -> {} => 1",
        nested("Foo(", "Integer", ")")
    ));
}

#[test]
fn nested_block_parameter_type_is_bounded() {
    assert_too_deep(format!("[:x :: {} | x]", nested("Foo(", "Integer", ")")));
}

#[test]
fn unclosed_shape_return_types_on_many_lines_do_not_overflow() {
    // `skip_shape_type` asks `is_method_definition_at` about each later
    // header line, which looks at that line's own `-> #{` return type: one
    // lookahead frame per line.
    let source = String::from("Object subclass: Foo\n") + &"  bar -> #{\n".repeat(DEEP);
    let (diagnostics, _) = parse_on_small_stack(source);
    assert!(!diagnostics.is_empty());
}

// ── Patterns ────────────────────────────────────────────────────────────────

#[test]
fn nested_tuple_patterns_are_bounded() {
    assert_too_deep(format!("x match: [{} -> 1]", nested("{", "a", "}")));
}

#[test]
fn nested_array_patterns_are_bounded() {
    assert_too_deep(format!("x match: [{} -> 1]", nested("#[", "a", "]")));
}

#[test]
fn nested_constructor_patterns_are_bounded() {
    assert_too_deep(format!(
        "x match: [{} -> 1]",
        nested("Result ok: {", "a", "}")
    ));
}

#[test]
fn nested_generic_type_patterns_do_not_overflow() {
    // A type pattern's generic arguments are rejected and skipped
    // iteratively rather than parsed, so this reports that instead.
    let (diagnostics, _) = parse_on_small_stack(format!(
        "x match: [v :: {} -> 1]",
        nested("Foo(", "Integer", ")")
    ));
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("generic type arguments")),
        "{:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

// ── Strings ─────────────────────────────────────────────────────────────────

#[test]
fn nested_string_interpolation_is_bounded() {
    // The lexer re-lexes each interpolation body with a sub-lexer.
    assert_too_deep(nested("\"a{", "1", "}a\""));
}

// ── Long flat chains ────────────────────────────────────────────────────────

#[test]
fn long_binary_chain_is_capped() {
    assert_too_long("a".to_string() + &" + a".repeat(LONG));
}

#[test]
fn long_unary_chain_is_capped() {
    assert_too_long("a".to_string() + &" foo".repeat(LONG));
}

#[test]
fn long_pipeline_is_capped() {
    assert_too_long("a".to_string() + &" |> foo".repeat(LONG));
}

#[test]
fn long_field_access_chain_is_capped() {
    assert_too_long("self".to_string() + &".a".repeat(LONG));
}

#[test]
fn long_type_intersection_chain_is_capped() {
    let (diagnostics, _) = parse_on_small_stack(format!("x :: A{} := 1", " & A".repeat(LONG)));
    assert_reported_too_long_once(&diagnostics);
}

#[test]
fn long_type_difference_chain_is_capped() {
    let (diagnostics, _) = parse_on_small_stack(format!("x :: A{} := 1", " \\ #a".repeat(LONG)));
    assert_reported_too_long_once(&diagnostics);
}

#[test]
fn nested_long_chains_are_bounded_by_total_depth() {
    // Each level is a chain just under the cap whose first operand is the
    // next level, so no single chain trips the length cap but the whole
    // expression is far deeper than any one chain.
    let per_level = MAX_EXPRESSION_DEPTH - 1;
    let mut source = String::new();
    for _ in 0..40 {
        source.push('(');
    }
    source.push('a');
    for _ in 0..40 {
        source.push_str(&" + a".repeat(per_level));
        source.push(')');
    }
    let (diagnostics, depth) = parse_on_small_stack(source);
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("too long") || d.message.contains("nesting is too deep")),
        "expected a depth diagnostic, got: {:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
    assert!(
        depth <= MAX_EXPRESSION_DEPTH,
        "the parser must not hand on an expression this deep, got depth {depth}"
    );
}

// ── Within the limits ───────────────────────────────────────────────────────

#[test]
fn moderately_nested_literals_still_parse() {
    let source = nested_n("#(", "1", ")", 30);
    let (diagnostics, _) = parse_on_small_stack(source);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn moderately_long_chains_still_parse() {
    let source = "a".to_string() + &" + a".repeat(500);
    let (diagnostics, _) = parse_on_small_stack(source);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

fn nested_n(open: &str, middle: &str, close: &str, n: usize) -> String {
    open.repeat(n) + middle + &close.repeat(n)
}

// ── Review follow-ups ───────────────────────────────────────────────────────

/// Parses `source` on a small stack; answers the diagnostics and the module.
fn parse_module_on_small_stack(source: String) -> (Vec<Diagnostic>, Module) {
    std::thread::Builder::new()
        .stack_size(SMALL_STACK)
        .spawn(move || {
            let (module, diagnostics) = parse(lex_with_eof(&source));
            (diagnostics, module)
        })
        .expect("spawn parse thread")
        .join()
        .expect("parse thread panicked")
}

/// `(` nested `levels` deep, each level a `chain`-link chain whose first
/// operand is the next level: deep through both mechanisms at once while
/// each chain and each level is within its own limit.
fn nested_chains(levels: usize, operator: &str, operand: &str) -> String {
    let link = format!(" {operator} {operand}");
    "(".repeat(levels) + operand + &(link.repeat(MAX_EXPRESSION_DEPTH - 1) + ")").repeat(levels)
}

#[test]
fn grouped_type_chains_are_bounded_by_total_depth() {
    // Grouping parens make each chain the first operand of the next, so the
    // intersection tree is far deeper than any one chain.
    let (diagnostics, module) =
        parse_module_on_small_stack(format!("x :: {} := 1", nested_chains(40, "&", "A")));
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("Type annotation is too long or nested too deep")),
        "{:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
    drop(module); // must not overflow either
}

#[test]
fn deep_operand_past_the_chain_limit_is_dropped_without_overflow() {
    // The right operand after the chain limit is discarded; it is itself as
    // deep as nested chains get.
    let source = "a".to_string()
        + &" + a".repeat(MAX_EXPRESSION_DEPTH)
        + " + "
        + &nested_chains(40, "+", "a");
    let (diagnostics, depth) = parse_on_small_stack(source);
    assert!(depth <= MAX_EXPRESSION_DEPTH, "depth {depth}");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("Expression chain is too long")),
        "{:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

#[test]
fn intermediate_nesting_levels_are_bounded() {
    // A deep tree inside a parenthesised operand is bounded where it is
    // built, not only at the statement, so the parser never holds it.
    let (diagnostics, depth) =
        parse_on_small_stack(format!("x foo: [{}]", nested_chains(40, "+", "a")));
    assert!(depth <= MAX_EXPRESSION_DEPTH, "depth {depth}");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("too long or nested too deep")),
        "{:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

/// Asserts the only diagnostic is the one nesting diagnostic: recovery
/// skipped exactly the over-deep construct.
fn assert_single_nesting_diagnostic(source: String) {
    let (diagnostics, _) = parse_on_small_stack(source);
    assert_eq!(
        diagnostics.len(),
        1,
        "expected only the nesting diagnostic: {:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
    assert!(diagnostics[0].message.contains("nesting is too deep"));
}

#[test]
fn too_deep_literal_recovery_skips_method_lookup() {
    // `>>` here is method lookup, not the end of a binary construction.
    assert_single_nesting_diagnostic(nested("#(", "Counter >> #inc", ")") + ".\ny := 2");
}

#[test]
fn too_deep_literal_recovery_skips_infix_shift() {
    // An infix `<<` (itself an error) does not open a construction, so
    // recovery stops at the literal's own `)` and the next statement parses.
    let (diagnostics, module) =
        parse_module_on_small_stack(nested("#(", "a << b", ")") + ".\ny := 2");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("nesting is too deep"))
    );
    assert_eq!(module.expressions.len(), 2, "{diagnostics:?}");
}

#[test]
fn too_deep_power_chain_across_lines_reports_once() {
    assert_single_nesting_diagnostic("2 **\n".repeat(DEEP) + "2");
}

#[test]
fn too_deep_type_recovery_skips_the_whole_union() {
    assert_single_nesting_diagnostic(format!("x :: {} := 1", nested("Foo(", "A | B & C", ")")));
}

#[test]
fn replaced_chain_leaves_no_pipeline_sends() {
    // The chain past the limit becomes one error node; the pipeline sends
    // recorded inside it are gone with it.
    let source = "(x |> f)".to_string() + &" + (x |> f)".repeat(MAX_EXPRESSION_DEPTH + 5);
    let (diagnostics, module) = parse_module_on_small_stack(source);
    assert_reported_too_long_once(&diagnostics);
    assert!(
        module.pipeline_sends.is_empty(),
        "{} stale pipeline sends",
        module.pipeline_sends.len()
    );
}

// ── Cost of the depth bound (BT-5237) ───────────────────────────────────────

/// Parses `source`; answers how many nodes the depth bounds visited and how
/// many tokens the source has.
fn depth_bound_visits(source: &str) -> (usize, usize) {
    let tokens = lex_with_eof(source);
    let token_count = tokens.len();
    let mut parser = Parser::new(tokens);
    let _module = parser.parse_module();
    let visits = parser.expression_depths.visits() + parser.type_depths.visits();
    (visits, token_count)
}

/// Asserts that the depth bounds over `source` visit each node a constant
/// number of times, not once per nesting level it sits inside: every node
/// adds a token, so a single visit per node stays within a small multiple
/// of the token count, however deep the nesting.
fn assert_depth_bound_is_linear(source: &str) {
    let (visits, tokens) = depth_bound_visits(source);
    assert!(
        visits <= 3 * tokens,
        "depth bounds visited {visits} nodes for {tokens} tokens: subtrees are walked again per level"
    );
}

#[test]
fn depth_bound_does_not_rewalk_nested_chains() {
    let level = " + a + a + a + a + a + a + a)";
    assert_depth_bound_is_linear(&format!("x := {}", nested_n("(", "a", level, 60)));
}

#[test]
fn depth_bound_does_not_rewalk_nested_literals() {
    let elements = "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ";
    assert_depth_bound_is_linear(&format!(
        "x := {}",
        nested_n(&format!("#({elements}"), "0", ")", 60)
    ));
}

#[test]
fn depth_bound_does_not_rewalk_nested_blocks() {
    assert_depth_bound_is_linear(&format!(
        "x := {}",
        nested_n("[ a foo: 1 bar: b. b baz. ", "a", " ]", 60)
    ));
}

#[test]
fn depth_bound_does_not_rewalk_nested_types() {
    assert_depth_bound_is_linear(&format!(
        "x :: {} := 1",
        nested_n("(", "A", " & B & C & D)", 60)
    ));
}

#[test]
fn depth_bound_does_not_rewalk_spliced_unions() {
    // A grouped union is spliced into the union around it, so each level
    // holds the members of every level inside it: the bound looks each
    // member up once per level (as the splice copies it), but must not walk
    // a member's subtree again. Each member here is eight levels deep.
    let member = nested_n("Foo(", "A", ")", 7);
    assert_depth_bound_is_linear(&format!(
        "x :: {} := 1",
        nested_n("(", "A", &format!(" | {member})"), 20)
    ));
}

#[test]
fn depth_bound_does_not_rewalk_the_repository_sources() {
    // Every `.bt` file in the repository. Debug builds also hold each
    // level's ledger depth equal to a full walk of its tree
    // (`Parser::bound_expression_depth`), so this checks both that the
    // ledger recognises the real code's subtrees and that it answers their
    // true depths. Symlinks, build output and hidden directories are skipped.
    fn bt_files(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
        for entry in std::fs::read_dir(dir).expect("read source directory") {
            let entry = entry.expect("directory entry");
            let file_type = entry.file_type().expect("file type");
            let path = entry.path();
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if file_type.is_dir() && !name.starts_with('.') && name != "_build" {
                bt_files(&path, out);
            } else if file_type.is_file() && path.extension().is_some_and(|ext| ext == "bt") {
                out.push(path);
            }
        }
    }
    let root = beamtalk_workspace::cargo_workspace_root!();
    let mut files = Vec::new();
    for dir in ["stdlib", "examples", "tests"] {
        bt_files(&root.join(dir), &mut files);
    }
    assert!(files.len() > 100, "found only {} .bt files", files.len());
    let (mut visits, mut tokens) = (0, 0);
    for file in &files {
        let source = std::fs::read_to_string(file).expect("read .bt file");
        let (file_visits, file_tokens) = depth_bound_visits(&source);
        assert!(
            file_visits <= 2 * file_tokens,
            "{}: depth bounds visited {file_visits} nodes for {file_tokens} tokens",
            file.display()
        );
        visits += file_visits;
        tokens += file_tokens;
    }
    assert!(
        visits <= tokens,
        "depth bounds visited {visits} nodes for {tokens} tokens over {} files",
        files.len()
    );
}

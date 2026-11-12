// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::*;

// ─── ADR 0137 (BT-4020): sends that may run user code ───────────────────

fn analyzed(src: &str) -> BlockMutationAnalysis {
    use crate::source_analysis::{lex_with_eof, parse};
    let (module, _) = parse(lex_with_eof(src));
    let Expression::Block(block) = &module.expressions[0].expression else {
        panic!("not a block: {src}");
    };
    analyze_block(block)
}

#[test]
fn a_send_to_another_object_may_run_user_code() {
    for src in [
        "[x foo]",
        "[x printString]",
        "[xs do: blk]",
        // A superclass method may run user code; in a block it runs on the cell.
        "[super foo]",
        // A literal-block collection message may be a closure handed to a runtime fold.
        "[xs do: [:e | e]]",
    ] {
        assert!(analyzed(src).has_user_code_sends, "{src}");
    }
}

#[test]
fn intrinsics_inlined_constructs_and_self_sends_do_not_count() {
    for src in [
        "[x isNil]",
        "[x == y]",
        "[x ifTrue: [1] ifFalse: [2]]",
        "[1 + 2]",
        // A guarded operator's send is a rare slow path, never a call that
        // makes a body thread (see `send_may_run_user_code`).
        "[a + b]",
        "[x < 1]",
        "[self foo]",
        "[x foo!]",
    ] {
        assert!(!analyzed(src).has_user_code_sends, "{src}");
    }
}

#[test]
fn a_nested_block_contributes_its_user_code_sends() {
    assert!(analyzed("[xs do: [:e | e foo]]").has_user_code_sends);
}

#[test]
fn a_call_is_a_state_effect_only_in_an_actor_root() {
    let analysis = analyzed("[x foo]");
    assert!(analysis.has_synced_user_code_sends());
    // `has_state_effects` stays context-free; only the actor root counts the bracket.
    assert!(!analysis.has_state_effects());
    assert!(analysis.needs_mutation_threading(Some(RootMethodKind::Actor), true));
    assert!(!analysis.needs_mutation_threading(Some(RootMethodKind::ValueType), true));
    assert!(!analysis.needs_mutation_threading(Some(RootMethodKind::Class), true));
    assert!(!analysis.needs_mutation_threading(None, true));
}

#[test]
fn may_write_actor_state_covers_writes_sends_and_super_but_not_reads() {
    // BT-4122: the Tier 2 merge predicate.
    assert!(analyzed("[self.n := 1]").may_write_actor_state());
    assert!(analyzed("[self bump]").may_write_actor_state());
    assert!(analyzed("[x foo]").may_write_actor_state());
    assert!(analyzed("[super bump]").may_write_actor_state());
    assert!(!analyzed("[self.n]").may_write_actor_state());
    assert!(!analyzed("[count := count + 1]").may_write_actor_state());
    assert!(!analyzed("[42]").may_write_actor_state());
}

#[test]
fn test_analyze_empty_block() {
    let block = Block::new(vec![], vec![], Span::new(0, 2));
    let analysis = analyze_block(&block);
    assert!(analysis.local_reads.is_empty());
    assert!(analysis.local_writes.is_empty());
    assert!(analysis.field_reads.is_empty());
    assert!(analysis.field_writes.is_empty());
}

#[test]
fn test_analyze_local_variable_read() {
    let block = Block::new(
        vec![BlockParameter::new("x", Span::new(0, 1))],
        vec![bare(make_expr_id("x"))],
        Span::new(0, 5),
    );
    let analysis = analyze_block(&block);
    assert!(analysis.local_reads.contains("x"));
    assert!(analysis.local_writes.is_empty());
}

#[test]
fn test_analyze_local_variable_write() {
    let block = Block::new(
        vec![],
        vec![bare(Expression::Assignment {
            target: Box::new(make_expr_id("count")),
            value: Box::new(Expression::Literal(
                crate::ast::Literal::Integer(0),
                Span::new(9, 10),
            )),
            type_annotation: None,
            span: Span::new(0, 10),
        })],
        Span::new(0, 12),
    );
    let analysis = analyze_block(&block);
    assert!(analysis.local_writes.contains("count"));
}

#[test]
fn test_analyze_local_variable_mutation() {
    // [:count | count := count + 1]
    // Variable is a parameter, so it's in scope for both read and write
    let block = Block::new(
        vec![BlockParameter::new("count", Span::new(1, 6))],
        vec![bare(Expression::Assignment {
            target: Box::new(make_expr_id("count")),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(make_expr_id("count")),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    Span::new(16, 17),
                )],
                is_cast: false,
                span: Span::new(9, 17),
            }),
            type_annotation: None,
            span: Span::new(0, 17),
        })],
        Span::new(0, 19),
    );
    let analysis = analyze_block(&block);
    assert!(analysis.local_reads.contains("count"));
    assert!(analysis.local_writes.contains("count"));
    assert_eq!(analysis.threaded_vars().len(), 1);
    assert!(analysis.threaded_vars().contains("count"));
}

#[test]
fn test_analyze_field_read() {
    // [self.value]
    let block = Block::new(
        vec![],
        vec![bare(Expression::FieldAccess {
            receiver: Box::new(make_expr_id("self")),
            field: make_id("value"),
            span: Span::new(0, 10),
        })],
        Span::new(0, 12),
    );
    let analysis = analyze_block(&block);
    assert!(analysis.field_reads.contains("value"));
    assert!(analysis.field_writes.is_empty());
}

#[test]
fn test_analyze_field_write() {
    // [self.value := 0]
    let block = Block::new(
        vec![],
        vec![bare(Expression::Assignment {
            target: Box::new(Expression::FieldAccess {
                receiver: Box::new(make_expr_id("self")),
                field: make_id("value"),
                span: Span::new(0, 10),
            }),
            value: Box::new(Expression::Literal(
                crate::ast::Literal::Integer(0),
                Span::new(14, 15),
            )),
            type_annotation: None,
            span: Span::new(0, 15),
        })],
        Span::new(0, 17),
    );
    let analysis = analyze_block(&block);
    assert!(analysis.field_writes.contains("value"));
    assert!(!analysis.field_reads.contains("value"));
}

/// Parses `src` (a single block-literal expression) into its `Block`.
fn parse_block(src: &str) -> Block {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = crate::source_analysis::parse(tokens);
    assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
    match module.expressions.into_iter().next().map(|s| s.expression) {
        Some(Expression::Block(block)) => block,
        other => panic!("{src}: expected a block literal, got {other:?}"),
    }
}

fn cv_accesses(src: &str) -> ClassVarAccesses {
    let vars: HashSet<String> = ["n".to_string(), "m".to_string()].into();
    class_var_accesses(&parse_block(src), &vars)
}

fn names(items: &[&str]) -> std::collections::BTreeSet<String> {
    items.iter().map(ToString::to_string).collect()
}

fn written(acc: &ClassVarAccesses) -> std::collections::BTreeSet<String> {
    acc.writes.keys().cloned().collect()
}

#[test]
fn class_var_accesses_write_only_block_reads_nothing() {
    let acc = cv_accesses("[:x | self.n := x]");
    assert_eq!(written(&acc), names(&["n"]));
    assert!(!acc.reads_class_state(), "{acc:?}");
    // `self.n := self.n + 1` both writes and reads.
    let acc = cv_accesses("[self.n := self.n + 1]");
    assert_eq!(written(&acc), names(&["n"]));
    assert_eq!(acc.reads, names(&["n"]));
}

#[test]
fn class_var_accesses_records_the_first_write_site() {
    let src = "[self.n := 1. self.n := 2]";
    let acc = cv_accesses(src);
    let first = u32::try_from(src.find("self.n").expect("fixture")).expect("small");
    assert_eq!(acc.writes["n"].start(), first, "{acc:?}");
}

#[test]
fn class_var_accesses_sees_reads_in_nested_blocks_and_cascade_messages() {
    let acc = cv_accesses("[:x | x foo; bar: self.n]");
    assert_eq!(acc.reads, names(&["n"]));
    let acc = cv_accesses("[[:y | y + self.m]]");
    assert_eq!(acc.reads, names(&["m"]));
    // An instance field of the same spelling is not a class variable.
    let acc = cv_accesses("[self.other + other.n]");
    assert!(!acc.reads_class_state(), "{acc:?}");
}

#[test]
fn class_var_accesses_has_field_probe_is_a_read_unless_cascaded() {
    let acc = cv_accesses("[self hasField: #n]");
    assert_eq!(
        acc.has_field_probes,
        [HasFieldProbe::Symbol("n".to_string())].into()
    );
    assert!(acc.reads_class_state());
    assert!(
        acc.reads.is_empty(),
        "a probe is not a variable read: {acc:?}"
    );
    // An undeclared name is still a probe of the class-variable home.
    let acc = cv_accesses("[self hasField: #nope]");
    assert_eq!(
        acc.has_field_probes,
        [HasFieldProbe::Symbol("nope".to_string())].into()
    );
    let acc = cv_accesses("[:k | self hasField: k]");
    assert_eq!(acc.has_field_probes, [HasFieldProbe::Computed].into());
    let acc = cv_accesses("[self hasField: #n; hasField: #m]");
    assert!(!acc.reads_class_state(), "{acc:?}");
    // Not to `self`: not a probe of the class-variable home.
    let acc = cv_accesses("[:o | o hasField: #n]");
    assert!(!acc.reads_class_state(), "{acc:?}");
}

#[test]
fn class_var_accesses_field_at_is_a_read_of_a_named_variable() {
    let acc = cv_accesses("[self fieldAt: #n]");
    assert_eq!(acc.field_at_reads, names(&["n"]));
    assert!(acc.reads_class_state());
    assert!(acc.reads.is_empty(), "not a `self.n` read: {acc:?}");
    assert_eq!(
        acc.field_at_read_noun("class variable").as_deref(),
        Some("class variable n")
    );
    // A literal name that is no variable reads no captured state (it
    // raises anyway).
    let acc = cv_accesses("[self fieldAt: #nope]");
    assert!(!acc.reads_class_state(), "{acc:?}");
    // A computed name may name one.
    let acc = cv_accesses("[:k | self fieldAt: k]");
    assert!(acc.computed_field_at_read && acc.reads_class_state());
    assert_eq!(acc.field_at_read_noun("field").as_deref(), Some("a field"));
    // A write is not a read; a cascade message or another receiver is
    // an ordinary send.
    let acc = cv_accesses("[self fieldAt: #n put: 1]");
    assert!(!acc.reads_class_state(), "{acc:?}");
    let acc = cv_accesses("[self fieldAt: #n; fieldAt: #m]");
    assert!(!acc.reads_class_state(), "{acc:?}");
    let acc = cv_accesses("[:o | o fieldAt: #n]");
    assert!(!acc.reads_class_state(), "{acc:?}");
}

#[test]
fn class_var_accesses_reads_phrase_names_each_variable_once() {
    let acc = cv_accesses("[self.n printString. self fieldAt: #n. self fieldAt: #m]");
    assert_eq!(
        acc.reads_phrase("class variable", "C").as_deref(),
        Some("reads class variable n of C and class variable m with `fieldAt:`")
    );
    let acc = cv_accesses("[self.n printString. self fieldAt: #n]");
    assert_eq!(
        acc.reads_phrase("field", "C").as_deref(),
        Some("reads field n of C")
    );
    let acc = cv_accesses("[:k | self.n printString. self fieldAt: #n. self fieldAt: k]");
    assert_eq!(
        acc.reads_phrase("field", "C").as_deref(),
        Some("reads field n of C and a field with `fieldAt:`")
    );
    assert_eq!(
        cv_accesses("[self hasField: #n]").reads_phrase("f", "C"),
        None
    );
}

#[test]
fn class_var_accesses_records_reflective_writes_in_walk_order() {
    let src = "[:k | self clearField: #nope. [self fieldAt: k put: 1]. self fieldAt: #n put: 2]";
    let acc = cv_accesses(src);
    let found: Vec<(ReflectiveWriteKind, Option<&str>)> = acc
        .reflective_writes
        .iter()
        .map(|w| (w.kind, w.name.as_deref()))
        .collect();
    assert_eq!(
        found,
        [
            (ReflectiveWriteKind::ClearField, Some("nope")),
            (ReflectiveWriteKind::FieldAtPut, None),
            (ReflectiveWriteKind::FieldAtPut, Some("n")),
        ]
    );
    let first = u32::try_from(src.find("self clearField").expect("fixture")).expect("small");
    assert_eq!(acc.reflective_writes[0].start, first);
    assert_eq!(
        acc.reflective_writes[0].describe("class variable", "C"),
        "clears class variable 'nope' of C with `clearField:`"
    );
    assert_eq!(
        acc.reflective_writes[1].describe("field", "C"),
        "writes a field of C with `fieldAt:put:`"
    );
    // A write is not a read.
    assert!(!acc.reads_class_state(), "{acc:?}");
    // A cascade message, a cast or another receiver is an ordinary send.
    for src in [
        "[self fieldAt: #n put: 1; clearField: #m]",
        "[self clearField: #n!]",
        "[:o | o fieldAt: #n put: 1. o clearField: #n]",
    ] {
        let acc = cv_accesses(src);
        assert!(acc.reflective_writes.is_empty(), "{src}: {acc:?}");
    }
}

#[test]
fn analyze_block_records_later_cascade_self_sends() {
    let analysis = analyze_block(&parse_block("[self log; bump: 1]"));
    assert_eq!(
        analysis.self_send_selectors,
        ["log".to_string(), "bump:".to_string()].into()
    );
}

#[test]
fn test_nested_block_propagates_field_writes() {
    // [:i | [:j | self.value := self.value + 1]]
    // Field writes in nested blocks must propagate to outer analysis
    let inner_block = Expression::Block(Block::new(
        vec![BlockParameter::new("j", Span::new(1, 2))],
        vec![bare(Expression::Assignment {
            target: Box::new(Expression::FieldAccess {
                receiver: Box::new(make_expr_id("self")),
                field: make_id("value"),
                span: Span::new(0, 10),
            }),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::FieldAccess {
                    receiver: Box::new(make_expr_id("self")),
                    field: make_id("value"),
                    span: Span::new(0, 10),
                }),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    Span::new(20, 21),
                )],
                is_cast: false,
                span: Span::new(0, 21),
            }),
            type_annotation: None,
            span: Span::new(0, 21),
        })],
        Span::new(0, 25),
    ));

    let outer_block = Block::new(
        vec![BlockParameter::new("i", Span::new(1, 2))],
        vec![bare(inner_block)],
        Span::new(0, 30),
    );

    let analysis = analyze_block(&outer_block);
    // Field writes from nested blocks MUST propagate
    assert!(
        analysis.field_writes.contains("value"),
        "field_writes should propagate from nested blocks"
    );
    // Local writes should NOT propagate
    assert!(
        analysis.local_writes.is_empty(),
        "local_writes should not propagate from nested blocks"
    );
}

#[test]
fn test_ensure_receiver_propagates_local_writes() {
    // [t := t + 1] ensure: [nil] — ensure:'s receiver (the
    // protected block) runs inline in the enclosing activation, so its
    // write to `t` must be visible at this block's own top-level
    // analysis (previously only ifTrue:/ifFalse:/ifTrue:ifFalse:
    // propagated local_writes out of a nested block).
    let protected_block = Expression::Block(Block::new(
        vec![],
        vec![bare(Expression::Assignment {
            target: Box::new(make_expr_id("t")),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(make_expr_id("t")),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    Span::new(9, 10),
                )],
                is_cast: false,
                span: Span::new(0, 10),
            }),
            type_annotation: None,
            span: Span::new(0, 10),
        })],
        Span::new(0, 12),
    ));
    let handler_block = Expression::Block(Block::new(vec![], vec![], Span::new(20, 26)));

    let outer_block = Block::new(
        vec![],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(protected_block),
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "ensure:",
                Span::new(13, 20),
            )]),
            arguments: vec![handler_block],
            is_cast: false,
            span: Span::new(0, 26),
        })],
        Span::new(0, 28),
    );

    let analysis = analyze_block(&outer_block);
    assert!(
        analysis.local_writes.contains("t"),
        "ensure:'s protected-block write to t must propagate to the enclosing block"
    );
}

#[test]
fn test_on_do_receiver_propagates_local_writes() {
    // [t := t + 1] on: Error do: [:e | nil] — on:do:'s receiver
    // (the try body) runs inline, same as ensure:'s.
    let try_block = Expression::Block(Block::new(
        vec![],
        vec![bare(Expression::Assignment {
            target: Box::new(make_expr_id("t")),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(make_expr_id("t")),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    Span::new(9, 10),
                )],
                is_cast: false,
                span: Span::new(0, 10),
            }),
            type_annotation: None,
            span: Span::new(0, 10),
        })],
        Span::new(0, 12),
    ));
    // Handler block binds its own exception param `e` — it must stay a
    // local binding of the nested block, not leak into the outer
    // analysis's captured_reads.
    let handler_block = Expression::Block(Block::new(
        vec![BlockParameter::new("e", Span::new(30, 31))],
        vec![],
        Span::new(29, 34),
    ));

    let outer_block = Block::new(
        vec![],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(try_block),
            selector: MessageSelector::Keyword(vec![
                crate::ast::KeywordPart::new("on:", Span::new(13, 16)),
                crate::ast::KeywordPart::new("do:", Span::new(25, 28)),
            ]),
            arguments: vec![make_expr_id("Error"), handler_block],
            is_cast: false,
            span: Span::new(0, 34),
        })],
        Span::new(0, 36),
    );

    let analysis = analyze_block(&outer_block);
    assert!(
        analysis.local_writes.contains("t"),
        "on:do:'s try-body write to t must propagate to the enclosing block"
    );
    assert!(
        !analysis.captured_reads.contains("e"),
        "on:do:'s handler block param must not leak as a captured read"
    );
}

#[test]
fn test_on_do_handler_param_write_does_not_leak_as_outer_local_write() {
    // [nil] on: Error do: [:e | e := 1] — the
    // handler writes its OWN exception param `e`. That write is confined
    // to the handler's own shadowed binding, not a genuine outer-scope
    // mutation, so it must NOT propagate into the enclosing block's
    // local_writes (which would otherwise misclassify the enclosing
    // block as needing mutation threading for a nonexistent outer `e`,
    // or spuriously fold into a same-named real outer local via
    // shadowing).
    let try_block = Expression::Block(Block::new(vec![], vec![], Span::new(0, 6)));
    let handler_block = Expression::Block(Block::new(
        vec![BlockParameter::new("e", Span::new(15, 16))],
        vec![bare(Expression::Assignment {
            target: Box::new(make_expr_id("e")),
            value: Box::new(Expression::Literal(
                crate::ast::Literal::Integer(1),
                Span::new(23, 24),
            )),
            type_annotation: None,
            span: Span::new(18, 24),
        })],
        Span::new(14, 26),
    ));

    let outer_block = Block::new(
        vec![],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(try_block),
            selector: MessageSelector::Keyword(vec![
                crate::ast::KeywordPart::new("on:", Span::new(7, 10)),
                crate::ast::KeywordPart::new("do:", Span::new(19, 22)),
            ]),
            arguments: vec![make_expr_id("Error"), handler_block],
            is_cast: false,
            span: Span::new(0, 26),
        })],
        Span::new(0, 28),
    );

    let analysis = analyze_block(&outer_block);
    assert!(
        !analysis.local_writes.contains("e"),
        "on:do:'s handler writing its own param must not leak as an outer local_write"
    );
}

#[test]
fn test_if_not_nil_propagates_local_writes() {
    // x ifNotNil: [:v | t := v] — ifNotNil: must be
    // included in is_inline_conditional_selector even for this
    // single-level (non-nested) case.
    let handler_block = Expression::Block(Block::new(
        vec![BlockParameter::new("v", Span::new(15, 16))],
        vec![bare(Expression::Assignment {
            target: Box::new(make_expr_id("t")),
            value: Box::new(make_expr_id("v")),
            type_annotation: None,
            span: Span::new(18, 25),
        })],
        Span::new(14, 26),
    ));

    let outer_block = Block::new(
        vec![],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(make_expr_id("x")),
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                "ifNotNil:",
                Span::new(2, 11),
            )]),
            arguments: vec![handler_block],
            is_cast: false,
            span: Span::new(0, 26),
        })],
        Span::new(0, 28),
    );

    let analysis = analyze_block(&outer_block);
    assert!(
        analysis.local_writes.contains("t"),
        "ifNotNil:'s handler-block write to t must propagate to the enclosing block"
    );
}

#[test]
fn test_is_literal_block() {
    let block_expr = Expression::Block(Block::new(vec![], vec![], Span::new(0, 2)));
    assert!(is_literal_block(&block_expr));

    let var_expr = make_expr_id("myBlock");
    assert!(!is_literal_block(&var_expr));
}

#[test]
fn test_is_control_flow_construct_while_true() {
    let condition = Expression::Block(Block::new(vec![], vec![], Span::new(0, 10)));
    let body = Expression::Block(Block::new(vec![], vec![], Span::new(20, 30)));
    let selector = MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
        "whileTrue:",
        Span::new(11, 21),
    )]);

    assert!(is_control_flow_construct(&condition, &selector, &[body]));
}

#[test]
fn test_is_not_control_flow_with_stored_block() {
    let condition = make_expr_id("conditionBlock");
    let body = Expression::Block(Block::new(vec![], vec![], Span::new(20, 30)));
    let selector = MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
        "whileTrue:",
        Span::new(11, 21),
    )]);

    // Not a control flow construct because receiver is not a literal block
    assert!(!is_control_flow_construct(&condition, &selector, &[body]));
}

#[test]
fn test_captured_reads_for_outer_variable_mutation() {
    // [count := count + 1] — `count` is read before being locally defined
    let block = Block::new(
        vec![],
        vec![bare(Expression::Assignment {
            target: Box::new(make_expr_id("count")),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(make_expr_id("count")),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    Span::new(16, 17),
                )],
                is_cast: false,
                span: Span::new(9, 17),
            }),
            type_annotation: None,
            span: Span::new(0, 17),
        })],
        Span::new(0, 19),
    );
    let analysis = analyze_block(&block);
    assert!(
        analysis.captured_reads.contains("count"),
        "count should be a captured read (read before definition)"
    );
    assert!(analysis.local_writes.contains("count"));
}

#[test]
fn test_no_captured_reads_for_new_local_definition() {
    // [:x | temp := x * 2. temp + 1] — `temp` is defined then read (not captured)
    let block = Block::new(
        vec![BlockParameter::new("x", Span::new(1, 2))],
        vec![
            bare(Expression::Assignment {
                target: Box::new(make_expr_id("temp")),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(make_expr_id("x")),
                    selector: MessageSelector::Binary("*".into()),
                    arguments: vec![Expression::Literal(
                        crate::ast::Literal::Integer(2),
                        Span::new(16, 17),
                    )],
                    is_cast: false,
                    span: Span::new(9, 17),
                }),
                type_annotation: None,
                span: Span::new(0, 17),
            }),
            bare(Expression::MessageSend {
                receiver: Box::new(make_expr_id("temp")),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(
                    crate::ast::Literal::Integer(1),
                    Span::new(26, 27),
                )],
                is_cast: false,
                span: Span::new(19, 27),
            }),
        ],
        Span::new(0, 29),
    );
    let analysis = analyze_block(&block);
    assert!(
        !analysis.captured_reads.contains("temp"),
        "temp should NOT be a captured read (defined locally before use)"
    );
    assert!(analysis.local_writes.contains("temp"));
    assert!(analysis.local_reads.contains("temp"));
}

#[test]
fn test_destructure_assignment_binds_variables() {
    // [{a, b} := expr. a + b] — a and b must be local bindings after destructure
    use crate::ast::Pattern;

    let tuple_pattern = Pattern::Tuple {
        elements: vec![
            Pattern::Variable(make_id("a")),
            Pattern::Variable(make_id("b")),
        ],
        span: Span::new(1, 7),
    };

    let block = Block::new(
        vec![],
        vec![
            bare(Expression::DestructureAssignment {
                pattern: tuple_pattern,
                value: Box::new(make_expr_id("someTuple")),
                span: Span::new(0, 20),
            }),
            bare(Expression::MessageSend {
                receiver: Box::new(make_expr_id("a")),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![make_expr_id("b")],
                is_cast: false,
                span: Span::new(22, 30),
            }),
        ],
        Span::new(0, 32),
    );
    let analysis = analyze_block(&block);

    // a and b are locally defined by the destructure — not captured from outer scope
    assert!(
        analysis.local_writes.contains("a"),
        "a should be in local_writes after destructure"
    );
    assert!(
        analysis.local_writes.contains("b"),
        "b should be in local_writes after destructure"
    );
    assert!(
        !analysis.captured_reads.contains("a"),
        "a should NOT be a captured read"
    );
    assert!(
        !analysis.captured_reads.contains("b"),
        "b should NOT be a captured read"
    );
    assert!(analysis.local_reads.contains("a"));
    assert!(analysis.local_reads.contains("b"));
}

#[test]
fn test_array_destructure_binds_variables() {
    // [#[first, second] := arr. first] — first and second are local after destructure
    use crate::ast::Pattern;

    let array_pattern = Pattern::Array {
        elements: vec![
            Pattern::Variable(make_id("first")),
            Pattern::Variable(make_id("second")),
        ],
        list_syntax: false,
        rest: None,
        span: Span::new(1, 15),
    };

    let block = Block::new(
        vec![],
        vec![
            bare(Expression::DestructureAssignment {
                pattern: array_pattern,
                value: Box::new(make_expr_id("arr")),
                span: Span::new(0, 20),
            }),
            // Read both bound variables so captured_reads assertions are meaningful
            bare(Expression::MessageSend {
                receiver: Box::new(make_expr_id("first")),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![make_expr_id("second")],
                is_cast: false,
                span: Span::new(22, 30),
            }),
        ],
        Span::new(0, 32),
    );
    let analysis = analyze_block(&block);

    assert!(analysis.local_writes.contains("first"));
    assert!(analysis.local_writes.contains("second"));
    assert!(!analysis.captured_reads.contains("first"));
    assert!(!analysis.captured_reads.contains("second"));
}

#[test]
fn test_binary_destructure_size_expr_recorded_as_read() {
    // [<<payload:len/binary>> := bin] where `len` is a variable —
    // the size expression `len` must appear in local_reads/captured_reads.
    use crate::ast::{BinarySegment, Pattern};

    let binary_pattern = Pattern::Binary {
        segments: vec![BinarySegment {
            value: Pattern::Variable(make_id("payload")),
            size: Some(Box::new(make_expr_id("len"))),
            segment_type: None,
            signedness: None,
            endianness: None,
            unit: None,
            span: Span::new(2, 14),
        }],
        span: Span::new(0, 16),
    };

    let block = Block::new(
        vec![],
        vec![bare(Expression::DestructureAssignment {
            pattern: binary_pattern,
            value: Box::new(make_expr_id("bin")),
            span: Span::new(0, 25),
        })],
        Span::new(0, 27),
    );
    let analysis = analyze_block(&block);

    // `payload` is a binding introduced by the pattern
    assert!(
        analysis.local_writes.contains("payload"),
        "payload should be in local_writes"
    );
    // `len` is read as a size expression — should appear in local_reads
    assert!(
        analysis.local_reads.contains("len"),
        "len (size expression) should be in local_reads"
    );
    // `len` is not locally defined, so it should be a captured read
    assert!(
        analysis.captured_reads.contains("len"),
        "len should be in captured_reads (read before definition)"
    );
}

#[test]
fn test_binary_forward_ref_size_is_captured_read() {
    // <<payload:len/binary, len:8>> — the size expression
    // `len` in segment 1 must still be a captured read even though `len` is
    // bound by segment 2 of the same binary pattern.
    use crate::ast::{BinarySegment, Pattern};

    let binary_pattern = Pattern::Binary {
        segments: vec![
            // Segment 1: payload:len/binary  (size refers to `len`, not yet bound)
            BinarySegment {
                value: Pattern::Variable(make_id("payload")),
                size: Some(Box::new(make_expr_id("len"))),
                segment_type: None,
                signedness: None,
                endianness: None,
                unit: None,
                span: Span::new(2, 16),
            },
            // Segment 2: len:8  (binds `len`)
            BinarySegment {
                value: Pattern::Variable(make_id("len")),
                size: Some(Box::new(Expression::Literal(
                    crate::ast::Literal::Integer(8),
                    Span::new(19, 20),
                ))),
                segment_type: None,
                signedness: None,
                endianness: None,
                unit: None,
                span: Span::new(18, 22),
            },
        ],
        span: Span::new(0, 24),
    };

    let block = Block::new(
        vec![],
        vec![bare(Expression::DestructureAssignment {
            pattern: binary_pattern,
            value: Box::new(make_expr_id("bin")),
            span: Span::new(0, 30),
        })],
        Span::new(0, 32),
    );
    let analysis = analyze_block(&block);

    // Both pattern variables must be recorded as writes
    assert!(
        analysis.local_writes.contains("payload"),
        "payload should be in local_writes"
    );
    assert!(
        analysis.local_writes.contains("len"),
        "len should be in local_writes"
    );
    // `len` is used as a size expression before it is bound by segment 2;
    // it must be a captured read (read before local definition).
    assert!(
        analysis.local_reads.contains("len"),
        "len (size expression) should be in local_reads"
    );
    assert!(
        analysis.captured_reads.contains("len"),
        "len should be in captured_reads — it is read before it is bound"
    );
}

#[test]
fn test_block_param_read_is_not_captured() {
    // [:x | x + 1] — `x` is a block param, not a captured read
    let block = Block::new(
        vec![BlockParameter::new("x", Span::new(1, 2))],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(make_expr_id("x")),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(
                crate::ast::Literal::Integer(1),
                Span::new(6, 7),
            )],
            is_cast: false,
            span: Span::new(3, 7),
        })],
        Span::new(0, 9),
    );
    let analysis = analyze_block(&block);
    assert!(
        !analysis.captured_reads.contains("x"),
        "block parameter should NOT be in captured_reads"
    );
    assert!(analysis.local_reads.contains("x"));
}

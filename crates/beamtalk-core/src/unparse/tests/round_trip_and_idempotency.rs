// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Parse → unparse → parse round-tripping, idempotency (including against the
//! `getting-started` examples and `fixtures/` corpus), and identity
//! (already-canonical source is a formatting no-op) tests.

use super::common::*;

// --- Round-trip: parse → unparse → parse ---

#[test]
fn round_trip_simple_expression() {
    let source = "42";
    let module = parse_source(source);
    let unparsed = unparse_module(&module);
    let module2 = parse_source(&unparsed);
    assert_eq!(module.expressions.len(), module2.expressions.len());
    // Expressions should be structurally equivalent (ignoring spans)
    for (a, b) in module.expressions.iter().zip(module2.expressions.iter()) {
        assert!(
            expressions_equivalent(&a.expression, &b.expression),
            "Round-trip mismatch:\n  original: {a:?}\n  after round-trip: {b:?}"
        );
    }
}

#[test]
fn round_trip_assignment() {
    let source = "x := 42";
    let module = parse_source(source);
    let unparsed = unparse_module(&module);
    let module2 = parse_source(&unparsed);
    assert_eq!(module.expressions.len(), module2.expressions.len());
    for (a, b) in module.expressions.iter().zip(module2.expressions.iter()) {
        assert!(expressions_equivalent(&a.expression, &b.expression));
    }
}

#[test]
fn round_trip_message_send() {
    let source = "x size";
    let module = parse_source(source);
    let unparsed = unparse_module(&module);
    let module2 = parse_source(&unparsed);
    assert_eq!(module.expressions.len(), module2.expressions.len());
    for (a, b) in module.expressions.iter().zip(module2.expressions.iter()) {
        assert!(
            expressions_equivalent(&a.expression, &b.expression),
            "Round-trip mismatch:\n  original: {a:?}\n  after round-trip: {b:?}"
        );
    }
}

#[test]
fn round_trip_class_definition() {
    let source = "Actor subclass: Counter\n  state: value = 0\n\n  getValue => self.value";
    let module = parse_source(source);
    let unparsed = unparse_module(&module);
    let module2 = parse_source(&unparsed);
    // Class count must match
    assert_eq!(
        module.classes.len(),
        module2.classes.len(),
        "Class count mismatch after round-trip.\n  unparsed: {unparsed:?}"
    );
    // Class name must match
    assert_eq!(module.classes[0].name.name, module2.classes[0].name.name);
    // Method count must match
    assert_eq!(
        module.classes[0].methods.len(),
        module2.classes[0].methods.len()
    );
}

#[test]
fn round_trip_class_with_native_keyword() {
    let source = "Actor subclass: Foo native: my_module\n  doIt => 42";
    let module = parse_source(source);
    let unparsed = unparse_module(&module);
    let module2 = parse_source(&unparsed);
    assert_eq!(module2.classes.len(), 1);
    assert_eq!(
        module2.classes[0]
            .backing_module
            .as_ref()
            .map(|id| id.name.as_str()),
        Some("my_module"),
        "backing_module lost after round-trip.\n  unparsed: {unparsed:?}"
    );
    assert_eq!(module2.classes[0].methods.len(), 1);
}

/// Structural equivalence check ignoring spans.
fn expressions_equivalent(a: &Expression, b: &Expression) -> bool {
    match (a, b) {
        (Expression::Literal(la, _), Expression::Literal(lb, _)) => la == lb,
        (Expression::Identifier(ia), Expression::Identifier(ib)) => ia.name == ib.name,
        (
            Expression::Assignment {
                target: ta,
                value: va,
                ..
            },
            Expression::Assignment {
                target: tb,
                value: vb,
                ..
            },
        ) => expressions_equivalent(ta, tb) && expressions_equivalent(va, vb),
        (
            Expression::MessageSend {
                receiver: ra,
                selector: sa,
                arguments: aa,
                is_cast: ca,
                ..
            },
            Expression::MessageSend {
                receiver: rb,
                selector: sb,
                arguments: ab,
                is_cast: cb,
                ..
            },
        ) => {
            ca == cb
                && sa == sb
                && expressions_equivalent(ra, rb)
                && aa.len() == ab.len()
                && aa.iter().zip(ab).all(|(x, y)| expressions_equivalent(x, y))
        }
        _ => false,
    }
}

// --- Idempotency tests with realistic source strings ---

#[test]
fn idempotent_line_comment() {
    assert_idempotent("// a line comment\nx := 42\n");
}

#[test]
fn idempotent_license_header() {
    assert_idempotent(
        "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\nx := 1\n",
    );
}

#[test]
fn idempotent_class_with_line_comment() {
    assert_idempotent("// A useful class\nObject subclass: Foo\n  bar => 42\n");
}

#[test]
fn idempotent_class_definition() {
    assert_idempotent("Actor subclass: Counter\n  state: value = 0\n\n  getValue => self.value\n");
}

// --- Blank line round-trip (BT-987) ---

#[test]
fn idempotent_method_body_blank_lines() {
    assert_idempotent("Object subclass: Foo\n\n  doStuff =>\n    x := 1\n\n    x + 1\n");
}

#[test]
fn idempotent_method_body_no_blank_lines() {
    assert_idempotent("Object subclass: Foo\n\n  doStuff =>\n    x := 1\n    x + 1\n");
}

#[test]
fn consecutive_blank_lines_collapsed() {
    // Multiple blank lines should collapse to a single one
    let source = "Object subclass: Foo\n\n  doStuff =>\n    x := 1\n\n\n\n    x + 1\n";
    let pass1 = unparse_module(&parse_source(source));
    // Count actual blank lines between x := 1 and x + 1
    let between = pass1
        .split("x := 1")
        .nth(1)
        .unwrap()
        .split("x + 1")
        .next()
        .unwrap();
    let newline_count = between.chars().filter(|&c| c == '\n').count();
    // Should have exactly 2 newlines (one for end of `x := 1` line, one blank line)
    assert_eq!(
        newline_count, 2,
        "Expected 2 newlines (= 1 blank line) between statements, got {newline_count} in: {between:?}"
    );
    // And it should be idempotent after that
    let pass2 = unparse_module(&parse_source(&pass1));
    assert_eq!(pass1, pass2, "Not idempotent after blank line collapse");
}

#[test]
fn idempotent_module_level_blank_lines() {
    assert_idempotent("x := 1\n\ny := 2\n");
}

// --- Block formatting ---

#[test]
fn short_block_renders_inline() {
    // A short single-statement block fits within 80 columns → inline
    let source = "x ifTrue: [^1]";
    let module = parse_source(source);
    let out = unparse_module(&module);
    assert_eq!(out, "x ifTrue: [^1]");
}

#[test]
fn short_block_with_param_renders_inline() {
    let source = "coll do: [:x | x println]";
    let module = parse_source(source);
    let out = unparse_module(&module);
    assert_eq!(out, "coll do: [:x | x println]");
}

#[test]
fn multi_statement_block_always_breaks() {
    let source = "[\n  x println.\n  y println\n]";
    let module = parse_source(source);
    let out = unparse_module(&module);
    // Multi-statement: always broken, newlines separate statements (no dots)
    assert!(
        out.contains("x println\n  y println"),
        "expected broken multi-stmt block without dots in: {out:?}"
    );
}

#[test]
fn long_block_breaks() {
    // Construct a block whose content exceeds 80 columns
    let long_name = "aVeryLongVariableNameThatDefinitelyExceedsTheLineWidthLimitWhenInsideABlock";
    let source = format!("x ifTrue: [{long_name}]");
    let module = parse_source(&source);
    let out = unparse_module(&module);
    // Should break: body on next line with 2-space indent, ] on its own line
    assert!(
        out.contains(&format!("[\n  {long_name}\n]")),
        "expected broken block in: {out:?}"
    );
}

// --- Idempotency tests for block formatting ---

#[test]
fn idempotent_short_block_inline() {
    assert_idempotent("x ifTrue: [^1]\n");
}

#[test]
fn idempotent_block_with_param() {
    assert_idempotent("coll do: [:x | x println]\n");
}

#[test]
fn idempotent_multi_statement_block() {
    assert_idempotent("[\n  x println.\n  y println\n]\n");
}

#[test]
fn idempotent_iftrue_guard() {
    assert_idempotent("flag ifTrue: [^42]\n");
}

// --- Idempotency tests using example .bt files ---

#[test]
fn idempotent_hello_bt() {
    assert_idempotent(include_str!(
        "../../../../../examples/getting-started/src/hello.bt"
    ));
}

#[test]
fn idempotent_hanoi_bt() {
    assert_idempotent(include_str!(
        "../../../../../examples/getting-started/src/hanoi.bt"
    ));
}

#[test]
fn idempotent_point_bt() {
    assert_idempotent(include_str!(
        "../../../../../examples/getting-started/src/point.bt"
    ));
}

// --- Identity tests: first-pass must be a no-op on canonical source ---

#[test]
fn identity_hello_bt() {
    assert_identity(include_str!(
        "../../../../../examples/getting-started/src/hello.bt"
    ));
}

#[test]
fn identity_hanoi_bt() {
    assert_identity(include_str!(
        "../../../../../examples/getting-started/src/hanoi.bt"
    ));
}

#[test]
fn identity_point_bt() {
    assert_identity(include_str!(
        "../../../../../examples/getting-started/src/point.bt"
    ));
}

#[test]
fn identity_counter_bt() {
    assert_identity(include_str!(
        "../../../../../examples/getting-started/src/counter.bt"
    ));
}

#[test]
fn identity_logging_counter_bt() {
    assert_identity(include_str!(
        "../../../../../examples/getting-started/src/logging_counter.bt"
    ));
}

#[test]
fn identity_protoobject_proxy_bt() {
    assert_identity(include_str!(
        "../../../../../examples/getting-started/src/transparent_proxy.bt"
    ));
}

// --- Identity tests: fixture files ---

#[test]
fn identity_fixture_value_class() {
    assert_identity(include_str!("../fixtures/value_class.bt"));
}

#[test]
fn identity_fixture_actor_class() {
    assert_identity(include_str!("../fixtures/actor_class.bt"));
}

#[test]
fn identity_fixture_method_comments() {
    assert_identity(include_str!("../fixtures/method_comments.bt"));
}

#[test]
fn identity_fixture_blocks() {
    assert_identity(include_str!("../fixtures/blocks.bt"));
}

#[test]
fn identity_fixture_standalone_methods() {
    assert_identity(include_str!("../fixtures/standalone_methods.bt"));
}

#[test]
fn identity_fixture_keyword_blocks() {
    assert_identity(include_str!("../fixtures/keyword_blocks.bt"));
}

#[test]
fn identity_fixture_long_keyword_messages() {
    assert_identity(include_str!("../fixtures/long_keyword_messages.bt"));
}

#[test]
fn identity_empty_source() {
    assert_identity("");
}

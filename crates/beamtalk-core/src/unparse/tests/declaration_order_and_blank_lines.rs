// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-definition header trailing comments (BT-2933), `handleScope:` unparse
//! emission (BT-2942), doc-comment/type-alias interactions (BT-2924),
//! top-level declaration order (BT-2907), and blank-line preservation between
//! top-level declarations and section boundaries (BT-2929, BT-2945).

use super::common::*;

// --- Class definition header trailing comment (BT-2933) ---

#[test]
fn class_header_trailing_comment_round_trip() {
    // BT-2933: a trailing end-of-line comment on the `subclass:` header
    // line must round-trip losslessly, mirroring the identical fix for
    // type alias/protocol declarations (BT-2906). Before the fix,
    // `parse_class_definition` never populated `comments.trailing`, so
    // `unparse_class_definition`'s trailing-comment branch was dead code
    // and the comment was silently dropped.
    // A class with no `state:` declarations always gets a blank line
    // before its first method (canonical formatting, unrelated to this
    // fix), hence the blank line in the expected output below.
    let source = concat!(
        "Object subclass: Foo  // header comment\n",
        "\n",
        "  x => 1\n",
    );
    assert_identity(source);
}

#[test]
fn class_header_trailing_comment_with_type_params_round_trip() {
    // BT-2933: the trailing comment attaches after the last header
    // token — here, the type parameter list.
    let source = concat!(
        "Object subclass: Box(T)  // header comment\n",
        "\n",
        "  x => 1\n",
    );
    assert_identity(source);
}

#[test]
fn class_header_trailing_comment_with_native_round_trip() {
    // BT-2933: the trailing comment attaches after the `native:` module
    // name — the last header token when present.
    let source = concat!(
        "Object subclass: Foo native: my_module  // header comment\n",
        "\n",
        "  x => 1\n",
    );
    assert_identity(source);
}

#[test]
fn class_header_trailing_comment_with_state_round_trip() {
    // BT-2933 (review follow-up): the most common real-world class
    // shape — a header trailing comment plus a `state:` declaration.
    let source = concat!(
        "Object subclass: Counter  // counter class\n",
        "  state: count :: Integer = 0\n",
        "\n",
        "  increment => count := count + 1\n",
    );
    assert_identity(source);
}

// --- `handleScope:` unparse emission + header trailing comment (BT-2942) ---

#[test]
fn handle_scope_round_trip() {
    // BT-2942: the unparser never emitted `handleScope:` at all, so a
    // class declaring it didn't round-trip. Canonical style (ADR 0103)
    // puts the clause on its own indented line following the header.
    let source = concat!(
        "sealed typed Object subclass: MetricsTable\n",
        "  handleScope: #node\n",
    );
    assert_identity(source);
}

#[test]
fn handle_scope_with_state_and_methods_round_trip() {
    // BT-2942: `handleScope:` alongside a state declaration and a method,
    // exercising the blank-line-before-first-method logic together with
    // the new `handleScope:` line.
    let source = concat!(
        "Object subclass: Registry\n",
        "  handleScope: #process\n",
        "  state: count :: Integer = 0\n",
        "\n",
        "  increment => count := count + 1\n",
    );
    assert_identity(source);
}

#[test]
fn handle_scope_on_new_line_with_header_trailing_comment_round_trip() {
    // BT-2942: when `handleScope:` sits on its own line (the canonical
    // style), a trailing comment on the class header line itself (e.g.
    // `Object subclass: Foo  // comment`) lives in the class-name
    // token's trailing trivia, not `handleScope:`'s `#symbol` token's.
    // Before the fix, `collect_trailing_comment()` unconditionally
    // inspected `current - 1` *after* parsing `handleScope:`, landing on
    // the `#symbol` token and silently dropping the header comment.
    let source = concat!(
        "Object subclass: Foo  // header comment\n",
        "  handleScope: #node\n",
        "\n",
        "  x => 1\n",
    );
    assert_identity(source);
}

#[test]
fn handle_scope_with_native_and_header_trailing_comment_round_trip() {
    // BT-2942: header clauses are parsed in a fixed order — `native:`
    // then `handleScope:` (ADR 0103) — so the trailing-comment anchor
    // must still be the `native:` module token, not the class name,
    // when both a `native:` clause and a following-line `handleScope:`
    // are present.
    let source = concat!(
        "Object subclass: Foo native: my_module  // header comment\n",
        "  handleScope: #node\n",
        "\n",
        "  x => 1\n",
    );
    assert_identity(source);
}

// --- BT-2924 regression: `type` alias sandwiched between a class's doc
// comment and the class itself must not lose the class's doc comment. ---

/// The exact repro shape from BT-2924: a class's doc comment, a blank
/// line, a `type` alias with its own directly-adjacent doc comment, a
/// blank line, then the class declaration the first doc comment
/// describes.
///
/// Before the fix, `format_source` deleted the class's doc comment
/// outright (it lives in the `type` alias token's leading trivia, and
/// `collect_doc_comment` kept only the alias's own — nearer — block,
/// discarding the earlier one without preserving it anywhere).
const HTTPSERVER_REPRO_SOURCE: &str = concat!(
    "/// HTTPServer — cowboy-backed HTTP server actor for Beamtalk.\n",
    "/// Some more description text here.\n",
    "/// @see HTTPResponse (for building response objects)\n",
    "\n",
    "/// Anything `HTTPServer start:handler:` accepts as a request handler.\n",
    "type HTTPHandlerLike = Integer | String\n",
    "\n",
    "Actor subclass: HTTPServer\n",
    "  start => 1\n",
);

#[test]
fn type_alias_preceded_by_orphaned_class_doc_preserves_all_text() {
    let formatted = format_source(HTTPSERVER_REPRO_SOURCE)
        .expect("format_source must succeed for valid source");

    // Neither doc comment's content may be dropped.
    for line in [
        "/// HTTPServer — cowboy-backed HTTP server actor for Beamtalk.",
        "/// Some more description text here.",
        "/// @see HTTPResponse (for building response objects)",
        "/// Anything `HTTPServer start:handler:` accepts as a request handler.",
    ] {
        assert!(
            formatted.contains(line),
            "formatted output lost {line:?}:\n{formatted}"
        );
    }

    // Formatting must be idempotent — a second pass changes nothing.
    let formatted_again =
        format_source(&formatted).expect("format_source must succeed on formatted output");
    assert_eq!(
        formatted, formatted_again,
        "format_source is not idempotent for the BT-2924 repro shape"
    );
}

#[test]
fn type_alias_preceded_by_orphaned_class_doc_reattaches_correctly_on_reparse() {
    let formatted = format_source(HTTPSERVER_REPRO_SOURCE)
        .expect("format_source must succeed for valid source");
    let module = parse_source(&formatted);

    assert_eq!(module.type_aliases.len(), 1);
    assert_eq!(
        module.type_aliases[0].doc_comment.as_deref(),
        Some("Anything `HTTPServer start:handler:` accepts as a request handler."),
        "the alias's own doc comment must still attach to the alias after a format round-trip"
    );
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].name.name, "HTTPServer");
}

#[test]
fn type_alias_with_directly_adjacent_doc_comment_has_no_unattached_warning() {
    // BT-2924 secondary bug: the lint must not flag the alias's own
    // directly-adjacent `///` comment as unattached just because an
    // earlier, unrelated block shares the same leading trivia.
    use crate::source_analysis::{Severity, lex_with_eof, parse};
    let tokens = lex_with_eof(HTTPSERVER_REPRO_SOURCE);
    let (_module, diagnostics) = parse(tokens);
    let warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Warning)
        .collect();
    assert!(
        warnings.is_empty(),
        "expected no warnings for the BT-2924 repro shape, got: {warnings:?}"
    );
}

// --- BT-2907: top-level declaration order (classes/protocols/type aliases) ---
//
// `unparse_module_doc` used to always emit type aliases first, then
// protocols, then classes — regardless of where each declaration
// appeared in the source. A `type` alias (or a protocol) declared
// *after* a class would jump to the top of the file on a format
// round-trip. The three declaration kinds are now interleaved back into
// their original source order.

#[test]
fn type_alias_after_class_preserves_source_order() {
    // Before the fix, this `type` alias — declared after the class —
    // would have been hoisted above `Actor subclass: Server`.
    let source = concat!(
        "Actor subclass: Server\n",
        "  start => 1\n",
        "\n",
        "type Port = Integer\n",
    );
    let formatted = format_source(source).expect("format_source must succeed for valid source");
    let class_pos = formatted
        .find("Actor subclass: Server")
        .expect("class declaration must be present");
    let alias_pos = formatted
        .find("type Port = Integer")
        .expect("type alias declaration must be present");
    assert!(
        class_pos < alias_pos,
        "a type alias declared after a class must stay after it on format; \
         formatted output:\n{formatted}"
    );

    // Idempotent: formatting the already-formatted output changes nothing.
    let formatted_again =
        format_source(&formatted).expect("format_source must succeed on formatted output");
    assert_eq!(
        formatted, formatted_again,
        "format_source is not idempotent"
    );
}

#[test]
fn protocol_after_class_preserves_source_order() {
    // Same category of bug as the type-alias case above, for protocols:
    // before the fix, a protocol declared after a class would jump above it.
    let source = concat!(
        "Actor subclass: Server\n",
        "  start => 1\n",
        "\n",
        "Protocol define: Startable\n",
        "  start -> Integer\n",
    );
    let formatted = format_source(source).expect("format_source must succeed for valid source");
    let class_pos = formatted
        .find("Actor subclass: Server")
        .expect("class declaration must be present");
    let protocol_pos = formatted
        .find("Protocol define: Startable")
        .expect("protocol declaration must be present");
    assert!(
        class_pos < protocol_pos,
        "a protocol declared after a class must stay after it on format; \
         formatted output:\n{formatted}"
    );

    let formatted_again =
        format_source(&formatted).expect("format_source must succeed on formatted output");
    assert_eq!(
        formatted, formatted_again,
        "format_source is not idempotent"
    );
}

#[test]
fn class_protocol_type_alias_interleaved_preserves_relative_order() {
    // Classes, protocols, and type aliases interleaved in source must
    // come back out in the same relative order — not grouped by kind.
    let source = concat!(
        "type Port = Integer\n",
        "\n",
        "Protocol define: Startable\n",
        "  start -> Integer\n",
        "\n",
        "Actor subclass: Server\n",
        "  start => 1\n",
        "\n",
        "type Timeout = Integer\n",
        "\n",
        "Actor subclass: Client\n",
        "  connect => 1\n",
    );
    let formatted = format_source(source).expect("format_source must succeed for valid source");

    let positions = [
        ("type Port = Integer", "type Port"),
        ("Protocol define: Startable", "Protocol define: Startable"),
        ("Actor subclass: Server", "Actor subclass: Server"),
        ("type Timeout = Integer", "type Timeout"),
        ("Actor subclass: Client", "Actor subclass: Client"),
    ]
    .map(|(needle, label)| {
        (
            formatted
                .find(needle)
                .unwrap_or_else(|| panic!("{label} must be present:\n{formatted}")),
            label,
        )
    });

    for pair in positions.windows(2) {
        let (prev_pos, prev_label) = pair[0];
        let (next_pos, next_label) = pair[1];
        assert!(
            prev_pos < next_pos,
            "{prev_label} must come before {next_label} on format; \
             formatted output:\n{formatted}"
        );
    }

    let formatted_again =
        format_source(&formatted).expect("format_source must succeed on formatted output");
    assert_eq!(
        formatted, formatted_again,
        "format_source is not idempotent"
    );
}

// --- BT-2929: blank line between top-level declarations ---
//
// `unparse_module_doc` used to always emit exactly one `line()` after
// each top-level declaration, regardless of whether the source had a
// blank line there — so the separating blank line was either silently
// dropped, or (for a class) looked "relocated" into the class's own
// body, since a class unconditionally gets a blank line before its
// first method whether or not it has state declarations (an unrelated,
// pre-existing formatting rule — see the `Blank line before first
// method` comment in `unparse_class_definition`). All three assertions
// below use `assert_identity` with sources that are already in that
// canonical form, so a regression here fails on the *first* format
// pass rather than requiring a second one to notice non-idempotency.

#[test]
fn blank_line_preserved_type_alias_then_class() {
    // Repro A from BT-2929.
    assert_identity(concat!(
        "type Port = Integer\n",
        "\n",
        "Object subclass: Foo\n",
        "\n",
        "  m => 1\n",
    ));
}

#[test]
fn blank_line_preserved_class_then_class() {
    // Repro B from BT-2929 — confirms the bug wasn't type-alias-specific.
    assert_identity(concat!(
        "Object subclass: A\n",
        "\n",
        "  m => 1\n",
        "\n",
        "Object subclass: B\n",
        "\n",
        "  n => 2\n",
    ));
}

#[test]
fn blank_line_preserved_type_alias_then_type_alias() {
    // Repro C from BT-2929 — the blank line was dropped outright here
    // (neither declaration has a body to "absorb" it into).
    assert_identity(concat!(
        "type Port = Integer\n",
        "\n",
        "type Timeout = Integer\n",
    ));
}

#[test]
fn no_blank_line_between_top_level_declarations_stays_absent() {
    // The converse of the three tests above: declarations with *no*
    // blank line between them in the source must not gain one.
    assert_identity(concat!("type Port = Integer\n", "type Timeout = Integer\n",));
}

#[test]
fn blank_line_before_first_top_level_declaration_is_dropped() {
    // BT-2929 (review follow-up): the `i > 0` guard in
    // `unparse_module_doc`'s loop intentionally does not preserve a
    // blank line before the very first top-level declaration — this
    // predates BT-2929 and is documented in the loop's comment.
    // Confirmed here with an explicit regression test rather than only
    // a code comment.
    //
    // Two leading newlines are required, not one: a single `\n` is
    // below `has_blank_line_before_first_comment()`'s `>= 2` threshold,
    // so it would produce `leading_blank_line: false` and never
    // actually exercise the `i > 0` guard this test targets.
    let source = "\n\ntype Port = Integer\n";
    let formatted = format_source(source).expect("format_source must succeed");
    assert_eq!(formatted, "type Port = Integer\n");
}

#[test]
fn multiple_blank_lines_between_top_level_declarations_normalised_to_one() {
    // BT-2929 (review follow-up): `has_blank_line_before_first_comment`
    // treats any run of 2+ blank lines as "a blank line preceded this
    // node" — the unparser always re-emits exactly one, so multiple
    // blank lines collapse to one on format (standard normalisation),
    // not a bug. Two blank lines in source...
    let source = "type Port = Integer\n\n\ntype Timeout = Integer\n";
    let formatted = format_source(source).expect("format_source must succeed");
    // ...become one blank line in the formatted output...
    assert_eq!(formatted, "type Port = Integer\n\ntype Timeout = Integer\n");
    // ...and that single-blank-line form is stable under a second pass.
    assert_identity(&formatted);
}

#[test]
fn blank_line_preserved_across_all_three_declaration_kinds_interleaved() {
    // Full combination: type alias, protocol, and class, each separated
    // by a blank line — every pairwise gap (type-alias/protocol,
    // protocol/class, class/type-alias) must round-trip.
    assert_identity(concat!(
        "type Port = Integer\n",
        "\n",
        "Protocol define: Startable\n",
        "  start -> Integer\n",
        "\n",
        "Object subclass: Server\n",
        "\n",
        "  start => 1\n",
        "\n",
        "type Timeout = Integer\n",
    ));
}

// --- Blank line at section boundaries: declarations / standalone methods
// / expressions (BT-2943) ---
//
// BT-2929 only fixed blank-line preservation *within* the interleaved
// class/protocol/type-alias declaration section. `Module`'s other two
// sections — standalone methods (`Class >> method => body`) and top-level
// expressions — are always rendered after it, in that fixed order. These
// tests cover the three section *boundaries* where a blank line was still
// silently dropped: declarations→standalone-methods,
// standalone-method→standalone-method, and
// standalone-methods→expressions.

#[test]
fn blank_line_preserved_between_declaration_and_standalone_method() {
    assert_identity(concat!(
        "Object subclass: Foo\n",
        "\n",
        "  m => 1\n",
        "\n",
        "Foo >> bar => 2\n",
    ));
}

#[test]
fn no_blank_line_between_declaration_and_standalone_method_stays_absent() {
    assert_identity(concat!(
        "Object subclass: Foo\n",
        "\n",
        "  m => 1\n",
        "Foo >> bar => 2\n",
    ));
}

#[test]
fn blank_line_preserved_between_standalone_methods() {
    assert_identity(concat!(
        "Counter >> increment => self.value := self.value + 1\n",
        "\n",
        "Counter >> decrement => self.value := self.value - 1\n",
    ));
}

#[test]
fn no_blank_line_between_standalone_methods_stays_absent() {
    assert_identity(concat!(
        "Counter >> increment => self.value := self.value + 1\n",
        "Counter >> decrement => self.value := self.value - 1\n",
    ));
}

#[test]
fn blank_line_before_first_standalone_method_is_dropped() {
    // Mirrors `blank_line_before_first_top_level_declaration_is_dropped`:
    // a blank line before the very first construct in the file — here, a
    // standalone method with no preceding declaration section — is
    // dropped, not preserved. Two leading newlines are required (see that
    // test's comment for why one is not enough).
    let source = "\n\nCounter >> increment => self.value := self.value + 1\n";
    let formatted = format_source(source).expect("format_source must succeed");
    assert_eq!(
        formatted,
        "Counter >> increment => self.value := self.value + 1\n"
    );
}

#[test]
fn blank_line_preserved_between_standalone_method_and_expression() {
    assert_identity(concat!(
        "Counter >> increment => self.value := self.value + 1\n",
        "\n",
        "x := 1\n",
    ));
}

#[test]
fn no_blank_line_between_standalone_method_and_expression_stays_absent() {
    assert_identity(concat!(
        "Counter >> increment => self.value := self.value + 1\n",
        "x := 1\n",
    ));
}

#[test]
fn blank_line_preserved_across_all_three_section_boundaries() {
    // Full combination: declaration, two standalone methods, and an
    // expression, each separated by a blank line — every boundary from
    // BT-2943's acceptance criteria in one round-trip.
    assert_identity(concat!(
        "Object subclass: Foo\n",
        "\n",
        "  m => 1\n",
        "\n",
        "Foo >> bar => 2\n",
        "\n",
        "Foo >> baz => 3\n",
        "\n",
        "x := 1\n",
    ));
}

#[test]
fn blank_line_before_class_name_wins_over_internal_selector_comment_blank_line() {
    // BT-2943 regression: when a standalone method has no leading comment
    // before its class name, but does have one wedged between `>>` and
    // the selector (unusual, but the parser doesn't reject it), the
    // blank-line signal used for module-level section-boundary spacing
    // must still come from the position right before the class-name
    // token — the true start of the construct — not from
    // `parse_method_definition()`'s own, unrelated collect for that
    // inner comment (which, on its own, sees no blank line here).
    let source = concat!(
        "Counter >> increment => self.value := self.value + 1\n",
        "\n",
        "Counter >> // note\n",
        "  decrement => self.value := self.value - 1\n",
    );
    let module = parse_source(source);
    assert_eq!(module.method_definitions.len(), 2);
    assert!(
        module.method_definitions[1]
            .method
            .comments
            .leading_blank_line,
        "expected the blank line before the second standalone method's \
         class name to be preserved: {:#?}",
        module.method_definitions[1].method.comments
    );
}

#[test]
fn blank_line_preserved_between_protocol_and_standalone_method() {
    // The declarations→standalone-methods boundary must round-trip for
    // every declaration kind, not just `Object subclass:` — protocols and
    // type aliases go through the same `TopLevelDecl` interleaving.
    assert_identity(concat!(
        "Protocol define: Startable\n",
        "  start -> Integer\n",
        "\n",
        "Counter >> start => 1\n",
    ));
}

#[test]
fn blank_line_preserved_between_type_alias_and_expression_directly() {
    // Declarations→expressions with no standalone-methods section in
    // between — exercises the same `is_first_module_item` check in
    // `parse_module`, but with `method_definitions` empty rather than
    // `classes`/`protocols`/`type_aliases`.
    assert_identity(concat!("type Port = Integer\n", "\n", "x := 1\n",));
}

#[test]
fn blank_line_preserved_before_class_side_standalone_method() {
    // The blank-line signal is captured before the class-name token,
    // ahead of the optional `class` modifier — must still work for
    // class-side standalone methods (`Counter class >> ...`).
    assert_identity(concat!(
        "Counter >> increment => self.value := self.value + 1\n",
        "\n",
        "Counter class >> withInitial: n => n\n",
    ));
}

#[test]
fn blank_line_preserved_before_package_qualified_standalone_method() {
    // Same, for a package-qualified standalone method
    // (`package@Class >> ...`, ADR 0070) — the blank line must be
    // captured before the package identifier, not lost when the
    // `identifier @ Identifier` pair is parsed.
    assert_identity(concat!(
        "Counter >> increment => self.value := self.value + 1\n",
        "\n",
        "json@Parser >> lenientParse: input => input\n",
    ));
}

// --- Blank line between a leading comment and its declaration (BT-2945) ---
//
// `has_blank_line_before_first_comment` (BT-2929's `leading_blank_line`)
// only detects a blank line *before* the whole leading-comment block; it
// has no way to represent a blank line *inside* that block, between the
// last comment and the declaration itself. `blank_line_after_comments`
// fills that gap.

#[test]
fn blank_line_preserved_between_leading_comment_and_type_alias() {
    assert_identity(concat!("// note\n", "\n", "type Foo = Bar\n"));
}

#[test]
fn blank_line_preserved_between_leading_comment_and_protocol() {
    assert_identity(concat!(
        "// note\n",
        "\n",
        "Protocol define: Startable\n",
        "  start -> Integer\n",
    ));
}

#[test]
fn blank_line_preserved_between_leading_comment_and_class() {
    assert_identity(concat!(
        "// note\n",
        "\n",
        "Object subclass: Foo\n",
        "\n",
        "  m => 1\n",
    ));
}

#[test]
fn no_blank_line_between_leading_comment_and_declaration_stays_absent() {
    // The converse of the three tests above, for all three declaration
    // kinds: no blank line in source between the comment and the
    // declaration must not gain one. This also guards against
    // `unparse_class_definition`'s pre-fix behaviour, which used to
    // unconditionally insert a blank line after any non-empty leading
    // comment regardless of what the source actually had.
    assert_identity("// note\ntype Foo = Bar\n");
    assert_identity(concat!(
        "// note\n",
        "Protocol define: Startable\n",
        "  start -> Integer\n",
    ));
    assert_identity(concat!(
        "// note\n",
        "Object subclass: Foo\n",
        "\n",
        "  m => 1\n",
    ));
}

#[test]
fn blank_line_before_comment_block_and_after_it_are_independent_signals() {
    // A blank line before the whole comment block (BT-2929's
    // `leading_blank_line`) and a blank line after the last comment,
    // before the declaration (BT-2945's `blank_line_after_comments`) are
    // tracked independently and must both round-trip when both are
    // present in the same source, between two top-level declarations
    // (blank-before-the-first-declaration-in-a-section is dropped by
    // design, so this needs a preceding declaration to attach to).
    assert_identity(concat!(
        "type Port = Integer\n",
        "\n",
        "// note\n",
        "\n",
        "type Timeout = Integer\n",
    ));
}

#[test]
fn state_declaration_preceded_by_orphaned_doc_block_does_not_merge_into_own_doc_comment() {
    // BT-2924 follow-up (found in adversarial review): a `///` block that
    // breaks away from a *different* declaration must not get glued onto
    // — and then silently merged into — a state field's own doc comment
    // on a format round-trip.
    let source = concat!(
        "Object subclass: Foo\n",
        "  /// Section header orphan.\n",
        "\n",
        "  /// The port to bind.\n",
        "  state: port :: Integer = 0\n",
    );
    let formatted = format_source(source).expect("format_source must succeed for valid source");
    let module = parse_source(&formatted);
    assert_eq!(module.classes.len(), 1);
    let field = &module.classes[0].state[0];
    assert_eq!(
        field.doc_comment.as_deref(),
        Some("The port to bind."),
        "the orphaned block above must not merge into the field's own doc comment; \
         formatted output:\n{formatted}"
    );
    assert!(
        formatted.contains("Section header orphan."),
        "the orphaned block's text must still be preserved somewhere:\n{formatted}"
    );
}

#[test]
fn protocol_preceded_by_orphaned_doc_block_does_not_merge_into_own_doc_comment() {
    // BT-2924 follow-up (found in adversarial review): same shape as
    // above, for a `Protocol define:` declaration.
    let source = concat!(
        "/// Orphaned block, meant for something else entirely.\n",
        "\n",
        "/// Things that can be printed.\n",
        "Protocol define: Displayable\n",
        "  asString -> String\n",
    );
    let formatted = format_source(source).expect("format_source must succeed for valid source");
    let module = parse_source(&formatted);
    assert_eq!(module.protocols.len(), 1);
    assert_eq!(
        module.protocols[0].doc_comment.as_deref(),
        Some("Things that can be printed."),
        "the orphaned block above must not merge into the protocol's own doc comment; \
         formatted output:\n{formatted}"
    );
    assert!(
        formatted.contains("Orphaned block, meant for something else entirely."),
        "the orphaned block's text must still be preserved somewhere:\n{formatted}"
    );
}

#[test]
fn method_with_no_doc_comment_preceded_by_orphaned_doc_block_does_not_attach_it() {
    // BT-2924 follow-up (review finding on the fix itself): a `///` block
    // that breaks away from a different declaration must not attach to a
    // *method with no doc comment of its own* on a format round-trip —
    // the class/protocol/state cases were fixed, but the method case was
    // missed since its blank-line guard lives inside `if let Some(doc)`.
    let source = concat!(
        "Object subclass: Foo\n",
        "  /// Section header (orphaned — blank line below).\n",
        "\n",
        "  doSomething => 1\n",
    );
    let formatted = format_source(source).expect("format_source must succeed for valid source");
    let module = parse_source(&formatted);
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(
        method.doc_comment, None,
        "the orphaned block above must not attach as the method's own doc comment; \
         formatted output:\n{formatted}"
    );
    assert!(
        formatted.contains("Section header (orphaned"),
        "the orphaned block's text must still be preserved somewhere:\n{formatted}"
    );
}

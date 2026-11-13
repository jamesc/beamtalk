// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0147 (BT-4735): editor support for map shapes — key completion after
//! `at: #` and inside map literals, hover on shape-typed values and keys,
//! go-to-definition on a shape alias, and key-level diagnostic spans.
//!
//! Sources mark the cursor with `‸`.

use super::common::*;

/// Splits `marked` at its `‸` into the source and the cursor position.
fn cursor(marked: &str) -> (String, Position) {
    let offset = marked.find('‸').expect("test source needs a `‸` cursor");
    let source = format!(
        "{}{}",
        &marked[..offset],
        &marked[offset + '‸'.len_utf8()..]
    );
    let offset = u32::try_from(offset).unwrap();
    let position = Position::from_byte_offset(&source, ByteOffset::new(offset)).unwrap();
    (source, position)
}

fn service_with(source: &str) -> (SimpleLanguageService, Utf8PathBuf) {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("shapes.bt");
    service.update_file(file.clone(), source.to_string());
    (service, file)
}

fn completions(marked: &str) -> Vec<Completion> {
    let (source, position) = cursor(marked);
    let (service, file) = service_with(&source);
    service.completions(&file, position)
}

fn labels(completions: &[Completion]) -> Vec<&str> {
    completions.iter().map(|c| c.label.as_str()).collect()
}

fn hover(marked: &str) -> String {
    let (source, position) = cursor(marked);
    let (service, file) = service_with(&source);
    service
        .hover(&file, position)
        .map(|h| h.contents.to_string())
        .unwrap_or_default()
}

const OPTIONS: &str = "type Options = #{#port => Integer, ?#host => String}\n";

// ── key completion after `at: #` ────────────────────────────────────────

#[test]
fn at_hash_on_an_inferred_literal_shape_offers_its_keys() {
    let got = completions("opts := #{#port => 80, #name => \"x\"}.\nopts at: #‸");
    assert_eq!(labels(&got), vec!["port", "name"], "{got:?}");
    assert_eq!(got[0].detail.as_deref(), Some("Integer"));
    assert_eq!(got[1].detail.as_deref(), Some("String"));
}

#[test]
fn at_hash_on_an_aliased_shape_marks_optional_keys() {
    let src = format!("{OPTIONS}o :: Options := #{{#port => 80}}.\no at: #‸");
    let got = completions(&src);
    assert_eq!(labels(&got), vec!["port", "host"], "{got:?}");
    assert_eq!(got[0].detail.as_deref(), Some("Integer"));
    assert_eq!(got[1].detail.as_deref(), Some("String (optional)"));
}

#[test]
fn partial_key_still_offers_every_key_for_the_client_to_filter() {
    let src = format!("{OPTIONS}o :: Options := #{{#port => 80}}.\no at: #po‸");
    assert_eq!(labels(&completions(&src)), vec!["port", "host"]);
}

#[test]
fn at_hash_before_if_absent_offers_keys() {
    let src = format!("{OPTIONS}o :: Options := #{{#port => 80}}.\no at: #‸ ifAbsent: [0]");
    assert_eq!(labels(&completions(&src)), vec!["port", "host"]);
}

#[test]
fn at_put_and_if_present_offer_keys_too() {
    for selector in ["at: #‸ put: 1", "at: #‸ ifPresent: [:v | v]"] {
        let src = format!("{OPTIONS}o :: Options := #{{#port => 80}}.\no {selector}");
        assert_eq!(
            labels(&completions(&src)),
            vec!["port", "host"],
            "{selector}"
        );
    }
}

#[test]
fn at_hash_inside_a_method_on_a_shape_parameter() {
    let src = format!("{OPTIONS}Object subclass: Server\n  run: o :: Options =>\n    o at: #‸\n");
    assert_eq!(labels(&completions(&src)), vec!["port", "host"]);
}

#[test]
fn at_hash_on_a_union_of_shapes_lists_each_key_once() {
    let src = "type First = #{#x => Integer, #y => Integer}\n\
               type Second = #{#x => String, #z => Integer}\n\
               Object subclass: P\n  run: o :: First | Second =>\n    o at: #‸\n";
    let got = completions(src);
    assert_eq!(labels(&got), vec!["x", "y", "z"], "{got:?}");
    assert_eq!(got[0].detail.as_deref(), Some("Integer | String"));
}

#[test]
fn at_hash_on_a_plain_dictionary_offers_no_shape_keys() {
    let got = completions("d := Dictionary new.\nd at: #‸");
    assert!(!labels(&got).contains(&"port"), "{got:?}");
}

#[test]
fn at_hash_on_a_non_keyed_selector_is_not_a_key_site() {
    let src = format!("{OPTIONS}o :: Options := #{{#port => 80}}.\no removeKey: #‸");
    assert!(!labels(&completions(&src)).contains(&"port"));
}

// ── key completion inside a map literal ─────────────────────────────────

#[test]
fn literal_assigned_to_an_annotated_local_offers_unwritten_keys() {
    let src = format!("{OPTIONS}o :: Options := #{{‸}}");
    assert_eq!(labels(&completions(&src)), vec!["port", "host"]);

    let src = format!("{OPTIONS}o :: Options := #{{#port => 80, ‸}}");
    let got = completions(&src);
    assert_eq!(labels(&got), vec!["host"], "{got:?}");
    assert_eq!(got[0].detail.as_deref(), Some("String (optional)"));
}

#[test]
fn literal_partial_key_and_following_pairs_still_complete() {
    let src = format!("{OPTIONS}o :: Options := #{{#port => 80, #ho‸, #zzz => 1}}");
    assert_eq!(labels(&completions(&src)), vec!["host"]);
}

#[test]
fn cursor_inside_an_existing_symbol_still_completes() {
    let src = format!("{OPTIONS}o :: Options := #{{#port => 80}}.\no at: #po‸rt");
    assert_eq!(labels(&completions(&src)), vec!["port", "host"]);
    let src = format!("{OPTIONS}o :: Options := #{{#ho‸st => \"h\"}}");
    assert_eq!(labels(&completions(&src)), vec!["port", "host"]);
}

#[test]
fn literal_key_before_an_existing_separator_keeps_later_pairs() {
    let src = format!("{OPTIONS}o :: Options := #{{#ho‸, #port => 80}}");
    assert_eq!(labels(&completions(&src)), vec!["host"]);
}

#[test]
fn literal_argument_offers_the_parameter_shape_keys() {
    let src = "type Opts = #{#timeout => Integer, ?#retries => Integer}\n\
               Object subclass: Client\n  \
               get: url :: String options: o :: Opts => url\n\
               c := Client new.\n\
               c get: 'u' options: #{#timeout => 5, ‸}";
    let got = completions(src);
    assert_eq!(labels(&got), vec!["retries"], "{got:?}");
}

#[test]
fn literal_state_default_offers_the_declared_shape_keys() {
    let src = format!("{OPTIONS}Object subclass: Server\n  state: o :: Options = #{{‸}}\n");
    assert_eq!(labels(&completions(&src)), vec!["port", "host"]);
}

#[test]
fn nested_literal_offers_the_inner_shape_keys() {
    let src = "type Inner = #{#a => Integer, #b => Integer}\n\
               type Outer = #{#inner => Inner, #n => Integer}\n\
               o :: Outer := #{#inner => #{#a => 1, ‸}}";
    assert_eq!(labels(&completions(src)), vec!["b"]);
}

#[test]
fn literal_with_no_expected_shape_gets_ordinary_completions() {
    let got = completions("x := #{#a => 1, ‸}");
    assert!(labels(&got).contains(&"self"), "{got:?}");
}

// ── hover ───────────────────────────────────────────────────────────────

const DECL: &str = "type Options = #{#port => Integer, ?#host => String}\n";

#[test]
fn hover_on_an_alias_typed_value_names_the_alias() {
    let got = hover(&format!("{DECL}o :: Options := #{{#port => 80}}.\n‸o"));
    assert!(got.contains("Type: Options"), "{got}");
}

#[test]
fn hover_on_an_unaliased_shape_shows_the_shape() {
    let got = hover("p := #{#a => 1, #b => \"s\"}.\n‸p");
    assert!(got.contains("#{#a => Integer, #b => String}"), "{got}");
}

#[test]
fn hover_on_a_long_unaliased_shape_is_truncated() {
    let got = hover(
        "p := #{#a => 1, #b => 1, #c => 1, #d => 1, #e => 1, #f => 1, #g => 1, #h => 1, #i => 1, #j => 1}.\n‸p",
    );
    assert!(got.contains("more"), "expected a truncation marker: {got}");
    assert!(!got.contains("#j"), "{got}");
}

#[test]
fn hover_on_a_key_in_a_keyed_read_shows_its_value_type() {
    let got = hover(&format!(
        "{DECL}o :: Options := #{{#port => 80}}.\no at: #po‸rt"
    ));
    assert!(
        got.contains("`#port`") && got.contains("Type: Integer"),
        "{got}"
    );
    assert!(got.contains("of `Options`"), "{got}");
    assert!(!got.contains("optional"), "{got}");
}

#[test]
fn hover_on_an_optional_key_says_it_may_be_absent() {
    let got = hover(&format!(
        "{DECL}o :: Options := #{{#port => 80}}.\no at: #ho‸st ifAbsent: [\"h\"]"
    ));
    assert!(got.contains("Type: String"), "{got}");
    assert!(got.contains("may be absent"), "{got}");
}

#[test]
fn hover_on_a_key_in_a_literal_shows_its_value_type() {
    let got = hover("p := #{#a => 1, #‸b => \"s\"}");
    assert!(
        got.contains("`#b`") && got.contains("Type: String"),
        "{got}"
    );
}

#[test]
fn hover_on_a_symbol_key_of_a_plain_dictionary_is_the_ordinary_symbol_hover() {
    let got = hover("d := Dictionary new.\nd at: #‸k");
    assert_eq!(got, "Symbol: `#k`");
}

// ── go to definition ────────────────────────────────────────────────────

#[test]
fn goto_definition_on_a_shape_alias_reaches_its_declaration() {
    let (service, file) = service_with(
        "type Options = #{#port => Integer}\nObject subclass: S\n  run: o :: Options => o\n",
    );
    // `  run: o :: Options` — `Options` starts at column 12.
    let def = service
        .goto_definition(&file, Position::new(2, 14))
        .expect("goto-def on a shape alias");
    assert_eq!(def.file, file);
    assert_eq!(def.span.start(), 5, "the alias name follows `type `");
}

#[test]
fn goto_definition_on_a_shape_alias_works_across_files() {
    let mut service = SimpleLanguageService::new();
    let decl = Utf8PathBuf::from("options.bt");
    let user = Utf8PathBuf::from("server.bt");
    service.update_file(
        decl.clone(),
        "type Options = #{#port => Integer}\n".to_string(),
    );
    service.update_file(
        user.clone(),
        "Object subclass: S\n  run: o :: Options => o\n".to_string(),
    );
    let def = service
        .goto_definition(&user, Position::new(1, 14))
        .expect("goto-def on a shape alias from another file");
    assert_eq!(def.file, decl);
}

// ── diagnostic spans ────────────────────────────────────────────────────

fn diagnostic_spans(source: &str) -> Vec<(String, String)> {
    let (service, file) = service_with(source);
    service
        .diagnostics(&file)
        .into_iter()
        .map(|d| {
            (
                d.message.to_string(),
                source[d.span.start() as usize..d.span.end() as usize].to_string(),
            )
        })
        .collect()
}

fn shape_warning_span(source: &str, needle: &str) -> String {
    diagnostic_spans(source)
        .into_iter()
        .find(|(message, _)| message.contains(needle))
        .unwrap_or_else(|| panic!("no diagnostic containing {needle:?}"))
        .1
}

#[test]
fn unknown_key_in_an_argument_literal_is_spanned_on_the_key() {
    let src = "type Options = #{#port => Integer}\nValue subclass: Srv\n  run: o :: Options => o\n  go => self run: #{#prot => 1}\n";
    assert_eq!(shape_warning_span(src, "Argument 1"), "#prot");
}

#[test]
fn wrong_value_type_in_an_argument_literal_is_spanned_on_the_key() {
    let src = "type Options = #{#port => Integer}\nValue subclass: Srv\n  run: o :: Options => o\n  go => self run: #{#port => \"x\"}\n";
    assert_eq!(shape_warning_span(src, "Argument 1"), "#port");
}

#[test]
fn nested_wrong_value_type_is_spanned_on_the_innermost_key() {
    let src = "type Inner = #{#a => Integer}\ntype Outer = #{#inner => Inner}\nValue subclass: Srv\n  run: o :: Outer => o\n  go => self run: #{#inner => #{#a => \"x\"}}\n";
    assert_eq!(shape_warning_span(src, "Argument 1"), "#a");
}

#[test]
fn missing_required_key_keeps_the_whole_literal_span() {
    // Nothing is written for a missing key, so there is no key to point at.
    let src = "type Options = #{#port => Integer, #host => String}\nValue subclass: Srv\n  run: o :: Options => o\n  go => self run: #{#port => 1}\n";
    assert_eq!(
        shape_warning_span(src, "Argument 1"),
        "self run: #{#port => 1}"
    );
}

#[test]
fn annotated_local_literal_is_spanned_on_the_offending_key() {
    let src = "type Options = #{#port => Integer}\no :: Options := #{#prot => 1}\n";
    assert_eq!(shape_warning_span(src, "Type mismatch"), "#prot");
    let src = "type Options = #{#port => Integer}\no :: Options := #{#port => \"x\"}\n";
    assert_eq!(shape_warning_span(src, "Type mismatch"), "#port");
}

#[test]
fn nullable_shape_annotation_still_spans_the_key() {
    let src = "type Options = #{#port => Integer}\no :: Options | Nil := #{#port => \"x\"}\n";
    assert_eq!(shape_warning_span(src, "Type mismatch"), "#port");
}

#[test]
fn nil_first_union_annotation_spans_the_key_like_nil_last() {
    // BT-4789: the shape member's error wins whatever the member order.
    let src = "type Options = #{#port => Integer}\no :: Nil | Options := #{#port => \"x\"}\n";
    assert_eq!(shape_warning_span(src, "Type mismatch"), "#port");
    let src = "type Options = #{#port => Integer}\no :: Nil | Options := #{#prot => 1}\n";
    assert_eq!(shape_warning_span(src, "Type mismatch"), "#prot");
    let src = "type Options = #{#port => Integer}\no :: Options | Nil := #{#prot => 1}\n";
    assert_eq!(shape_warning_span(src, "Type mismatch"), "#prot");
}

#[test]
fn parenthesised_literal_is_spanned_on_the_offending_key() {
    let src = "type Options = #{#port => Integer}\no :: Options := (#{#prot => 1})\n";
    assert_eq!(shape_warning_span(src, "Type mismatch"), "#prot");
    let src = "type Options = #{#port => Integer}\no :: Nil | Options := ((#{#port => \"x\"}))\n";
    assert_eq!(shape_warning_span(src, "Type mismatch"), "#port");
}

#[test]
fn state_default_literal_is_spanned_on_the_offending_key() {
    let src = "type Options = #{#port => Integer}\nActor subclass: Srv\n  state: o :: Options = #{#prot => 1}\n";
    assert_eq!(shape_warning_span(src, "state `o`"), "#prot");
}

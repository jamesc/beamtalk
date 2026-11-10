// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Position features and send queries on a selector written in a `|>`
//! pipeline stage (ADR 0145 § 2, consequence 1; BT-4518).
//!
//! The parser desugars `a m1 |> m2` into the nested send `(a m1) m2`, with
//! the stage's selector span where it is written. So hover, go-to-definition,
//! signature help, senders and references see an ordinary send and need no
//! pipeline-specific code. These tests pin that.

use super::common::*;

const DEFS: &str = "Value subclass: Foo\n  \
    /// Answer the receiver.\n  \
    bar -> Foo => self\n  \
    /// Answer n.\n  \
    baz: n :: Integer -> Integer => n\n";

// Line 1: single-message stages. `bar` (stage) at 14..17, `baz:` at 21..25.
// Line 2: one multi-message stage `bar bar baz: 2`; second `bar` at 14..17,
//         `baz:` at 18..22.
const CALLS: &str = "x := Foo new\n\
    y := x bar |> bar |> baz: 1\n\
    z := x |> bar bar baz: 2\n";

fn service() -> (SimpleLanguageService, Utf8PathBuf, Utf8PathBuf) {
    let mut service = SimpleLanguageService::new();
    let defs = Utf8PathBuf::from("foo.bt");
    let calls = Utf8PathBuf::from("calls.bt");
    service.update_file(defs.clone(), DEFS.to_string());
    service.update_file(calls.clone(), CALLS.to_string());
    (service, defs, calls)
}

#[test]
fn calls_fixture_parses_without_diagnostics() {
    let (service, _, calls) = service();
    let errors: Vec<_> = service
        .diagnostics(&calls)
        .into_iter()
        .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .collect();
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn hover_on_selector_in_pipeline_stage() {
    let (service, _, calls) = service();
    for position in [Position::new(1, 15), Position::new(2, 15)] {
        let hover = service
            .hover(&calls, position)
            .unwrap_or_else(|| panic!("hover at {position:?}"));
        assert!(
            hover.contents.contains("bar"),
            "{position:?}: {}",
            hover.contents
        );
    }
    let hover = service
        .hover(&calls, Position::new(2, 19))
        .expect("hover on keyword in a multi-message stage");
    assert!(hover.contents.contains("baz:"), "{}", hover.contents);
}

#[test]
fn goto_definition_on_selector_in_pipeline_stage() {
    let (service, defs, calls) = service();
    for position in [
        Position::new(1, 15),
        Position::new(1, 22),
        Position::new(2, 11),
        Position::new(2, 15),
        Position::new(2, 19),
    ] {
        let def = service
            .goto_definition(&calls, position)
            .unwrap_or_else(|| panic!("definition at {position:?}"));
        assert_eq!(def.file, defs, "{position:?}");
    }
}

#[test]
fn signature_help_on_keyword_stage() {
    let (service, _, calls) = service();
    // Inside the argument of a single-message keyword stage, and of the
    // keyword tail of a multi-message stage.
    for position in [Position::new(1, 26), Position::new(2, 23)] {
        let help = service
            .signature_help(&calls, position)
            .unwrap_or_else(|| panic!("signature help at {position:?}"));
        assert!(
            help.signatures[0].label.starts_with("baz:"),
            "{position:?}: {}",
            help.signatures[0].label
        );
    }
}

#[test]
fn senders_find_sends_written_in_pipeline_stages() {
    let (service, _, calls) = service();
    let bar_sites: Vec<_> = service
        .find_selector_send_sites_across_files("bar")
        .into_iter()
        .filter(|l| l.file == calls)
        .collect();
    // `x bar`, the `|> bar` stage, and both sends of the multi-message stage.
    assert_eq!(bar_sites.len(), 4, "{bar_sites:?}");
    let baz_sites: Vec<_> = service
        .find_selector_send_sites_across_files("baz:")
        .into_iter()
        .filter(|l| l.file == calls)
        .collect();
    assert_eq!(baz_sites.len(), 2, "{baz_sites:?}");
}

#[test]
fn references_from_a_pipeline_stage_find_every_send_and_the_definition() {
    let (service, defs, calls) = service();
    assert_eq!(
        service.references_query_at(&calls, Position::new(1, 15)),
        Some(NavQuery::SendersOf("bar".into()))
    );
    let refs = service.find_references(&calls, Position::new(1, 15));
    assert!(refs.iter().any(|r| r.file == defs), "{refs:?}");
    assert_eq!(
        refs.iter().filter(|r| r.file == calls).count(),
        4,
        "{refs:?}"
    );
}

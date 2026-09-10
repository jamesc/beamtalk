// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Keeps `parse_divider_name`
//! ([`beamtalk_core::source_analysis::parse_divider_name`]) and the
//! `TextMate` `comment.line.double-slash.section-divider.beamtalk` regex in
//! `editors/vscode/syntaxes/beamtalk.tmLanguage.json` from silently drifting
//! apart.
//!
//! Both sides read the *same* fixture,
//! `tests/fixtures/section_divider_grammar_cases.json`:
//! this test calls `parse_divider_name` directly against each case's
//! `content`; `editors/vscode/src/__tests__/sectionDividerGrammar.test.ts`
//! runs the regex — parsed straight out of the `.tmLanguage.json` file, never
//! a hand-copied duplicate — through the real Oniguruma engine
//! (`vscode-oniguruma`, the same WASM binding VS Code itself uses) against
//! `"// " + content`. A change to either recognizer's behavior that isn't
//! reflected in the fixture (or a fixture case one side doesn't actually
//! reproduce) fails a test, on purpose — see that module's own doc for the
//! reverse direction.
//!
//! This fixture guards two real divergences between the two recognizers.
//! The `===Name===`-with-no-space cases guard a specific asymmetry:
//! `parse_divider_name` never requires whitespace around the name (only
//! that the trimmed name be non-empty); the regex's whitespace is likewise
//! optional rather than required, so a valid whitespace-free divider is not
//! silently under-highlighted, while the mismatched-run-length and
//! whitespace-only-name traps that a naive `\s+` → `\s*` substitution would
//! reopen stay rejected — see the `.tmLanguage.json` comment for the
//! two-sided implementation.

use beamtalk_core::source_analysis::parse_divider_name;

#[derive(serde::Deserialize)]
struct Case {
    content: String,
    name: Option<String>,
}

#[derive(serde::Deserialize)]
struct Fixture {
    cases: Vec<Case>,
}

#[test]
fn parse_divider_name_matches_the_shared_fixture() {
    let raw = include_str!("fixtures/section_divider_grammar_cases.json");
    let fixture: Fixture =
        serde_json::from_str(raw).expect("fixture must be valid JSON matching the Fixture shape");
    assert!(
        !fixture.cases.is_empty(),
        "fixture should not be empty — an empty fixture would make this test vacuous"
    );

    let mut failures = Vec::new();
    for case in &fixture.cases {
        let actual = parse_divider_name(&case.content);
        let expected = case.name.as_deref();
        if actual != expected {
            failures.push(format!(
                "content {:?}: parse_divider_name returned {:?}, fixture expects {:?}",
                case.content, actual, expected
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "parse_divider_name disagrees with the shared fixture \
         (tests/fixtures/section_divider_grammar_cases.json) — also consumed by \
         editors/vscode/src/__tests__/sectionDividerGrammar.test.ts against the real \
         TextMate grammar, so a genuine behavior change here needs the fixture (and \
         probably the .tmLanguage.json regex) updated too, not just this test:\n{}",
        failures.join("\n")
    );
}

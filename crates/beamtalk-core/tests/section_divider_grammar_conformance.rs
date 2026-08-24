// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3261: keeps `parse_divider_name`
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
//! BT-3237 review found (and fixed) two real divergences between the two
//! recognizers; this fixture is what keeps them fixed. The
//! `===Name===`-with-no-space cases are BT-3261 itself: `parse_divider_name`
//! never required whitespace around the name (only that the trimmed name be
//! non-empty), but the regex did, silently under-highlighting a valid
//! divider — closed by making the regex's whitespace optional (defended
//! against the mismatched-run-length and whitespace-only-name traps that a
//! naive `\s+` → `\s*` substitution reopens; see the `.tmLanguage.json`
//! comment for the two-sided fix).

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

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// BT-3261: keeps the TextMate `comment.line.double-slash.section-divider.beamtalk`
// regex in `../../syntaxes/beamtalk.tmLanguage.json` from silently drifting away
// from `parse_divider_name`
// (`crates/beamtalk-core/src/source_analysis/method_category.rs`), the real
// divider-recognition rule it approximates.
//
// This test and `crates/beamtalk-core/tests/section_divider_grammar_conformance.rs`
// read the *same* shared fixture,
// `crates/beamtalk-core/tests/fixtures/section_divider_grammar_cases.json` — that
// Rust test calls `parse_divider_name` directly on each case's `content`; this one
// builds `"// " + content` and runs it through the real Oniguruma engine
// (`vscode-oniguruma`, the same WASM binding VS Code itself uses at runtime)
// against the regex parsed straight out of the `.tmLanguage.json` file — never a
// hand-copied duplicate of it — so a change to either side that the fixture
// doesn't already cover, or a fixture case one side doesn't actually reproduce,
// fails a test instead of silently drifting. See that Rust test's module doc for
// the fuller history (BT-3237, BT-3261).

import { readFileSync } from "node:fs";
import * as path from "node:path";
import * as oniguruma from "vscode-oniguruma";
import { beforeAll, describe, expect, it } from "vitest";

const GRAMMAR_PATH = path.resolve(__dirname, "../../syntaxes/beamtalk.tmLanguage.json");
const FIXTURE_PATH = path.resolve(
  __dirname,
  "../../../../crates/beamtalk-core/tests/fixtures/section_divider_grammar_cases.json"
);
const DIVIDER_SCOPE = "comment.line.double-slash.section-divider.beamtalk";

interface FixtureCase {
  content: string;
  name: string | null;
}

interface TmCapture {
  name: string;
}

interface TmPattern {
  name?: string;
  match?: string;
  captures?: Record<string, TmCapture>;
}

function loadDividerPattern(): TmPattern {
  const grammar = JSON.parse(readFileSync(GRAMMAR_PATH, "utf8")) as {
    repository: { comments: { patterns: TmPattern[] } };
  };
  const pattern = grammar.repository.comments.patterns.find((p) => p.name === DIVIDER_SCOPE);
  if (!pattern?.match || !pattern.captures) {
    throw new Error(
      `expected a "${DIVIDER_SCOPE}" pattern with "match" and "captures" in ${GRAMMAR_PATH}`
    );
  }
  return pattern;
}

function loadFixtureCases(): FixtureCase[] {
  const fixture = JSON.parse(readFileSync(FIXTURE_PATH, "utf8")) as { cases: FixtureCase[] };
  return fixture.cases;
}

/** The capture group index whose scope names the divider's heading text —
 * looked up by scope rather than hardcoded, so a future renumbering of the
 * regex's capture groups can't silently point this test at the wrong group. */
function nameCaptureIndex(pattern: TmPattern): number {
  const entry = Object.entries(pattern.captures ?? {}).find(([, capture]) =>
    capture.name.includes("entity.name.section")
  );
  if (!entry) {
    throw new Error(`expected a capture with an "entity.name.section" scope in ${GRAMMAR_PATH}`);
  }
  return Number(entry[0]);
}

describe("section-divider TextMate grammar vs. parse_divider_name (BT-3261)", () => {
  let scanner: oniguruma.OnigScanner;
  let captureIndex: number;

  beforeAll(async () => {
    const wasmPath = path.resolve(
      __dirname,
      "../../node_modules/vscode-oniguruma/release/onig.wasm"
    );
    await oniguruma.loadWASM(readFileSync(wasmPath).buffer);
    const pattern = loadDividerPattern();
    captureIndex = nameCaptureIndex(pattern);
    // biome-ignore lint/style/noNonNullAssertion: loadDividerPattern already asserted match is set
    scanner = new oniguruma.OnigScanner([pattern.match!]);
  });

  const cases = loadFixtureCases();
  it("fixture is non-empty (a guard against a vacuous test)", () => {
    expect(cases.length).toBeGreaterThan(0);
  });

  for (const { content, name } of cases) {
    const line = `// ${content}`;
    it(`${JSON.stringify(line)} -> ${JSON.stringify(name)}`, () => {
      const result = scanner.findNextMatchSync(line, 0);
      const capture = result?.captureIndices[captureIndex];
      const matchedName = capture ? line.slice(capture.start, capture.end) : null;
      expect(matchedName).toBe(name);
    });
  }
});

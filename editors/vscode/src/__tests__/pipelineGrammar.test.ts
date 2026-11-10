// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// BT-4517 (ADR 0145): the TextMate grammar highlights the pipeline operator
// `|>` as one operator token, and only when `|` and `>` are adjacent, as the
// lexer's `PipeGt`. The `operators` patterns are parsed straight out of
// `../../syntaxes/beamtalk.tmLanguage.json` and run through the real Oniguruma
// engine (`vscode-oniguruma`) in the grammar's order, as VS Code would.

import { readFileSync } from "node:fs";
import * as path from "node:path";
import { beforeAll, describe, expect, it } from "vitest";
import * as oniguruma from "vscode-oniguruma";

const GRAMMAR_PATH = path.resolve(__dirname, "../../syntaxes/beamtalk.tmLanguage.json");
const PIPELINE_SCOPE = "keyword.operator.pipeline.beamtalk";

interface TmPattern {
  name?: string;
  match?: string;
}

function loadOperatorPatterns(): TmPattern[] {
  const grammar = JSON.parse(readFileSync(GRAMMAR_PATH, "utf8")) as {
    repository: { operators: { patterns: TmPattern[] } };
  };
  return grammar.repository.operators.patterns.filter((p) => p.match !== undefined);
}

describe("pipeline operator TextMate grammar (BT-4517)", () => {
  let scanner: oniguruma.OnigScanner;
  let patterns: TmPattern[];

  beforeAll(async () => {
    const wasmPath = path.resolve(
      __dirname,
      "../../node_modules/vscode-oniguruma/release/onig.wasm"
    );
    await oniguruma.loadWASM(readFileSync(wasmPath).buffer);
    patterns = loadOperatorPatterns();
    scanner = new oniguruma.OnigScanner(patterns.map((p) => p.match as string));
  });

  /** The scope and text of the first operator token at or after `from`. */
  function firstOperator(line: string, from: number): { scope?: string; text: string } | null {
    const result = scanner.findNextMatchSync(line, from);
    if (!result) {
      return null;
    }
    const whole = result.captureIndices[0];
    return {
      scope: patterns[result.index].name,
      text: line.slice(whole.start, whole.end),
    };
  }

  it("scopes `|>` as one pipeline operator token", () => {
    expect(firstOperator("x foo |> bar", 0)).toEqual({ scope: PIPELINE_SCOPE, text: "|>" });
  });

  it("does not scope `| >` (not adjacent) as a pipeline", () => {
    expect(firstOperator("x | > y", 0)?.scope).not.toBe(PIPELINE_SCOPE);
  });
});

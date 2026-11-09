// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Tokenizes Beamtalk source with the same TextMate grammar and Oniguruma
// engine VS Code uses, and reduces each token's scope stack to one of a small
// set of highlight categories. Shared by the extension's service worker and
// the Node tests.

import oniguruma from "vscode-oniguruma";
import vsctm from "vscode-textmate";

/**
 * Scope prefix -> highlight category. Order matters: the first prefix that
 * matches the innermost mappable scope wins. Categories correspond to GitHub's
 * own prettylights syntax colours (see content.css).
 */
const SCOPE_CATEGORIES = [
  ["entity.name.section", "heading"],
  ["punctuation.section.divider", "comment"],
  ["comment", "comment"],
  ["constant.character.escape", "keyword"],
  ["punctuation.section.interpolation", "keyword"],
  ["string", "string"],
  ["constant.character", "string"],
  ["constant", "constant"],
  ["variable.language", "constant"],
  ["entity.name.type", "type"],
  ["entity.other.inherited-class", "type"],
  ["entity.name.function", "entity"],
  ["keyword", "keyword"],
  ["storage", "keyword"],
];

/** @param {string} scope */
function categoryOfScope(scope) {
  for (const [prefix, category] of SCOPE_CATEGORIES) {
    if (scope === prefix || scope.startsWith(`${prefix}.`)) return category;
  }
  return null;
}

/** @param {string[]} scopes outermost first */
function categoryOf(scopes) {
  for (let i = scopes.length - 1; i >= 0; i--) {
    const category = categoryOfScope(scopes[i]);
    if (category) return category;
  }
  return null;
}

/**
 * @param {{ wasm: ArrayBuffer | Uint8Array, grammar: object }} options
 * @returns {Promise<{ tokenizeLines: (lines: string[], continuous: boolean) => Array<Array<[number, number, string]>> }>}
 */
export async function createTokenizer({ wasm, grammar }) {
  await oniguruma.loadWASM(wasm);
  const registry = new vsctm.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (patterns) => new oniguruma.OnigScanner(patterns),
      createOnigString: (s) => new oniguruma.OnigString(s),
    }),
    loadGrammar: async (scopeName) => (scopeName === grammar.scopeName ? grammar : null),
  });
  const loaded = await registry.loadGrammar(grammar.scopeName);
  if (!loaded) throw new Error(`grammar ${grammar.scopeName} failed to load`);

  /**
   * Returns, per line, `[start, end, category]` triples (UTF-16 offsets,
   * adjacent same-category tokens merged, uncategorised text omitted).
   * `continuous` carries tokenizer state across lines (a whole file or code
   * block); otherwise each line starts fresh (diff lines, which interleave
   * old and new text).
   */
  function tokenizeLines(lines, continuous) {
    let state = vsctm.INITIAL;
    return lines.map((line) => {
      const result = loaded.tokenizeLine(line, continuous ? state : vsctm.INITIAL);
      state = result.ruleStack;
      /** @type {Array<[number, number, string]>} */
      const out = [];
      for (const token of result.tokens) {
        const end = Math.min(token.endIndex, line.length);
        if (end <= token.startIndex) continue;
        const category = categoryOf(token.scopes);
        if (!category) continue;
        const last = out[out.length - 1];
        if (last && last[1] === token.startIndex && last[2] === category) last[1] = end;
        else out.push([token.startIndex, end, category]);
      }
      return out;
    });
  }

  return { tokenizeLines };
}

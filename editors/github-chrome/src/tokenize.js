// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Tokenizes Beamtalk source with the same TextMate grammar and Oniguruma
// engine VS Code uses, and reduces each token's scope stack to one of a small
// set of highlight categories. Shared by the extension's service worker and
// the Node tests.

import oniguruma from "vscode-oniguruma";
import vsctm from "vscode-textmate";
import { LIMITS } from "./protocol.js";

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
 * @typedef {Array<Array<[number, number, string]>>} BlockTokens
 * @param {{ wasm: ArrayBuffer | Uint8Array, grammar: object, now?: () => number }} options
 *   `now` is the clock for the request budget (tests inject one).
 * @returns {Promise<{
 *   tokenizeLines: (lines: string[], continuous: boolean) => BlockTokens,
 *   tokenizeBlocks: (blocks: Array<{ lines: string[], continuous: boolean }>) =>
 *     { blocks: BlockTokens[], incomplete: number[] },
 * }>}
 */
export async function createTokenizer({ wasm, grammar, now = Date.now }) {
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
   *
   * Bounded (BT-5194), since the text is whatever page is open and one
   * service worker tokenizes for every tab: a line longer than
   * `LIMITS.maxLineLength`, and every line once `deadline` has passed, gets
   * no tokens; each line gets `LIMITS.lineTimeLimitMs`, after which the rest
   * of it is left uncoloured and the next line starts from a fresh state
   * (the stopped line's state is mid-line).
   */
  function tokenizeLines(lines, continuous) {
    return tokenizeWithin(lines, continuous, now() + LIMITS.requestBudgetMs).tokens;
  }

  /**
   * {@link tokenizeLines} against a given `deadline`; `cut` says whether the
   * deadline left any line untokenized (worth retrying later, unlike a line
   * that is too long or out of its own time, which would be cut again).
   */
  function tokenizeWithin(lines, continuous, deadline) {
    let state = vsctm.INITIAL;
    let cut = false;
    const tokens = lines.map((line) => {
      if (line.length > LIMITS.maxLineLength) {
        state = vsctm.INITIAL;
        return [];
      }
      if (now() > deadline) {
        cut = true;
        state = vsctm.INITIAL;
        return [];
      }
      const result = loaded.tokenizeLine(
        line,
        continuous ? state : vsctm.INITIAL,
        LIMITS.lineTimeLimitMs
      );
      state = result.stoppedEarly ? vsctm.INITIAL : result.ruleStack;
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
    return { tokens, cut };
  }

  /**
   * Tokenizes a whole request's blocks within one `LIMITS.requestBudgetMs`.
   * `incomplete` lists the blocks the budget cut short, which the content
   * script does not cache, so a later scan asks for them again.
   */
  function tokenizeBlocks(blocks) {
    const deadline = now() + LIMITS.requestBudgetMs;
    const incomplete = [];
    const tokens = blocks.map((b, i) => {
      const result = tokenizeWithin(b.lines, b.continuous, deadline);
      if (result.cut) incomplete.push(i);
      return result.tokens;
    });
    return { blocks: tokens, incomplete };
  }

  return { tokenizeLines, tokenizeBlocks };
}

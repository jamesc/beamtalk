// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { test } from "node:test";
import { createTokenizer } from "../src/tokenize.js";

const require = createRequire(import.meta.url);
const tokenizer = await createTokenizer({
  wasm: readFileSync(require.resolve("vscode-oniguruma/release/onig.wasm")),
  grammar: JSON.parse(
    readFileSync(new URL("../../vscode/syntaxes/beamtalk.tmLanguage.json", import.meta.url))
  ),
});

/** Returns `[text, category]` pairs for one line. */
function categorised(line, continuous = true) {
  const [tokens] = tokenizer.tokenizeLines([line], continuous);
  return tokens.map(([start, end, category]) => [line.slice(start, end), category]);
}

test("comments, strings, symbols and numbers", () => {
  const pairs = categorised('x := #foo -> "hi" + 42 // note');
  assert.deepEqual(
    pairs.filter(([, c]) => c !== "keyword"),
    [
      ["#foo", "constant"],
      ['"hi"', "string"],
      ["42", "constant"],
      ["// note", "comment"],
    ]
  );
});

test("class definition names the class and superclass as types", () => {
  const pairs = categorised("Object subclass: Counter");
  assert.ok(pairs.some(([t, c]) => t === "Counter" && c === "type"), JSON.stringify(pairs));
  assert.ok(pairs.some(([t, c]) => t === "Object" && c === "type"), JSON.stringify(pairs));
});

test("self and nil are constants", () => {
  const pairs = categorised("self foo ifNil: [nil]");
  assert.ok(pairs.some(([t, c]) => t === "self" && c === "constant"));
  assert.ok(pairs.some(([t, c]) => t === "nil" && c === "constant"));
});

test("continuous mode carries block comments across lines", () => {
  const lines = ["/* start", "still comment */ x"];
  const [, second] = tokenizer.tokenizeLines(lines, true);
  assert.deepEqual(second[0], [0, "still comment */".length, "comment"]);
  const [, fresh] = tokenizer.tokenizeLines(lines, false);
  assert.notDeepEqual(fresh[0], second[0]);
});

test("offsets are UTF-16 indices", () => {
  const line = '"héllo 🎉" foo';
  const [tokens] = tokenizer.tokenizeLines([line], true);
  assert.equal(line.slice(tokens[0][0], tokens[0][1]), '"héllo 🎉"');
});

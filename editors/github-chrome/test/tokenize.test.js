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

for (const keyword of ["state:", "field:", "classState:"]) {
  test(`${keyword} declarations are keywords, not selector keywords`, () => {
    const pairs = categorised(`  ${keyword} count :: Integer = 0`);
    assert.deepEqual(pairs[0], [keyword, "keyword"]);
    assert.ok(pairs.some(([t, c]) => t === "Integer" && c === "type"), JSON.stringify(pairs));
  });
}

test("late slot declarations", () => {
  const pairs = categorised("  late state: engine :: WorkflowEngine");
  assert.deepEqual(pairs.slice(0, 2), [
    ["late", "keyword"],
    ["state:", "keyword"],
  ]);
  assert.deepEqual(categorised("  late := 1")[0], [":=", "keyword"]);
});

test("class header modifiers and native: module", () => {
  const pairs = categorised("abstract sealed typed Value subclass: Uuid native: beamtalk_uuid");
  for (const word of ["abstract", "sealed", "typed", "subclass:", "native:"]) {
    assert.ok(pairs.some(([t, c]) => t.trim() === word && c === "keyword"), `${word}: ${JSON.stringify(pairs)}`);
  }
  assert.ok(pairs.some(([t, c]) => t === "Uuid" && c === "type"));
  assert.deepEqual(categorised("declare native: lists"), [
    ["declare", "keyword"],
    ["native:", "keyword"],
  ]);
});

test("method modifiers, with a unary method name", () => {
  const keywordMethod = categorised("  class internal sealed attachMany: xs => xs");
  for (const word of ["class", "internal", "sealed"]) {
    assert.ok(keywordMethod.some(([t, c]) => t.includes(word) && c === "keyword"), word);
  }
  assert.ok(keywordMethod.some(([t, c]) => t === "attachMany:" && c === "entity"));
  const unary = categorised("  class sealed empty -> Binary => Binary new");
  assert.ok(unary.some(([t, c]) => t === "empty" && c === "entity"), JSON.stringify(unary));
});

test("modifier words are not keywords mid-line or as assignment targets", () => {
  assert.deepEqual(categorised("  class := x"), [[":=", "keyword"]]);
  assert.ok(!categorised("  x class sealed").some(([, c]) => c === "keyword"));
});

test("type aliases and protocol definitions", () => {
  const alias = categorised("internal type RestartStrategy = #permanent | #transient");
  assert.deepEqual(alias.slice(0, 3).map(([t, c]) => [t.trim(), c]), [
    ["internal", "keyword"],
    ["type", "keyword"],
    ["RestartStrategy", "type"],
  ]);
  const protocol = categorised("Protocol define: Comparable");
  assert.ok(protocol.some(([t, c]) => t === "define:" && c === "keyword"), JSON.stringify(protocol));
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

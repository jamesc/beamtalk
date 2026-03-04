// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { describe, expect, it } from "vitest";
import {
  findMethodDeclaration,
  findStateVarDeclaration,
  extractStateVarInfo,
  extractMethodDocComment,
} from "../textUtils";

// ─── Fixtures ────────────────────────────────────────────────────────────────

// Mirrors examples/sicp/src/main.bt — has both a class-side and instance-side `run`.
const MAIN_BT = `\
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0


Object subclass: Main

  class run => self new run

  run =>
    reader := SchemeReader new
    ev := SchemeEval new
    printer := SchemePrinter new
    env := ev defaultEnv

    eval := [:src | printer print: (ev eval: (reader read: src) in: env)]
    (WorkspaceInterface current) bind: eval as: #eval

    self traceCr: "Run: eval value: \\"(* 6 7)\\"."
    self
`;

// Mirrors examples/sicp/src/scheme/env.bt — keyword selectors, doc comments, Actor subclass.
const ENV_BT = `\
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/// SchemeEnv — mutable lexical environment.
///
/// ## Examples
/// \`\`\`beamtalk
/// env lookup: "x"    // => 42 (if x was defined)
/// \`\`\`
Actor subclass: SchemeEnv
  state: bindings = #{}
  state: parent = nil

  /// Look up \`name\` in this frame, then walk parent frames until found.
  lookup: name => self lookup: name depth: 0

  lookup: name depth: depth =>
    (depth > 1000) ifTrue: [
      ^self error: "Environment chain exceeded maximum depth"
    ]
    (self.bindings includesKey: name) ifTrue: [^self.bindings at: name]
    self.parent notNil ifTrue: [^(self.parent lookup: name depth: depth + 1)]
    self error: "Unbound variable: " ++ name

  define: name value: val =>
    self.bindings := self.bindings at: name put: val
    nil

  setParent: p =>
    (p =:= self) ifTrue: [
      ^self error: "Cycle detected"
    ]
    self.parent := p
`;

// ─── Helpers ─────────────────────────────────────────────────────────────────

function lineOf(text: string, offset: number): number {
  return text.slice(0, offset).split("\n").length - 1;
}

// ─── main.bt tests ───────────────────────────────────────────────────────────

describe("findMethodDeclaration — main.bt", () => {
  it("finds the class-side run declaration, not the instance call site", () => {
    const offset = findMethodDeclaration(MAIN_BT, "run", "class");
    expect(offset).not.toBe(-1);
    // Should be on the `class run =>` line, not `self new run` call site
    const line = lineOf(MAIN_BT, offset);
    expect(MAIN_BT.split("\n")[line]).toContain("class run =>");
  });

  it("finds the instance-side run declaration, not the class-side one", () => {
    const offset = findMethodDeclaration(MAIN_BT, "run", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(MAIN_BT, offset);
    const lineText = MAIN_BT.split("\n")[line];
    expect(lineText).toMatch(/^\s*run\s*=>/);
    expect(lineText).not.toContain("class run");
  });

  it("class-side and instance-side run resolve to different lines", () => {
    const classOffset = findMethodDeclaration(MAIN_BT, "run", "class");
    const instanceOffset = findMethodDeclaration(MAIN_BT, "run", "instance");
    expect(lineOf(MAIN_BT, classOffset)).not.toBe(lineOf(MAIN_BT, instanceOffset));
  });

  it("returns -1 for a selector that does not exist", () => {
    expect(findMethodDeclaration(MAIN_BT, "nonexistent", "instance")).toBe(-1);
    expect(findMethodDeclaration(MAIN_BT, "nonexistent", "class")).toBe(-1);
  });
});

// ─── env.bt tests ─────────────────────────────────────────────────────────────

describe("findMethodDeclaration — env.bt", () => {
  it("finds lookup: keyword selector (instance)", () => {
    const offset = findMethodDeclaration(ENV_BT, "lookup:", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(ENV_BT, offset);
    expect(ENV_BT.split("\n")[line]).toMatch(/^\s*lookup:\s/);
  });

  it("finds lookup:depth: keyword selector (instance)", () => {
    const offset = findMethodDeclaration(ENV_BT, "lookup:depth:", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(ENV_BT, offset);
    expect(ENV_BT.split("\n")[line]).toMatch(/^\s*lookup:.*depth:/);
  });

  it("finds define:value: keyword selector (instance)", () => {
    const offset = findMethodDeclaration(ENV_BT, "define:value:", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(ENV_BT, offset);
    expect(ENV_BT.split("\n")[line]).toMatch(/^\s*define:.*value:/);
  });

  it("does not match lookup: appearing in doc comments", () => {
    // The doc comment contains `env lookup: \"x\"    // => 42`
    const offset = findMethodDeclaration(ENV_BT, "lookup:", "instance");
    const line = lineOf(ENV_BT, offset);
    // Must be the declaration line, not the comment inside the doc block
    expect(ENV_BT.split("\n")[line].trimStart()).not.toMatch(/^\/\//);
    expect(ENV_BT.split("\n")[line].trimStart()).not.toMatch(/^\/\/\//);
  });

  it("does not match lookup: appearing inside method bodies (call sites)", () => {
    const offset = findMethodDeclaration(ENV_BT, "lookup:", "instance");
    const line = lineOf(ENV_BT, offset);
    // The declaration line should be `lookup: name =>`, not `self.parent lookup: name depth: ...`
    const lineText = ENV_BT.split("\n")[line];
    expect(lineText).toMatch(/^\s*lookup:\s+name\s*=>/);
  });

  it("returns -1 for a class-side selector that does not exist in env.bt", () => {
    expect(findMethodDeclaration(ENV_BT, "lookup:", "class")).toBe(-1);
  });
});

// ─── lambda.bt tests ─────────────────────────────────────────────────────────
// Mirrors examples/sicp/src/scheme/lambda.bt — class-side keyword selector
// with 3 parts: `withParams:body:env:`.

const LAMBDA_BT = `\
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/// SchemeLambda — a user-defined Scheme procedure (closure).
///
/// ## Examples
/// \`\`\`beamtalk
/// lam := SchemeLambda withParams: #("x") body: bodyExpr env: env
/// \`\`\`
Value subclass: SchemeLambda
  state: params = #()
  state: body = nil
  state: closureEnv = nil

  class withParams: p body: b env: e =>
    SchemeLambda new: #{#params => p, #body => b, #closureEnv => e}

  params => self.params
  body => self.body
  closureEnv => self.closureEnv
`;

describe("findMethodDeclaration — lambda.bt", () => {
  it("finds class-side withParams:body:env: keyword selector", () => {
    const offset = findMethodDeclaration(LAMBDA_BT, "withParams:body:env:", "class");
    expect(offset).not.toBe(-1);
    const line = lineOf(LAMBDA_BT, offset);
    expect(LAMBDA_BT.split("\n")[line]).toMatch(/class\s+withParams:.*body:.*env:/);
  });

  it("does not find withParams:body:env: on instance side (it's class-only)", () => {
    expect(findMethodDeclaration(LAMBDA_BT, "withParams:body:env:", "instance")).toBe(-1);
  });

  it("finds params unary instance method", () => {
    const offset = findMethodDeclaration(LAMBDA_BT, "params", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(LAMBDA_BT, offset);
    expect(LAMBDA_BT.split("\n")[line]).toMatch(/^\s*params\s*=>/);
  });

  it("does not match withParams: appearing in doc comment example", () => {
    // The doc comment contains `lam := SchemeLambda withParams: ...`
    const offset = findMethodDeclaration(LAMBDA_BT, "withParams:body:env:", "class");
    const line = lineOf(LAMBDA_BT, offset);
    expect(LAMBDA_BT.split("\n")[line].trimStart()).not.toMatch(/^\/\//);
    expect(LAMBDA_BT.split("\n")[line]).toContain("class withParams:");
  });
});

// ─── Edge-case tests ─────────────────────────────────────────────────────────

describe("findMethodDeclaration — edge cases", () => {
  it("skips // comment lines", () => {
    const src = `\
Object subclass: Foo
  // run => this is in a comment
  run =>
    42
`;
    const offset = findMethodDeclaration(src, "run", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(src, offset);
    expect(src.split("\n")[line].trimStart()).not.toMatch(/^\/\//);
  });

  it("skips /// doc comment lines", () => {
    const src = `\
Object subclass: Foo
  /// run => documented method
  run =>
    42
`;
    const offset = findMethodDeclaration(src, "run", "instance");
    const line = lineOf(src, offset);
    expect(src.split("\n")[line].trimStart()).not.toMatch(/^\/\/\//);
    expect(src.split("\n")[line]).toMatch(/^\s*run\s*=>/);
  });

  it("does not match instance method when looking for class-side", () => {
    const src = `\
Object subclass: Foo
  run =>
    42
`;
    expect(findMethodDeclaration(src, "run", "class")).toBe(-1);
  });

  it("does not match class method when looking for instance-side", () => {
    const src = `\
Object subclass: Foo
  class run => 42
`;
    expect(findMethodDeclaration(src, "run", "instance")).toBe(-1);
  });

  it("returns the offset pointing at the selector, not the line start", () => {
    const src = `Object subclass: Foo\n  run =>\n    42\n`;
    const offset = findMethodDeclaration(src, "run", "instance");
    // The text at the offset should start with the selector
    expect(src.slice(offset, offset + 3)).toBe("run");
  });
});

// ─── extractStateVarInfo tests ────────────────────────────────────────────────

describe("extractStateVarInfo", () => {
  const LAMBDA_STATE = `\
Value subclass: SchemeLambda
  state: params = #()  // List of parameter name strings
  state: body = nil  // Unevaluated body expression (AST)
  state: closureEnv = nil  // SchemeEnv actor at point of definition
  state: simple = 42
`;

  it("extracts default value and inline comment", () => {
    const info = extractStateVarInfo(LAMBDA_STATE, "params");
    expect(info).not.toBeUndefined();
    expect(info?.defaultValue).toBe("#()");
    expect(info?.comment).toBe("List of parameter name strings");
  });

  it("extracts nil default with comment", () => {
    const info = extractStateVarInfo(LAMBDA_STATE, "body");
    expect(info?.defaultValue).toBe("nil");
    expect(info?.comment).toBe("Unevaluated body expression (AST)");
  });

  it("extracts default without comment", () => {
    const info = extractStateVarInfo(LAMBDA_STATE, "simple");
    expect(info?.defaultValue).toBe("42");
    expect(info?.comment).toBeUndefined();
  });

  it("returns undefined for unknown var", () => {
    expect(extractStateVarInfo(LAMBDA_STATE, "nonexistent")).toBeUndefined();
  });

  it("skips comment lines", () => {
    const src = `Object subclass: Foo\n  // state: x = 1\n  state: x = 2\n`;
    const info = extractStateVarInfo(src, "x");
    expect(info?.defaultValue).toBe("2");
  });
});

// ─── findStateVarDeclaration tests ───────────────────────────────────────────

describe("findStateVarDeclaration", () => {
  it("finds a state var and returns offset pointing at the name", () => {
    const src = `Object subclass: Foo\n  state: count = 0\n`;
    const offset = findStateVarDeclaration(src, "count");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 5)).toBe("count");
  });

  it("handles a variable named 'state' without matching the prefix", () => {
    const src = `Object subclass: Foo\n  state: state = nil\n`;
    const offset = findStateVarDeclaration(src, "state");
    expect(offset).not.toBe(-1);
    // Must point at the variable name, not the `state:` keyword
    expect(src.slice(offset, offset + 5)).toBe("state");
    // The `state` at offset must be followed by ` = `, not `:`
    expect(src[offset + 5]).toBe(" ");
  });

  it("returns -1 for unknown variable", () => {
    const src = `Object subclass: Foo\n  state: count = 0\n`;
    expect(findStateVarDeclaration(src, "nonexistent")).toBe(-1);
  });

  it("skips comment lines", () => {
    const src = `Object subclass: Foo\n  // state: x = 1\n  state: x = 2\n`;
    const offset = findStateVarDeclaration(src, "x");
    expect(offset).not.toBe(-1);
    // Should be on the non-comment line
    const line = src.slice(0, offset).split("\n").length - 1;
    expect(src.split("\n")[line].trimStart()).not.toMatch(/^\/\//);
  });
});

// ─── extractMethodDocComment tests ───────────────────────────────────────────

const WITH_DOCS = `\
Object subclass: Foo

  /// Runs the program.
  class run => self new run

  /// Execute the main loop.
  ///
  /// Reads from stdin until EOF.
  run =>
    42
`;

describe("extractMethodDocComment", () => {
  it("extracts single-line doc for class method", () => {
    const doc = extractMethodDocComment(WITH_DOCS, "run", "class");
    expect(doc).toBe("Runs the program.");
  });

  it("extracts multi-line doc for instance method", () => {
    const doc = extractMethodDocComment(WITH_DOCS, "run", "instance");
    expect(doc).toBe("Execute the main loop.\n\nReads from stdin until EOF.");
  });

  it("class and instance run get different docs", () => {
    const classDoc = extractMethodDocComment(WITH_DOCS, "run", "class");
    const instanceDoc = extractMethodDocComment(WITH_DOCS, "run", "instance");
    expect(classDoc).not.toBe(instanceDoc);
  });

  it("returns undefined when no doc comment", () => {
    const src = `Object subclass: Foo\n  run => 42\n`;
    expect(extractMethodDocComment(src, "run", "instance")).toBeUndefined();
  });

  it("returns undefined for unknown selector", () => {
    expect(extractMethodDocComment(WITH_DOCS, "nonexistent", "instance")).toBeUndefined();
  });

  it("works with keyword selectors (env.bt lookup:)", () => {
    const doc = extractMethodDocComment(ENV_BT, "lookup:", "instance");
    expect(doc).toBe("Look up `name` in this frame, then walk parent frames until found.");
  });
});

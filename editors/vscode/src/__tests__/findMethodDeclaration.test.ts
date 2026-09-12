// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { describe, expect, it } from "vitest";
// The real implementation `vscode.Uri` is built on (published by Microsoft,
// used inside VS Code itself) — used below to test `aliasSourceUriString`/
// `parseAliasSourceUriPath` against real URI-parsing semantics instead of a
// hand-rolled simulation. `vscodeMock.ts` (used by other test files in this
// suite) only stubs `Uri.file`, never `Uri.parse`, so nothing exercised the
// real parser's behavior before — which is exactly how the two bugs
// documented below (double-decode, and the "//"-leading-path throw) shipped
// undetected.
import { URI } from "vscode-uri";
import {
  aliasSourceUriString,
  classNameToStdlibFilename,
  extractClassDocComment,
  extractMethodDocComment,
  extractStateVarDocComment,
  extractStateVarInfo,
  findClassDeclaration,
  findMethodDeclaration,
  findStateVarDeclaration,
  findTypeAliasDeclaration,
  offsetForDeclarationLine,
  parseAliasSourceUriPath,
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
  if (offset < 0) {
    throw new Error("Expected non-negative offset from declaration lookup");
  }
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

// ─── Typed method/state var tests ────────────────────────────────────────────

const TYPED_BT = `\
Object subclass: TypedAccount
  state: balance :: Integer = 0
  state: owner :: String = ""

  /// Returns current balance.
  balance -> Integer => self.balance

  deposit: amount :: Integer -> Integer =>
    self.balance := self.balance + amount
    self.balance
`;

describe("findMethodDeclaration — typed declarations", () => {
  it("finds a typed unary method (balance -> Integer =>)", () => {
    const offset = findMethodDeclaration(TYPED_BT, "balance", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(TYPED_BT, offset);
    expect(TYPED_BT.split("\n")[line]).toMatch(/balance\s*->/);
  });

  it("finds a typed keyword method (deposit:)", () => {
    const offset = findMethodDeclaration(TYPED_BT, "deposit:", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(TYPED_BT, offset);
    expect(TYPED_BT.split("\n")[line]).toMatch(/deposit:/);
  });

  it("extracts doc comment for typed unary method", () => {
    const doc = extractMethodDocComment(TYPED_BT, "balance", "instance");
    expect(doc).toBe("Returns current balance.");
  });
});

// Found by cross-checking findMethodDeclaration/findClassDeclaration/
// findStateVarDeclaration against every real declaration in stdlib/src
// (1281 methods, 107 classes, 103 state vars) — a `sealed`/`internal`
// modifier, a binary selector's typed parameter, and several type-position
// shapes (metaclass refs, generics, unions, singleton symbols) were never
// handled by the regex at all, so any stdlib method using them fell through
// to the document-symbol-provider tier (or worse, a raw text.indexOf guess).
describe("findMethodDeclaration — sealed/internal modifiers", () => {
  it("finds a sealed instance method (stdlib: Metaclass>>isMeta)", () => {
    const src = "Class subclass: Metaclass\n  sealed isMeta -> Boolean => true\n";
    const offset = findMethodDeclaration(src, "isMeta", "instance");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 6)).toBe("isMeta");
  });

  it("finds an internal keyword instance method (stdlib: BeamtalkInterface>>help:)", () => {
    const src = "Object subclass: Foo\n  internal help: aClass :: Object => nil\n";
    const offset = findMethodDeclaration(src, "help:", "instance");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 5)).toBe("help:");
  });

  it("finds a class-side method with class+sealed combined (stdlib: Duration class>>milliseconds:)", () => {
    const src =
      "Value subclass: Duration\n  class sealed milliseconds: n :: Number -> Duration => nil\n";
    const offset = findMethodDeclaration(src, "milliseconds:", "class");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 13)).toBe("milliseconds:");
  });

  it("finds a class-side method with sealed before class (either modifier order)", () => {
    const src = "Object subclass: Foo\n  sealed class current => nil\n";
    const offset = findMethodDeclaration(src, "current", "class");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 7)).toBe("current");
  });

  it("does not match a sealed method when searching the wrong side", () => {
    const src = "Object subclass: Foo\n  sealed isMeta => true\n";
    expect(findMethodDeclaration(src, "isMeta", "class")).toBe(-1);
  });

  it("a method literally named `class` is not eaten as a modifier (stdlib: ProtoObject>>class)", () => {
    const src = 'Object subclass: Foo\n  class => @intrinsic "class"\n';
    const offset = findMethodDeclaration(src, "class", "instance");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 5)).toBe("class");
  });

  it("a method literally named `sealed` is not eaten as a modifier", () => {
    const src = "Object subclass: Foo\n  sealed => 42\n";
    const offset = findMethodDeclaration(src, "sealed", "instance");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 6)).toBe("sealed");
  });
});

describe("findMethodDeclaration — binary selectors with typed params", () => {
  it("finds a binary selector with a typed param and return type (stdlib: Integer>>+)", () => {
    const src = "Number subclass: Integer\n  + other :: Number -> Integer => @primitive\n";
    const offset = findMethodDeclaration(src, "+", "instance");
    expect(offset).not.toBe(-1);
    expect(src[offset]).toBe("+");
  });

  it("finds an untyped binary selector param (no :: annotation)", () => {
    const src = "Object subclass: Vector\n  + other => self x + other x\n";
    const offset = findMethodDeclaration(src, "+", "instance");
    expect(offset).not.toBe(-1);
    expect(src[offset]).toBe("+");
  });

  it("treats `=:=` as one binary selector, not a keyword selector (stdlib: ProtoObject>>=:=)", () => {
    const src = 'Object subclass: Foo\n  =:= other :: ProtoObject -> Boolean => @intrinsic "=:="\n';
    const offset = findMethodDeclaration(src, "=:=", "instance");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 3)).toBe("=:=");
  });
});

describe("findMethodDeclaration — type-position syntax in params/returns", () => {
  it("finds a method with a metaclass return type (stdlib: Collection>>species)", () => {
    const src = "Value subclass: Collection\n  species -> Self class => self class\n";
    const offset = findMethodDeclaration(src, "species", "instance");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 7)).toBe("species");
  });

  it("finds a method with a generic return type (stdlib: FileHandle>>lines)", () => {
    const src = "Object subclass: FileHandle\n  lines -> Stream(String) => nil\n";
    const offset = findMethodDeclaration(src, "lines", "instance");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 5)).toBe("lines");
  });

  it("finds a method with a nested generic return type (stdlib: Package class>>all)", () => {
    const src =
      "Object subclass: Package\n  class all -> Dictionary(String, List(Package)) => nil\n";
    const offset = findMethodDeclaration(src, "all", "class");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 3)).toBe("all");
  });

  it("finds a method with a union return type (stdlib: Behaviour>>>>)", () => {
    const src =
      "Object subclass: Behaviour\n  sealed >> aSelector :: Symbol -> CompiledMethod | Nil => nil\n";
    const offset = findMethodDeclaration(src, ">>", "instance");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 2)).toBe(">>");
  });

  it("finds a method with singleton symbols in a union return type (stdlib: BeamtalkInterface>>logLevel)", () => {
    const src = "Object subclass: Foo\n  logLevel -> LogLevel | #all | #none => nil\n";
    const offset = findMethodDeclaration(src, "logLevel", "instance");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 8)).toBe("logLevel");
  });
});

// This codebase's real typed-parameter syntax uses `::` (not the single `:`
// the original methodHeadPattern assumed), often with generic type args.
const DOUBLE_COLON_BT = `\
Actor subclass: EventStore
  /// List executions matching a filter dictionary.
  ///
  /// ## Examples
  /// \`\`\`beamtalk
  /// store listExecutions: #{#status => #running}
  /// store listExecutions: #{}   // all executions
  /// \`\`\`
  listExecutions: filter :: Dictionary -> List =>
    42

  recordFailedAndRaise: eventStore :: EventStore workflowId: workflowId :: String payload: payload :: Dictionary(Symbol, JsonValue) =>
    42
`;

describe("findMethodDeclaration — :: typed params (BT-3439 gap)", () => {
  it("finds the real declaration, not the doc-comment usage example", () => {
    const offset = findMethodDeclaration(DOUBLE_COLON_BT, "listExecutions:", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(DOUBLE_COLON_BT, offset);
    expect(DOUBLE_COLON_BT.split("\n")[line]).toMatch(/^\s*listExecutions:\s+filter\s*::/);
  });

  it("handles multiple :: typed keyword params with a generic type argument", () => {
    const offset = findMethodDeclaration(
      DOUBLE_COLON_BT,
      "recordFailedAndRaise:workflowId:payload:",
      "instance"
    );
    expect(offset).not.toBe(-1);
    const line = lineOf(DOUBLE_COLON_BT, offset);
    expect(DOUBLE_COLON_BT.split("\n")[line]).toMatch(/^\s*recordFailedAndRaise:/);
  });

  it("handles a 3-keyword selector where a param name shadows a keyword (saveSnapshot:state:eventId:)", () => {
    const src = [
      "Actor subclass: EventStore",
      '  /// store saveSnapshot: "wf-1" state: #{#replayCursor => 50} eventId: 51',
      "  saveSnapshot: workflowId :: String state: state :: ReplaySnapshot eventId: eventId :: Integer -> Nil =>",
      "    42",
    ].join("\n");
    const offset = findMethodDeclaration(src, "saveSnapshot:state:eventId:", "instance");
    expect(offset).not.toBe(-1);
    const line = lineOf(src, offset);
    expect(src.split("\n")[line]).toMatch(/^\s*saveSnapshot:\s+workflowId\s*::/);
  });
});

describe("findStateVarDeclaration — typed state vars", () => {
  it("finds typed state var 'balance: Integer'", () => {
    const offset = findStateVarDeclaration(TYPED_BT, "balance");
    expect(offset).not.toBe(-1);
    expect(TYPED_BT.slice(offset, offset + 7)).toBe("balance");
  });

  it("finds typed state var 'owner: String'", () => {
    const offset = findStateVarDeclaration(TYPED_BT, "owner");
    expect(offset).not.toBe(-1);
    expect(TYPED_BT.slice(offset, offset + 5)).toBe("owner");
  });
});

describe("extractStateVarInfo — typed state vars", () => {
  it("extracts default value for typed state var", () => {
    const info = extractStateVarInfo(TYPED_BT, "balance");
    expect(info?.defaultValue).toBe("0");
  });

  it("extracts empty string default for typed string state var", () => {
    const info = extractStateVarInfo(TYPED_BT, "owner");
    expect(info?.defaultValue).toBe('""');
  });
});

// Reproduces task_queue_registry.bt's `state: queues :: Dictionary(String, TaskQueue)`
// — a typed state var with no `= default` at all.
const DEFAULTLESS_TYPED_BT = `\
typed Actor subclass: TaskQueueRegistry
  /// Internal mapping of queue names to TaskQueue actors.
  state: queues :: Dictionary(String, TaskQueue)

  initialize -> Nil =>
    self.queues := #{}
    nil
`;

describe("findStateVarDeclaration — defaultless typed state var", () => {
  it("finds a typed state var with no default value at all", () => {
    const offset = findStateVarDeclaration(DEFAULTLESS_TYPED_BT, "queues");
    expect(offset).not.toBe(-1);
    expect(DEFAULTLESS_TYPED_BT.slice(offset, offset + 6)).toBe("queues");
  });

  it("does not false-positive on a longer name sharing the same prefix", () => {
    const src = "state: queuesFoo :: Dictionary(String, TaskQueue)";
    expect(findStateVarDeclaration(src, "queues")).toBe(-1);
  });
});

describe("extractStateVarInfo — defaultless typed state var", () => {
  it("returns an empty info object rather than undefined", () => {
    const info = extractStateVarInfo(DEFAULTLESS_TYPED_BT, "queues");
    expect(info).toEqual({});
  });

  it("does not false-positive on a longer name sharing the same prefix", () => {
    const src = "state: queuesFoo :: Dictionary(String, TaskQueue)";
    expect(extractStateVarInfo(src, "queues")).toBeUndefined();
  });
});

describe("extractStateVarDocComment", () => {
  it("extracts a /// doc comment above a defaultless typed state var", () => {
    const doc = extractStateVarDocComment(DEFAULTLESS_TYPED_BT, "queues");
    expect(doc).toBe("Internal mapping of queue names to TaskQueue actors.");
  });

  it("extracts a multi-line doc comment", () => {
    const src = [
      "Object subclass: Foo",
      "  /// Line one.",
      "  ///",
      "  /// Line two.",
      "  state: bar = nil",
    ].join("\n");
    expect(extractStateVarDocComment(src, "bar")).toBe("Line one.\n\nLine two.");
  });

  it("returns undefined when there is no doc comment", () => {
    const src = "Object subclass: Foo\n  state: bar = nil\n";
    expect(extractStateVarDocComment(src, "bar")).toBeUndefined();
  });

  it("does not match a doc comment belonging to a different state var", () => {
    const src = [
      "Object subclass: Foo",
      "  /// Doc for bar.",
      "  state: bar = nil",
      "  state: baz = nil",
    ].join("\n");
    expect(extractStateVarDocComment(src, "baz")).toBeUndefined();
  });
});

// BT-3497: classes previously had no doc-comment-read fallback at all in the
// sidebar hover (unlike methods/state vars) — extractClassDocComment closes
// that gap. Mirrors extractStateVarDocComment/extractMethodDocComment's
// doc-comment-walk shape, and findClassDeclaration's declaration pattern
// (SuperClass subclass: ClassName, optionally generic).
describe("extractClassDocComment", () => {
  it("extracts a /// doc comment above a class declaration", () => {
    const src = [
      "/// A supervised background worker.",
      "Actor subclass: Worker",
      "  run => nil",
    ].join("\n");
    expect(extractClassDocComment(src, "Worker")).toBe("A supervised background worker.");
  });

  it("extracts a multi-line doc comment", () => {
    const src = [
      "/// Line one.",
      "///",
      "/// Line two.",
      "Object subclass: Foo",
      "  run => nil",
    ].join("\n");
    expect(extractClassDocComment(src, "Foo")).toBe("Line one.\n\nLine two.");
  });

  it("extracts a doc comment above a generic class declaration (stdlib: Collection(E))", () => {
    const src = [
      "/// Base class for all ordered collections.",
      "abstract typed Value subclass: Collection(E)",
    ].join("\n");
    expect(extractClassDocComment(src, "Collection")).toBe(
      "Base class for all ordered collections."
    );
  });

  it("returns undefined when there is no doc comment", () => {
    const src = "Object subclass: Foo\n  run => nil\n";
    expect(extractClassDocComment(src, "Foo")).toBeUndefined();
  });

  it("returns undefined for a class that isn't declared in the source", () => {
    const src = "/// Some doc.\nObject subclass: Foo\n";
    expect(extractClassDocComment(src, "Bar")).toBeUndefined();
  });

  it("does not match a doc comment belonging to a different class", () => {
    const src = ["/// Doc for Foo.", "Object subclass: Foo", "", "Object subclass: Bar"].join("\n");
    expect(extractClassDocComment(src, "Bar")).toBeUndefined();
  });
});

// `field:` is a synonym for `state:` used by several stdlib classes (e.g.
// RetryPolicy) — findStateVarDeclaration/extractStateVarInfo/
// extractStateVarDocComment only ever recognized `state:` before this.
describe("findStateVarDeclaration / extractStateVarInfo — field: keyword (stdlib synonym for state:)", () => {
  it("finds a field: declaration (stdlib: RetryPolicy)", () => {
    const src = "Value subclass: RetryPolicy\n  field: initialInterval :: Integer = 1000\n";
    const offset = findStateVarDeclaration(src, "initialInterval");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 15)).toBe("initialInterval");
  });

  it("extracts info from a field: declaration", () => {
    const src = "Value subclass: RetryPolicy\n  field: jitter :: Boolean = false\n";
    const info = extractStateVarInfo(src, "jitter");
    expect(info?.defaultValue).toBe("false");
  });

  it("extracts a doc comment above a field: declaration", () => {
    const src = [
      "Value subclass: RetryPolicy",
      "  /// Maximum number of retry attempts.",
      "  field: maximumAttempts :: Integer | Nil = nil",
    ].join("\n");
    expect(extractStateVarDocComment(src, "maximumAttempts")).toBe(
      "Maximum number of retry attempts."
    );
  });
});

// Verified against every real class declaration in stdlib/src (101/102
// matched exactly; BEAMError is the one hardcoded exception).
describe("classNameToStdlibFilename", () => {
  it("converts a plain PascalCase name", () => {
    expect(classNameToStdlibFilename("Array")).toBe("array.bt");
  });

  it("converts a multi-word PascalCase name", () => {
    expect(classNameToStdlibFilename("DateTime")).toBe("date_time.bt");
    expect(classNameToStdlibFilename("TaskQueueRegistry")).toBe("task_queue_registry.bt");
  });

  it("treats a short all-caps acronym with nothing following as one word", () => {
    expect(classNameToStdlibFilename("OS")).toBe("os.bt");
  });

  it("splits an acronym from a following capitalized word", () => {
    expect(classNameToStdlibFilename("HTTPClient")).toBe("http_client.bt");
  });

  it("uses the hardcoded exception for BEAMError instead of the derived beam_error", () => {
    expect(classNameToStdlibFilename("BEAMError")).toBe("beamerror.bt");
  });

  it("handles a name with a digit", () => {
    expect(classNameToStdlibFilename("Uuid")).toBe("uuid.bt");
  });
});

// BT-3496/BT-3505: the `beamtalk-alias://` virtual URI scheme for a type
// alias's read-only source view. `aliasSourceUriString` builds a
// percent-encoded URI string with no `vscode` dependency; `extension.ts`'s
// `aliasSourceUri` wraps it in `vscode.Uri.parse`. `parseAliasSourceUriPath`
// is the inverse, fed `.path` off the real `Uri` that produces.
//
// These round-trip tests go through the real `vscode-uri` `URI.parse` (not a
// hand-rolled simulation) specifically because two real bugs shipped
// undetected by simulated/mocked round-trips before: (1) double-decoding
// `.path` (BT-3496 review), and (2) `Uri.parse` *throwing*
// `UriError: ... the path cannot begin with two slash characters ("//")`
// for an authority-less URI whose path started with `//` — exactly what an
// empty/unknown `pkg` used to produce (`beamtalk-alias:////Foo.bt` →
// `.path === "//Foo.bt"`), reachable via `_hasNavigableAliasSource`'s
// pre-BT-3496 fallback path even against a current server. `aliasSourceUriString`
// now guarantees the package path segment is never empty (a marker prefix,
// `ALIAS_PACKAGE_SEGMENT_MARKER`) specifically to rule out that whole failure
// class rather than special-case the one input that triggered it (BT-3505).
function realUriPath(uriString: string): string {
  return URI.parse(uriString).path;
}

describe("aliasSourceUriString / parseAliasSourceUriPath", () => {
  it("builds a URI string embedding both package and name as path segments", () => {
    expect(aliasSourceUriString("Timeout", "my_app")).toBe("beamtalk-alias:///pmy_app/Timeout.bt");
  });

  it("marks an unknown package with the bare marker segment, never an empty one", () => {
    expect(aliasSourceUriString("Timeout", undefined)).toBe("beamtalk-alias:///p/Timeout.bt");
  });

  it("percent-encodes package/name characters that aren't URI-path-safe", () => {
    const uri = aliasSourceUriString("My Alias", "my pkg");
    expect(uri).toBe("beamtalk-alias:///pmy%20pkg/My%20Alias.bt");
  });

  it("does not throw when parsed by a real vscode.Uri, for any package presence (BT-3505 regression)", () => {
    for (const pkg of ["my_app", undefined, "", "beamtalk_stdlib"]) {
      const uri = aliasSourceUriString("Timeout", pkg);
      expect(() => URI.parse(uri)).not.toThrow();
    }
  });

  it("round-trips name and package through build → real Uri.parse → parse", () => {
    const uri = aliasSourceUriString("RestartStrategy", "beamtalk_stdlib");
    expect(parseAliasSourceUriPath(realUriPath(uri))).toEqual({
      name: "RestartStrategy",
      pkg: "beamtalk_stdlib",
    });
  });

  it("round-trips an unknown package back to undefined", () => {
    const uri = aliasSourceUriString("Timeout", undefined);
    expect(parseAliasSourceUriPath(realUriPath(uri))).toEqual({
      name: "Timeout",
      pkg: undefined,
    });
  });

  it("round-trips percent-encoded characters (space) back to their original form", () => {
    const uri = aliasSourceUriString("My Alias", "my pkg");
    expect(parseAliasSourceUriPath(realUriPath(uri))).toEqual({
      name: "My Alias",
      pkg: "my pkg",
    });
  });

  // Regression test: parseAliasSourceUriPath previously called
  // decodeURIComponent a second time on top of the real vscode.Uri's
  // already-decoded `.path`, throwing `URIError: URI malformed` for any
  // name/package containing a literal `%` and silently downgrading "Go to
  // Definition" to a false "Source not available" even though real content
  // existed.
  it("round-trips a literal '%' in the name/package without throwing (double-decode regression)", () => {
    const uri = aliasSourceUriString("50% Done", "100% Coverage");
    expect(() => parseAliasSourceUriPath(realUriPath(uri))).not.toThrow();
    expect(parseAliasSourceUriPath(realUriPath(uri))).toEqual({
      name: "50% Done",
      pkg: "100% Coverage",
    });
  });
});

describe("findClassDeclaration", () => {
  it("finds a generic class declaration (stdlib: Value subclass: Collection(E))", () => {
    const src = "abstract typed Value subclass: Collection(E)\n";
    const offset = findClassDeclaration(src, "Collection");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 10)).toBe("Collection");
  });

  it("finds a generic class declaration with multiple type params (stdlib: Result(T, E))", () => {
    const src = "sealed typed Value subclass: Result(T, E)\n";
    const offset = findClassDeclaration(src, "Result");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 6)).toBe("Result");
  });

  it("finds a class declared via SuperClass subclass: ClassName", () => {
    const src =
      "Actor subclass: TaskQueueRegistry\n  state: queues :: Dictionary(String, TaskQueue)\n";
    const offset = findClassDeclaration(src, "TaskQueueRegistry");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 17)).toBe("TaskQueueRegistry");
  });

  it("does not match a mention inside a doc-comment usage example", () => {
    const src = [
      "/// ## Examples",
      "/// ```beamtalk",
      "/// registry := TaskQueueRegistry spawn",
      "/// ```",
      "Actor subclass: TaskQueueRegistry",
      "  state: queues :: Dictionary(String, TaskQueue)",
    ].join("\n");
    const offset = findClassDeclaration(src, "TaskQueueRegistry");
    expect(offset).not.toBe(-1);
    const line = src.slice(0, offset).split("\n").length - 1;
    expect(src.split("\n")[line]).toBe("Actor subclass: TaskQueueRegistry");
  });

  it("returns -1 for a class that does not exist", () => {
    expect(findClassDeclaration("Object subclass: Foo", "Bar")).toBe(-1);
  });

  it("does not false-positive on a longer name sharing the same prefix", () => {
    const src = "Object subclass: FooBar";
    expect(findClassDeclaration(src, "Foo")).toBe(-1);
  });
});

describe("findTypeAliasDeclaration (ADR 0108 Phase 8, BT-2903)", () => {
  it("finds a plain type alias declaration", () => {
    const src = "type Timeout = Integer | #infinity\n";
    const offset = findTypeAliasDeclaration(src, "Timeout");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 7)).toBe("Timeout");
  });

  it("finds an internal type alias declaration", () => {
    const src = "internal type LogFormat = #text | #json\n";
    const offset = findTypeAliasDeclaration(src, "LogFormat");
    expect(offset).not.toBe(-1);
    expect(src.slice(offset, offset + 9)).toBe("LogFormat");
  });

  it("does not match a mention inside a doc-comment usage example", () => {
    const src = [
      "/// See the Timeout alias below for retry/backoff options.",
      "type Timeout = Integer | #infinity",
    ].join("\n");
    const offset = findTypeAliasDeclaration(src, "Timeout");
    expect(offset).not.toBe(-1);
    const line = src.slice(0, offset).split("\n").length - 1;
    expect(src.split("\n")[line]).toBe("type Timeout = Integer | #infinity");
  });

  it("returns -1 for an alias that does not exist", () => {
    expect(findTypeAliasDeclaration("type Timeout = Integer", "Nonexistent")).toBe(-1);
  });

  it("does not false-positive on a longer name sharing the same prefix", () => {
    expect(findTypeAliasDeclaration("type TimeoutMs = Integer", "Timeout")).toBe(-1);
  });
});

describe("offsetForDeclarationLine (BT-3439)", () => {
  // Actor subclass: Widget         <- line 1
  //   state: count = 0             <- line 2
  //   state: engine :: Engine      <- line 3
  //                                <- line 4
  //   increment =>                 <- line 5
  //     self.count := self.count + 1  <- line 6
  const SRC = [
    "Actor subclass: Widget",
    "  state: count = 0",
    "  state: engine :: Engine",
    "",
    "  increment =>",
    "    self.count := self.count + 1",
  ].join("\n");

  it("resolves a real line to the offset of its first non-whitespace column", () => {
    const offset = offsetForDeclarationLine(SRC, 2, "count");
    expect(offset).not.toBe(-1);
    expect(SRC.slice(offset, offset + 5)).toBe("state");
  });

  it("resolves a method line by its first selector keyword", () => {
    const offset = offsetForDeclarationLine(SRC, 5, "increment");
    expect(offset).not.toBe(-1);
    expect(SRC.slice(offset, offset + 9)).toBe("increment");
  });

  it("returns -1 for a line number outside the document", () => {
    expect(offsetForDeclarationLine(SRC, 0, "count")).toBe(-1);
    expect(offsetForDeclarationLine(SRC, 999, "count")).toBe(-1);
  });

  it("returns -1 when the line no longer contains the expected needle (BT-3439 stale line)", () => {
    // Simulates the file being edited (a line inserted above `engine`)
    // after the class was last compiled — beamtalk_xref still reports
    // line 3 for `engine`, but line 3 is now something else entirely.
    const edited = ["// a new comment", ...SRC.split("\n")].join("\n");
    expect(offsetForDeclarationLine(edited, 3, "engine")).toBe(-1);
    // The correct, shifted line (4) still resolves.
    expect(offsetForDeclarationLine(edited, 4, "engine")).not.toBe(-1);
  });

  it("does not false-positive on a stale line that merely contains the needle as a substring (review feedback)", () => {
    // `count`'s real declaration (line 2) is edited away; the stale line 3
    // it used to occupy now reads a comment that happens to contain
    // "count" as part of "discount" — a bare substring check would wrongly
    // validate this line and navigate there.
    const edited = ["Actor subclass: Widget", "// discount handling"].join("\n");
    expect(offsetForDeclarationLine(edited, 2, "count")).toBe(-1);
  });

  it("does not false-positive on a short method-keyword needle embedded in an unrelated word", () => {
    // `at:put:`'s first keyword ("at") is a substring of extremely common
    // tokens like "state" — a bare substring check would wrongly validate
    // any such line as the (stale) declaration of `at:put:`.
    expect(offsetForDeclarationLine("  state: count = 0", 1, "at")).toBe(-1);
  });

  it("still matches a real declaration whose needle sits at a line's start/end", () => {
    expect(offsetForDeclarationLine("count", 1, "count")).not.toBe(-1);
  });

  it("matches a symbolic binary-selector needle correctly", () => {
    const src = ["Object subclass: Vector", "  + other =>", "    self x + other x"].join("\n");
    const offset = offsetForDeclarationLine(src, 2, "+");
    expect(offset).not.toBe(-1);
    expect(src[offset]).toBe("+");
  });

  it("does not false-positive on a stale line landing on a doc-comment usage example", () => {
    // Reproduces the reported bug: a doc comment above the real declaration
    // mentions the selector in a usage example (`/// store listExecutions: ...`).
    // If the recorded line goes stale and now points at that comment line,
    // the word-boundary check alone would wrongly validate it since
    // "listExecutions" does appear there at a word boundary.
    const src = [
      "  /// List executions matching a filter dictionary.",
      "  ///",
      "  /// ## Examples",
      "  /// ```beamtalk",
      "  /// store listExecutions: #{#status => #running}",
      "  /// ```",
      "  listExecutions: filter :: Dictionary -> List =>",
    ].join("\n");
    expect(offsetForDeclarationLine(src, 5, "listExecutions")).toBe(-1);
    expect(offsetForDeclarationLine(src, 7, "listExecutions")).not.toBe(-1);
  });
});

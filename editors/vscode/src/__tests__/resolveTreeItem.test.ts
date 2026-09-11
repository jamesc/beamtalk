// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { beforeEach, describe, expect, it, vi } from "vitest";
// Type-only: erased at compile time, never touched at runtime (see the
// `vscode.mock` note below and workspaceTreeView.test.ts's identical comment).
import type * as vscode from "vscode";
import { buildVscodeModule } from "./vscodeMock";

// ─── `vscode` module mock (shared with workspaceTreeView.test.ts) ─────────────

const { executeCommandMock, openTextDocumentMock } = vi.hoisted(() => ({
  executeCommandMock: vi.fn(),
  openTextDocumentMock: vi.fn(),
}));

vi.mock("vscode", () => buildVscodeModule({ executeCommandMock, openTextDocumentMock }));

import type { ClassItemNode, MethodItemNode, StateVarItemNode } from "../workspaceTreeView";
import { WorkspaceTreeDataProvider } from "../workspaceTreeView";

/** A blank TreeItem, as `getTreeItem` produces before `resolveTreeItem` fills in the tooltip. */
function blankItem(): vscode.TreeItem {
  return { tooltip: undefined } as unknown as vscode.TreeItem;
}

const noToken = {} as unknown as vscode.CancellationToken;

/**
 * A fake TextDocument backed by a plain string, matching what the fast path
 * needs. `uri` mirrors what real `openTextDocument` returns (a document
 * carries the URI it was opened from) — `_resolveClassDocument` reads it off
 * the resolved document rather than recomputing it from `source_file`.
 */
function makeDoc(
  text: string,
  uri: unknown = {
    fsPath: "/proj/account.bt",
    path: "/proj/account.bt",
    toString: () => "/proj/account.bt",
  }
) {
  return {
    uri,
    getText: () => text,
    positionAt: (offset: number) => {
      const before = text.slice(0, offset);
      const lines = before.split("\n");
      return { line: lines.length - 1, character: lines[lines.length - 1].length };
    },
  };
}

const SOURCE = [
  "class Account",
  "  state: balance :: Integer = 0",
  "",
  "  deposit: amount =>",
  "    balance := balance + amount",
  "",
].join("\n");

// `findMethodDeclaration`'s regex understands a generic type argument with
// one level of nesting (`List(Foo)`) but not two (`Dictionary(String,
// List(Dictionary(String, Foo)))`), so this is real source shaped
// specifically to defeat the text-search fast path and force the
// document-symbol-provider fallback.
const SOURCE_TYPED_PARAM = [
  "class Account",
  "  state: balance :: Integer = 0",
  "",
  "  deposit: amount :: Dictionary(String, List(Dictionary(String, Integer))) =>",
  "    balance := balance + amount",
  "",
].join("\n");

const classInfo = { name: "Account", source_file: "/proj/account.bt", actor_count: 0 };

// Mirrors beamtalk-language-service's document_symbols_provider.rs flat shape —
// the fallback path when no declared line is available.
const flatDocumentSymbols = [
  {
    name: "Account (class)",
    kind: 4, // Class
    selectionRange: { start: { line: 0, character: 6 }, end: { line: 0, character: 13 } },
    children: [
      {
        name: "deposit:",
        kind: 5, // Method
        selectionRange: { start: { line: 3, character: 2 }, end: { line: 3, character: 10 } },
        children: [],
      },
    ],
  },
];

// BT-3440: a class with an instance-side and a class-side method sharing the
// same selector. Before BT-3439's real-line fast path existed, both sides'
// hover fell through to `_findSymbolPosition`, whose `targetKind` array is
// identical for "method" and "class-method" — so it couldn't tell them apart
// and always returned whichever same-named symbol its walk hit first.
const SOURCE_BOTH_SIDES = [
  "class Account",
  "  state: balance :: Integer = 0",
  "",
  "  deposit: amount =>",
  "    balance := balance + amount",
  "",
  "  class deposit: amount =>",
  "    ^self new",
  "",
].join("\n");

describe("sidebar hover tooltip resolution (resolveTreeItem)", () => {
  let provider: InstanceType<typeof WorkspaceTreeDataProvider>;

  beforeEach(() => {
    provider = new WorkspaceTreeDataProvider();
    executeCommandMock.mockReset();
    openTextDocumentMock.mockReset();
    openTextDocumentMock.mockResolvedValue(makeDoc(SOURCE));
  });

  it("hovers a method with a known declared line without a full document-symbol computation", async () => {
    executeCommandMock.mockImplementation((cmd: string) => {
      if (cmd === "vscode.executeHoverProvider") {
        return Promise.resolve([{ contents: [{ value: "**deposit:** amount" }] }]);
      }
      return Promise.resolve(undefined);
    });
    const node: MethodItemNode = {
      kind: "method-item",
      method: { name: "deposit:", selector: "deposit:", side: "instance", line: 4 },
      classInfo,
    };
    const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
    expect((resolved?.tooltip as { value: string }).value).toContain("deposit:");
    // The whole point of the fast path: skip the full-file symbol computation
    // every sidebar hover previously paid for, on top of the hover request
    // itself — see the comment on `_lspHoverTooltip`.
    expect(executeCommandMock).not.toHaveBeenCalledWith(
      "vscode.executeDocumentSymbolProvider",
      expect.anything()
    );
  });

  it("falls back to the document-symbol provider when text search can't locate the declaration", async () => {
    openTextDocumentMock.mockResolvedValue(makeDoc(SOURCE_TYPED_PARAM));
    executeCommandMock.mockImplementation((cmd: string) => {
      if (cmd === "vscode.executeDocumentSymbolProvider") {
        return Promise.resolve(flatDocumentSymbols);
      }
      if (cmd === "vscode.executeHoverProvider") {
        return Promise.resolve([{ contents: [{ value: "**deposit:** amount" }] }]);
      }
      return Promise.resolve(undefined);
    });
    const node: MethodItemNode = {
      kind: "method-item",
      method: { name: "deposit:", selector: "deposit:", side: "instance" }, // no `line`
      classInfo,
    };
    const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
    expect((resolved?.tooltip as { value: string }).value).toContain("deposit:");
    expect(executeCommandMock).toHaveBeenCalledWith(
      "vscode.executeDocumentSymbolProvider",
      expect.anything()
    );
  });

  it("falls back to the plain fallback tooltip when the LSP has no hover for the method", async () => {
    executeCommandMock.mockImplementation(() => Promise.resolve(undefined));
    const node: MethodItemNode = {
      kind: "method-item",
      method: { name: "deposit:", selector: "deposit:", side: "instance", line: 4 },
      classInfo,
    };
    const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
    // Never literally empty: _methodTooltipFallback always produces something.
    expect(resolved?.tooltip).toBeTruthy();
  });

  it("resolves a class-item tooltip via the document-symbol fallback (no declared-line metadata for classes)", async () => {
    executeCommandMock.mockImplementation((cmd: string) => {
      if (cmd === "vscode.executeDocumentSymbolProvider") {
        return Promise.resolve(flatDocumentSymbols);
      }
      if (cmd === "vscode.executeHoverProvider") {
        return Promise.resolve([{ contents: [{ value: "**Account**" }] }]);
      }
      return Promise.resolve(undefined);
    });
    const node: ClassItemNode = { kind: "class-item", info: classInfo };
    const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
    expect((resolved?.tooltip as { value: string }).value).toContain("Account");
  });

  // BT-3497: unlike methods/state vars, a class-item tooltip previously had
  // no doc-comment-read fallback at all — an LSP hover miss (the position is
  // found via the fast text-search offset, but `executeHoverProvider` itself
  // returns nothing, and the document-symbol fallback also comes up empty)
  // fell straight to the hardcoded name+instance-count fallback, losing the
  // class's real `///` documentation even though it's sitting right there in
  // the same source file `_resolveClassDocument` already opened.
  it("BT-3497: falls back to the class's /// doc comment when LSP hover returns nothing", async () => {
    const src = [
      "/// Holds a customer's balance and supports deposits.",
      "Object subclass: Account",
      "  state: balance :: Integer = 0",
      "",
      "  deposit: amount =>",
      "    balance := balance + amount",
      "",
    ].join("\n");
    openTextDocumentMock.mockResolvedValue(makeDoc(src));
    executeCommandMock.mockImplementation(() => Promise.resolve(undefined));

    const node: ClassItemNode = { kind: "class-item", info: classInfo };
    const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
    const tooltip = (resolved?.tooltip as { value: string }).value;
    expect(tooltip).toBe("Holds a customer's balance and supports deposits.");
    expect(tooltip).not.toContain("**Account**");
  });

  it("BT-3497: falls back to the plain name+count tooltip when there's no doc comment either", async () => {
    executeCommandMock.mockImplementation(() => Promise.resolve(undefined));
    const node: ClassItemNode = {
      kind: "class-item",
      info: { ...classInfo, actor_count: 2 },
    };
    const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
    const tooltip = (resolved?.tooltip as { value: string }).value;
    // SOURCE (the default fixture) has no /// comment above `class Account`.
    expect(tooltip).toContain("Account");
    expect(tooltip).toContain("2 running instances");
  });

  it("BT-3440: distinguishes an instance-side and class-side method sharing a selector via the declared-line fast path", async () => {
    openTextDocumentMock.mockResolvedValue(makeDoc(SOURCE_BOTH_SIDES));
    executeCommandMock.mockImplementation((cmd: string, _uri: unknown, pos?: { line: number }) => {
      if (cmd === "vscode.executeHoverProvider") {
        // Real hovers are position-sensitive: line 3 (0-based) is the instance
        // method's head, line 6 is the class method's — a stand-in for the
        // LSP actually resolving the symbol under the cursor.
        const content =
          pos?.line === 6 ? "**deposit:** (class) amount" : "**deposit:** (instance) amount";
        return Promise.resolve([{ contents: [{ value: content }] }]);
      }
      return Promise.resolve(undefined);
    });

    const instanceNode: MethodItemNode = {
      kind: "method-item",
      method: { name: "deposit:", selector: "deposit:", side: "instance", line: 4 },
      classInfo,
    };
    const classNode: MethodItemNode = {
      kind: "method-item",
      method: { name: "deposit:", selector: "deposit:", side: "class", line: 7 },
      classInfo,
    };

    const instanceResolved = await provider.resolveTreeItem(blankItem(), instanceNode, noToken);
    const classResolved = await provider.resolveTreeItem(blankItem(), classNode, noToken);

    expect((instanceResolved?.tooltip as { value: string }).value).toContain("(instance)");
    expect((classResolved?.tooltip as { value: string }).value).toContain("(class)");
    // The fast path resolves both without ever hitting the ambiguous
    // document-symbol fallback.
    expect(executeCommandMock).not.toHaveBeenCalledWith(
      "vscode.executeDocumentSymbolProvider",
      expect.anything()
    );
  });

  it("BT-3440: distinguishes them via the side-aware regex fallback when no declared line is available", async () => {
    openTextDocumentMock.mockResolvedValue(makeDoc(SOURCE_BOTH_SIDES));
    executeCommandMock.mockImplementation((cmd: string, _uri: unknown, pos?: { line: number }) => {
      if (cmd === "vscode.executeHoverProvider") {
        const content =
          pos?.line === 6 ? "**deposit:** (class) amount" : "**deposit:** (instance) amount";
        return Promise.resolve([{ contents: [{ value: content }] }]);
      }
      return Promise.resolve(undefined);
    });

    const instanceNode: MethodItemNode = {
      kind: "method-item",
      method: { name: "deposit:", selector: "deposit:", side: "instance" }, // no `line`
      classInfo,
    };
    const classNode: MethodItemNode = {
      kind: "method-item",
      method: { name: "deposit:", selector: "deposit:", side: "class" }, // no `line`
      classInfo,
    };

    const instanceResolved = await provider.resolveTreeItem(blankItem(), instanceNode, noToken);
    const classResolved = await provider.resolveTreeItem(blankItem(), classNode, noToken);

    expect((instanceResolved?.tooltip as { value: string }).value).toContain("(instance)");
    expect((classResolved?.tooltip as { value: string }).value).toContain("(class)");
    expect(executeCommandMock).not.toHaveBeenCalledWith(
      "vscode.executeDocumentSymbolProvider",
      expect.anything()
    );
  });

  describe("BT-3444: synthetic (compiler-generated) methods", () => {
    const syntheticNode: MethodItemNode = {
      kind: "method-item",
      method: {
        name: "withBalance:",
        selector: "withBalance:",
        side: "instance",
        line: 2, // BT-3439 line: the `state: balance` slot declaration, not this method
        source_status: "synthetic",
        signature: "withBalance: aValue -> Account",
        doc: "Compiler-derived copy-setter for slot `balance`.",
      },
      classInfo,
    };

    it("badges the tree item as synthetic without a navigable command", () => {
      const item = provider.getTreeItem(syntheticNode);
      expect(item.contextValue).toBe("method-item-synthetic");
      expect(item.command).toBeUndefined();
      expect((item.iconPath as { id: string }).id).toBe("gear");
    });

    it("builds the tooltip from the wire-supplied signature/doc, never a file read or LSP round trip", async () => {
      const resolved = await provider.resolveTreeItem(blankItem(), syntheticNode, noToken);
      const tooltip = (resolved?.tooltip as { value: string }).value;
      expect(tooltip).toContain("withBalance: aValue -> Account");
      expect(tooltip).toContain("compiler-generated");
      expect(tooltip).toContain("Compiler-derived copy-setter for slot `balance`.");
      expect(openTextDocumentMock).not.toHaveBeenCalled();
      expect(executeCommandMock).not.toHaveBeenCalled();
    });

    it("falls back to the bare selector when no signature was resolved", async () => {
      const node: MethodItemNode = {
        kind: "method-item",
        method: {
          name: "withBalance:",
          selector: "withBalance:",
          side: "instance",
          source_status: "synthetic",
        },
        classInfo,
      };
      const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
      expect((resolved?.tooltip as { value: string }).value).toContain("withBalance:");
    });
  });

  // Reproduces the reported bug: HTTPClient class>>supervisionSpec (a
  // native method injected for every Actor subclass) has `source_status:
  // "unindexed_runtime_fun"` — the backend's own doc calls this "no openable
  // source" — but the sidebar only special-cased `synthetic`, so this row
  // got a normal "Go to Definition" command that always silently failed to
  // find anything (HTTPClient.bt has no `supervisionSpec` text at all).
  describe("unindexed_runtime_fun methods (native/runtime-only, no openable source)", () => {
    const unindexedNode: MethodItemNode = {
      kind: "method-item",
      method: {
        name: "supervisionSpec",
        selector: "supervisionSpec",
        side: "class",
        source_status: "unindexed_runtime_fun",
      },
      classInfo,
    };

    it("badges the tree item without a navigable command", () => {
      const item = provider.getTreeItem(unindexedNode);
      expect(item.contextValue).toBe("method-item-unindexed");
      expect(item.command).toBeUndefined();
      expect((item.iconPath as { id: string }).id).toBe("gear");
    });

    it("never attempts a file read or LSP round trip for its tooltip", async () => {
      const resolved = await provider.resolveTreeItem(blankItem(), unindexedNode, noToken);
      const tooltip = (resolved?.tooltip as { value: string }).value;
      expect(tooltip).toContain("supervisionSpec");
      expect(tooltip).toContain("no source available");
      expect(openTextDocumentMock).not.toHaveBeenCalled();
      expect(executeCommandMock).not.toHaveBeenCalled();
    });
  });

  // Reproduces the reported bug: a class/method/state-var whose defining
  // class is stdlib-origin has no real `source_file` (the runtime never
  // tracks one for compiled-in stdlib classes) — every hover path used to
  // check only `source_file`/"unknown" and bail straight to the hardcoded
  // fallback tooltip, so a stdlib-defined method never showed its doc
  // comment. Hit hardest for *inherited* methods, since most ancestors
  // (Object, Actor, Collection, ...) are stdlib. `_resolveClassDocument`
  // now falls back to the injected stdlib virtual-URI opener, the same one
  // `beamtalk.navigateToMethod`/`openClassSource` already use.
  describe("stdlib-origin classes (no real source_file)", () => {
    const stdlibClassInfo = { name: "Actor", source_origin: "stdlib" as const };
    const stdlibDocOpener = vi.fn();

    beforeEach(() => {
      provider.setStdlibDocumentOpener(stdlibDocOpener);
      stdlibDocOpener.mockReset();
    });

    it("reads a method's doc comment via the stdlib opener when no real source_file exists", async () => {
      const source = [
        "class Actor",
        "",
        "  /// Spawns a new linked child process.",
        "  spawn =>",
        "    ^nil",
        "",
      ].join("\n");
      stdlibDocOpener.mockResolvedValue(makeDoc(source, { fsPath: "Actor.bt", path: "Actor.bt" }));
      executeCommandMock.mockImplementation(() => Promise.resolve(undefined)); // no LSP hover available

      const node: MethodItemNode = {
        kind: "method-item",
        method: { name: "spawn", selector: "spawn", side: "instance" },
        classInfo: stdlibClassInfo,
        definingClass: "Actor",
      };
      const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
      const tooltip = (resolved?.tooltip as { value: string }).value;

      expect(stdlibDocOpener).toHaveBeenCalledWith(stdlibClassInfo);
      expect(openTextDocumentMock).not.toHaveBeenCalled();
      expect(tooltip).toContain("Spawns a new linked child process.");
      expect(tooltip).toContain("_Inherited from Actor_");
    });

    it("reads a state variable's doc comment via the stdlib opener when no real source_file exists", async () => {
      const source = [
        "class Actor",
        "  /// The process id backing this actor.",
        "  state: pid = nil",
        "",
      ].join("\n");
      stdlibDocOpener.mockResolvedValue(makeDoc(source, { fsPath: "Actor.bt", path: "Actor.bt" }));

      const node: StateVarItemNode = {
        kind: "state-item",
        stateVar: { name: "pid" },
        classInfo: stdlibClassInfo,
      };
      const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
      const tooltip = (resolved?.tooltip as { value: string }).value;

      expect(stdlibDocOpener).toHaveBeenCalledWith(stdlibClassInfo);
      expect(tooltip).toContain("The process id backing this actor.");
    });

    it("falls back to the plain tooltip (never a file read) when no stdlib opener is injected", async () => {
      provider.setStdlibDocumentOpener(null);
      executeCommandMock.mockImplementation(() => Promise.resolve(undefined));

      const node: MethodItemNode = {
        kind: "method-item",
        method: { name: "spawn", selector: "spawn", side: "instance" },
        classInfo: stdlibClassInfo,
        definingClass: "Actor",
      };
      const resolved = await provider.resolveTreeItem(blankItem(), node, noToken);
      const tooltip = (resolved?.tooltip as { value: string }).value;

      expect(openTextDocumentMock).not.toHaveBeenCalled();
      expect(tooltip).toContain("spawn");
      expect(tooltip).toContain("_Inherited from Actor_");
    });
  });
});

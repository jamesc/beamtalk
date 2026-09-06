// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { beforeEach, describe, expect, it, vi } from "vitest";
// Type-only: erased at compile time, never touched at runtime (see the
// `vscode.mock` note below and workspaceTreeView.test.ts's identical comment).
import type * as vscode from "vscode";

// ─── Minimal `vscode` module mock (mirrors workspaceTreeView.test.ts) ──────────

const { executeCommandMock, openTextDocumentMock } = vi.hoisted(() => ({
  executeCommandMock: vi.fn(),
  openTextDocumentMock: vi.fn(),
}));

vi.mock("vscode", () => {
  class ThemeIcon {
    constructor(public id: string) {}
  }
  class MarkdownString {
    value = "";
    constructor(value?: string) {
      if (value) this.value = value;
    }
    appendMarkdown(v: string) {
      this.value += v;
    }
  }
  class TreeItem {
    label: string;
    collapsibleState: number;
    description?: string;
    iconPath?: unknown;
    contextValue?: string;
    tooltip?: unknown;
    command?: unknown;
    constructor(label: string, collapsibleState?: number) {
      this.label = label;
      this.collapsibleState = collapsibleState ?? 0;
    }
  }
  class EventEmitter<T> {
    private listeners: Array<(value: T) => void> = [];
    event = (listener: (value: T) => void) => {
      this.listeners.push(listener);
      return { dispose: () => {} };
    };
    fire(value: T): void {
      for (const listener of this.listeners) listener(value);
    }
    dispose(): void {
      this.listeners = [];
    }
  }
  return {
    TreeItemCollapsibleState: { None: 0, Collapsed: 1, Expanded: 2 },
    ThemeIcon,
    MarkdownString,
    TreeItem,
    EventEmitter,
    Uri: { file: (p: string) => ({ fsPath: p, path: p, toString: () => p }) },
    SymbolKind: {
      Class: 4,
      Method: 5,
      Field: 7,
    },
    commands: { executeCommand: executeCommandMock },
    window: {},
    workspace: { openTextDocument: openTextDocumentMock },
  };
});

import { WorkspaceTreeDataProvider } from "../workspaceTreeView";
import type { ClassItemNode, MethodItemNode } from "../workspaceTreeView";

/** A blank TreeItem, as `getTreeItem` produces before `resolveTreeItem` fills in the tooltip. */
function blankItem(): vscode.TreeItem {
  return { tooltip: undefined } as unknown as vscode.TreeItem;
}

const noToken = {} as unknown as vscode.CancellationToken;

/** A fake TextDocument backed by a plain string, matching what the fast path needs. */
function makeDoc(text: string) {
  return {
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

// BT-3439: `findMethodDeclaration`'s regex doesn't understand this codebase's
// `::` typed-parameter syntax (`deposit: amount :: Integer =>`), so this is
// real source shaped specifically to defeat the text-search fast path and
// force the document-symbol-provider fallback.
const SOURCE_TYPED_PARAM = [
  "class Account",
  "  state: balance :: Integer = 0",
  "",
  "  deposit: amount :: Integer =>",
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
});

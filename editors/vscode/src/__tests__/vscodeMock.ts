// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import type { vi } from "vitest";
// The real implementation `vscode.Uri` is built on (published by Microsoft,
// used inside VS Code itself) — `Uri.parse` below delegates to it so any test
// exercising code under test's `vscode.Uri.parse()` call gets real parsing/
// validation semantics (which throw for cases WHATWG `URL` tolerates, e.g. an
// authority-less URI whose path starts with `//`), not a hand-rolled stand-in
// that silently accepts anything. Before this, only `Uri.file` was mocked at
// all — `Uri.parse` was entirely unstubbed, so no test using this shared mock
// could ever have caught a `vscode.Uri.parse` throw (see BT-3505).
import { URI } from "vscode-uri";

// ─── Shared `vscode` module mock ───────────────────────────────────────────────
//
// `workspaceTreeView.ts` runs inside the extension host in production, where
// `vscode` is a real module injected by VS Code. Under `vitest` there is no
// extension host, so `vscode` must be mocked before the module under test is
// imported. Only the runtime VALUES the tree view actually constructs/calls
// need a fake — everything else it imports from `vscode` (`CancellationToken`,
// `DocumentSymbol`, `Uri`, `SymbolKind`, …) is used purely in type positions,
// erased at compile time, and never touched at runtime by the code paths these
// tests exercise. Shared by `workspaceTreeView.test.ts` and
// `resolveTreeItem.test.ts` so the mocked surface only needs updating in one
// place.
//
// Usage (per test file, mirroring vitest's mock-hoisting requirements — the
// handles must be created inline inside `vi.hoisted` since referencing an
// imported factory there runs into vitest's import-hoisting order; only the
// `vi.mock` factory itself, invoked lazily, is safe to hand off to an
// imported builder):
//
//   const { executeCommandMock, openTextDocumentMock } = vi.hoisted(() => ({
//     executeCommandMock: vi.fn(),
//     openTextDocumentMock: vi.fn(),
//   }));
//   vi.mock("vscode", () => buildVscodeModule({ executeCommandMock, openTextDocumentMock }));

export interface VscodeMockHandles {
  executeCommandMock: ReturnType<typeof vi.fn>;
  openTextDocumentMock: ReturnType<typeof vi.fn>;
}

/** Build the mocked `vscode` module shape, wired to the given handles. */
export function buildVscodeModule(handles: VscodeMockHandles) {
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
    Uri: { file: (p: string) => ({ fsPath: p, path: p, toString: () => p }), parse: URI.parse },
    SymbolKind: { Class: 4, Method: 5, Field: 7, Interface: 10 },
    commands: { executeCommand: handles.executeCommandMock },
    window: {},
    workspace: { openTextDocument: handles.openTextDocumentMock },
  };
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { describe, expect, it } from "vitest";
import {
  planDocumentRetarget,
  type TabSnapshot,
  type VisibleEditorSnapshot,
} from "../documentMoved";

// BT-3285: the server's `beamtalk-lsp/documentMoved` notification tells the
// extension which editor URI to retarget. `planDocumentRetarget` is the pure
// decision logic `handleDocumentMoved` (`extension.ts`) uses to figure out
// which open tabs need retargeting and what view state to restore — tested
// directly here since it (unlike `handleDocumentMoved` itself) has no
// `vscode` dependency. See `documentMoved.ts` for why the split exists.

describe("planDocumentRetarget", () => {
  const params = { oldUri: "file:///ws/counter.bt", newUri: "file:///ws/accumulator.bt" };

  it("returns a plan carrying the visible editor's selection for a matching tab", () => {
    const tabs: TabSnapshot[] = [{ uri: params.oldUri, viewColumn: 1 }];
    const editors: VisibleEditorSnapshot[] = [
      {
        uri: params.oldUri,
        viewColumn: 1,
        selection: { startLine: 3, startCharacter: 2, endLine: 3, endCharacter: 8 },
      },
    ];

    expect(planDocumentRetarget(tabs, editors, params)).toEqual([
      {
        viewColumn: 1,
        selection: { startLine: 3, startCharacter: 2, endLine: 3, endCharacter: 8 },
      },
    ]);
  });

  it("returns a plan with no selection for a matching tab that isn't visible", () => {
    // A background tab in an unfocused split — `vscode.window.visibleTextEditors`
    // never includes it, but the tab must still be retargeted.
    const tabs: TabSnapshot[] = [{ uri: params.oldUri, viewColumn: 1 }];

    expect(planDocumentRetarget(tabs, [], params)).toEqual([
      { viewColumn: 1, selection: undefined },
    ]);
  });

  it("ignores tabs and editors showing unrelated files", () => {
    const tabs: TabSnapshot[] = [{ uri: "file:///ws/other.bt", viewColumn: 1 }];
    const editors: VisibleEditorSnapshot[] = [
      {
        uri: "file:///ws/other.bt",
        viewColumn: 1,
        selection: { startLine: 0, startCharacter: 0, endLine: 0, endCharacter: 0 },
      },
    ];

    expect(planDocumentRetarget(tabs, editors, params)).toEqual([]);
  });

  it("returns one plan per split-view tab, each with its own view column's selection", () => {
    const tabs: TabSnapshot[] = [
      { uri: params.oldUri, viewColumn: 1 },
      { uri: params.oldUri, viewColumn: 2 },
    ];
    const editors: VisibleEditorSnapshot[] = [
      {
        uri: params.oldUri,
        viewColumn: 1,
        selection: { startLine: 0, startCharacter: 0, endLine: 0, endCharacter: 0 },
      },
      {
        uri: params.oldUri,
        viewColumn: 2,
        selection: { startLine: 5, startCharacter: 1, endLine: 5, endCharacter: 1 },
      },
    ];

    const plans = planDocumentRetarget(tabs, editors, params);
    expect(plans).toHaveLength(2);
    expect(plans.map((p) => p.viewColumn)).toEqual([1, 2]);
    expect(plans.map((p) => p.selection?.startLine)).toEqual([0, 5]);
  });

  it("keys a captured selection by view column, not by array position", () => {
    // Two tabs at the old path share the same view column (one is the
    // active tab in that group, one is a background tab behind it) — only
    // the visible editor's selection exists to capture, and it must attach
    // to *both* same-column plans rather than only whichever tab happened
    // to be first.
    const tabs: TabSnapshot[] = [
      { uri: params.oldUri, viewColumn: 1 },
      { uri: params.oldUri, viewColumn: 1 },
    ];
    const editors: VisibleEditorSnapshot[] = [
      {
        uri: params.oldUri,
        viewColumn: 1,
        selection: { startLine: 7, startCharacter: 0, endLine: 7, endCharacter: 4 },
      },
    ];

    const plans = planDocumentRetarget(tabs, editors, params);
    expect(plans).toHaveLength(2);
    for (const plan of plans) {
      expect(plan.selection).toEqual({
        startLine: 7,
        startCharacter: 0,
        endLine: 7,
        endCharacter: 4,
      });
    }
  });

  it("returns no plans when there are no matching tabs", () => {
    expect(planDocumentRetarget([], [], params)).toEqual([]);
  });

  it("returns no plans when only the new path has a tab or editor", () => {
    // Shouldn't normally happen (the new path was never open before the
    // rename), but the filter must key strictly off `oldUri`.
    const tabs: TabSnapshot[] = [{ uri: params.newUri, viewColumn: 1 }];
    const editors: VisibleEditorSnapshot[] = [
      {
        uri: params.newUri,
        viewColumn: 1,
        selection: { startLine: 0, startCharacter: 0, endLine: 0, endCharacter: 0 },
      },
    ];

    expect(planDocumentRetarget(tabs, editors, params)).toEqual([]);
  });
});

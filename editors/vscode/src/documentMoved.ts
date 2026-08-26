// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/**
 * Params carried by the LSP server's `beamtalk-lsp/documentMoved`
 * notification (BT-3285) — `{oldUri, newUri}` on the wire. Sent from
 * `apply_rename_class_move` (`crates/beamtalk-lsp/src/server.rs`) for a
 * `renameTo:`/`moveClass:to:` flush, at the point the (now-removed)
 * `workspace/applyEdit` `RenameFile` op used to fire: that op silently
 * no-ops in VS Code once the runtime has already renamed the file on disk
 * and unlinked the old path, so an open tab at the old path never actually
 * followed the rename. See BT-3285 and
 * docs/ADR/0114-class-and-method-rename.md's LSP section.
 */
export interface DocumentMovedParams {
  oldUri: string;
  newUri: string;
}

/**
 * One open tab's URI and view column — the subset of `vscode.Tab` that
 * `planDocumentRetarget` needs, expressed without a `vscode` import so this
 * module (and its test) stay independent of the `vscode` API, which is
 * unavailable outside the extension host (see `editors/vscode/src/__tests__`,
 * which only exercises `vscode`-free modules like this one and
 * `textUtils.ts`). Unlike a *visible* editor, a tab exists even when it is a
 * background tab in an unfocused split.
 */
export interface TabSnapshot {
  uri: string;
  viewColumn: number;
}

/**
 * One visible editor's URI, view column, and selection — the subset of
 * `vscode.TextEditor` that `planDocumentRetarget` needs, in the same
 * `vscode`-free shape as `TabSnapshot`.
 */
export interface VisibleEditorSnapshot {
  uri: string;
  viewColumn: number;
  selection: {
    startLine: number;
    startCharacter: number;
    endLine: number;
    endCharacter: number;
  };
}

/**
 * A per-tab plan for `extension.ts`'s `handleDocumentMoved` to follow when
 * reopening the moved file: which view column to reopen it in, and which
 * selection to restore there (`undefined` when none was captured).
 */
export interface RetargetPlan {
  viewColumn: number;
  selection: VisibleEditorSnapshot["selection"] | undefined;
}

/**
 * Pure decision logic for `beamtalk-lsp/documentMoved` handling: given every
 * open tab showing the old path (there can be more than one — the same file
 * open in a split view, or a background tab that isn't the active one in its
 * group) and the *visible* editors among them, returns one retarget plan per
 * matching tab, each carrying the cursor/scroll position captured from a
 * visible editor in that tab's view column when there is one (BT-3285's
 * "preserving view state ... where practical") — `undefined` for a tab that
 * wasn't currently visible, since there is no selection to capture for it.
 * Keying by view column (rather than pairing tabs and editors positionally)
 * is deliberate: `vscode.window.visibleTextEditors` only ever contains the
 * *active* tab per view column, so a view column's captured selection
 * belongs to whichever of its same-path tabs happens to be showing, not
 * necessarily the one at a given array index.
 */
export function planDocumentRetarget(
  matchingTabs: readonly TabSnapshot[],
  visibleEditors: readonly VisibleEditorSnapshot[],
  params: DocumentMovedParams
): RetargetPlan[] {
  const selectionByViewColumn = new Map<number, VisibleEditorSnapshot["selection"]>();
  for (const editor of visibleEditors) {
    if (editor.uri === params.oldUri) {
      selectionByViewColumn.set(editor.viewColumn, editor.selection);
    }
  }

  return matchingTabs
    .filter((tab) => tab.uri === params.oldUri)
    .map((tab) => ({
      viewColumn: tab.viewColumn,
      selection: selectionByViewColumn.get(tab.viewColumn),
    }));
}

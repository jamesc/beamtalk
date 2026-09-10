// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import type { ClassInfo } from "./workspaceClient";

/**
 * Opens a stdlib class's source via the `beamtalk-stdlib://` virtual URI
 * scheme — `openStdlibDocumentForClass` in extension.ts, which needs the LSP
 * `LanguageClient` this leaf module has no access to, hence the
 * injected-function shape rather than a direct import (extension.ts already
 * imports from workspaceTreeView.ts, so a reverse import would cycle).
 */
export type StdlibDocumentOpener = (
  classInfo: ClassInfo
) => Promise<vscode.TextDocument | undefined>;

/**
 * Outcome of `resolveClassDocument`: either the opened document, or which of
 * the two steps came up empty — a real `source_file` that failed to open
 * (with the underlying error), or no `source_file` and no stdlib fallback
 * available (or the fallback itself came up empty). Callers compose their
 * own user-facing report from this: a navigation command shows a message,
 * a hover tooltip lookup falls back silently.
 */
export type ClassDocumentResult =
  | { readonly kind: "opened"; readonly document: vscode.TextDocument }
  | { readonly kind: "source-file-error"; readonly sourceFile: string; readonly error: string }
  | { readonly kind: "no-source" };

/**
 * Resolve the document a class's source lives in: the real `source_file`
 * first, falling back to `stdlibOpener` (pass null where none is available)
 * for the compiled-in stdlib classes the runtime never records a real
 * `source_file` for. The one place this two-step rule lives — every sidebar
 * navigation command (`beamtalk.openClassSource`, `navigateToMethod`,
 * `navigateToStateVar` in extension.ts) and every hover/doc-comment tooltip
 * lookup (`WorkspaceTreeDataProvider._resolveClassDocument` in
 * workspaceTreeView.ts) goes through this instead of each reimplementing it
 * (see CLAUDE.md's no-duplicate-implementations rule).
 */
export async function resolveClassDocument(
  classInfo: ClassInfo,
  stdlibOpener: StdlibDocumentOpener | null
): Promise<ClassDocumentResult> {
  const sourceFile = classInfo.source_file;
  if (sourceFile && sourceFile !== "unknown") {
    try {
      const document = await vscode.workspace.openTextDocument(vscode.Uri.file(sourceFile));
      return { kind: "opened", document };
    } catch (err) {
      const error = err instanceof Error ? err.message : String(err);
      return { kind: "source-file-error", sourceFile, error };
    }
  }
  if (!stdlibOpener) return { kind: "no-source" };
  const document = await stdlibOpener(classInfo).catch(() => undefined);
  return document ? { kind: "opened", document } : { kind: "no-source" };
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { aliasSourceUriString, parseAliasSourceUriPath } from "./textUtils";
import type { ClassInfo, TypeAliasInfo } from "./workspaceClient";

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

/**
 * Minimal shape of `WorkspaceClient.browseAliasSource` this module needs —
 * accepted as a parameter rather than importing the concrete `WorkspaceClient`
 * class, so this leaf module (like the rest of `documentResolution.ts`) stays
 * cheap to unit test with a plain `vi.fn()` instead of a full client mock.
 */
export type AliasSourceFetcher = (
  name: string,
  pkg?: string
) => Promise<{ content: string | null }>;

/** Outcome of `resolveAliasDocument`. Unlike `ClassDocumentResult`, there is
 * no `source-file-error` case — a `type` alias has no directly-openable real
 * file to fail against; every failure mode collapses to "nothing to show". */
export type AliasDocumentResult =
  | { readonly kind: "opened"; readonly document: vscode.TextDocument }
  | { readonly kind: "no-source" };

/**
 * Resolve a `type` alias's read-only source document via the
 * `beamtalk-alias://` virtual URI scheme (BT-3314/BT-3496/BT-3505) —
 * `openAliasSourceDocument` in extension.ts, extracted here (mirroring
 * `resolveClassDocument` above) so this exact code path — the one that
 * shipped two real, review-missed bugs (a double-decode `URIError`, then a
 * leading-`//`-path `UriError`) — is exercised in tests by a real
 * `vscode.Uri.parse` instead of only the pure `aliasSourceUriString`/
 * `parseAliasSourceUriPath` string functions in isolation.
 *
 * Calls `fetchAliasSource` directly first (rather than going straight to
 * `openTextDocument`) so a `content: null` result — always the case for a
 * stdlib/dependency-origin alias (no live source tree to resolve against
 * server-side), and possibly a project-origin one whose recorded file no
 * longer exists — is detected as a clean failure here: `openTextDocument` on
 * a `beamtalk-alias://` URI never rejects on its own, since the registered
 * `TextDocumentContentProvider` returns friendly placeholder text instead.
 */
export async function resolveAliasDocument(
  info: TypeAliasInfo,
  fetchAliasSource: AliasSourceFetcher
): Promise<AliasDocumentResult> {
  try {
    const { content } = await fetchAliasSource(info.name, info.package);
    if (content === null) return { kind: "no-source" };
  } catch {
    return { kind: "no-source" };
  }
  try {
    const uri = vscode.Uri.parse(aliasSourceUriString(info.name, info.package));
    const document = await vscode.workspace.openTextDocument(uri);
    return { kind: "opened", document };
  } catch {
    return { kind: "no-source" };
  }
}

/**
 * Content for a `beamtalk-alias://` URI, for the `TextDocumentContentProvider`
 * registered against that scheme (`AliasContentProvider` in extension.ts,
 * which owns the `vscode.Uri` → this function's plain-string-path call).
 * Extracted alongside `resolveAliasDocument` for the same reason: real
 * `vscode.Uri` parsing/decoding in the loop, under test.
 *
 * `fetchAliasSource` is null when there's no live workspace connection —
 * distinct from `fetchAliasSource` itself throwing (a request that reached a
 * connected workspace but failed), so the placeholder text can tell the two
 * apart for the reader.
 */
export async function aliasSourceContentFor(
  uri: vscode.Uri,
  fetchAliasSource: AliasSourceFetcher | null
): Promise<string> {
  const { name, pkg } = parseAliasSourceUriPath(uri.path);
  if (!fetchAliasSource) {
    return `// ${uri.toString()}\n// Not connected to a Beamtalk workspace.\n`;
  }
  try {
    const { content } = await fetchAliasSource(name, pkg);
    return content ?? `// Source not available for type alias \`${name}\`.\n`;
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return `// Failed to load type alias source for \`${name}\`.\n// ${message}\n`;
  }
}

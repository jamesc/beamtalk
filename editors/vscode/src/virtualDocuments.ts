// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { aliasSourceContentFor, resolveAliasDocument } from "./documentResolution";
import { state } from "./extensionState";
import { classNameToStdlibFilename } from "./textUtils";
import type { ClassInfo, TypeAliasInfo } from "./workspaceClient";

/**
 * TextDocumentContentProvider for `beamtalk-stdlib://` virtual URIs.
 *
 * Fetches content from the LSP server via the `beamtalk-lsp/fetchContent`
 * custom request. URIs must have the form `beamtalk-stdlib:///ClassName.bt`.
 */
export class StdlibContentProvider implements vscode.TextDocumentContentProvider {
  async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    if (!state.client) {
      return `// ${uri.toString()}\n// Language server is not connected. Install Beamtalk to enable stdlib navigation.\n`;
    }
    try {
      const result = await state.client.sendRequest<{ content: string }>(
        "beamtalk-lsp/fetchContent",
        {
          uri: uri.toString(),
        }
      );
      return result.content;
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      state.outputChannel?.warn(`Failed to fetch stdlib content for ${uri}: ${message}`);
      return `// Failed to load ${uri.toString()}\n// ${message}\n`;
    }
  }
}

/**
 * Open a stdlib class's source via the `beamtalk-stdlib://` virtual URI
 * scheme, for sidebar navigation commands. The runtime's `list-classes`/
 * `methods` ops never report a real `source_file` for compiled-in stdlib
 * classes (they simply don't track one) — this is the same fallback the LSP
 * already uses for Ctrl+click go-to-definition on a stdlib reference inside
 * a real file (`stdlibSourceDir`/sysroot auto-discovery), just entered from
 * a class name instead of a code reference.
 *
 * Calls `beamtalk-lsp/fetchContent` directly first (rather than going
 * straight to `openTextDocument`) so a wrong filename guess or a
 * disconnected LSP is detected as a clean failure here — `openTextDocument`
 * on a `beamtalk-stdlib://` URI never rejects on its own; `StdlibContentProvider`
 * deliberately returns friendly placeholder comment text instead, for
 * LSP-initiated navigation where a raw VS Code error tab would be worse.
 *
 * Returns undefined if the class isn't stdlib-origin, no filename guess is
 * possible, or the guessed file doesn't exist — callers should fall back to
 * their normal "source not available" handling.
 */
export async function openStdlibDocumentForClass(
  classInfo: ClassInfo
): Promise<vscode.TextDocument | undefined> {
  if (classInfo.source_origin !== "stdlib" || !state.client) return undefined;
  const uriString = `beamtalk-stdlib:///${classNameToStdlibFilename(classInfo.name)}`;
  try {
    await state.client.sendRequest<{ content: string }>("beamtalk-lsp/fetchContent", {
      uri: uriString,
    });
  } catch {
    return undefined;
  }
  try {
    return await vscode.workspace.openTextDocument(vscode.Uri.parse(uriString));
  } catch {
    return undefined;
  }
}

/** Bind `workspaceWsClient.browseAliasSource` into the shape `documentResolution.ts`'s
 * alias-resolution functions accept — null when there's no live workspace
 * connection, so they can tell that apart from a connected fetch that threw. */
function aliasSourceFetcher():
  | ((name: string, pkg?: string) => Promise<{ content: string | null }>)
  | null {
  const wsClient = state.workspaceWsClient;
  return wsClient ? (name, pkg) => wsClient.browseAliasSource(name, pkg) : null;
}

/**
 * TextDocumentContentProvider for `beamtalk-alias://` virtual URIs — the
 * read-only source view for a `type` alias declaration (BT-3314/BT-3496).
 * Unlike `StdlibContentProvider`, this goes through the workspace protocol's
 * `browse-alias-source` op (`workspaceWsClient`), not the LSP: a `type`
 * declaration erases entirely at compile time (no BEAM module), so there is
 * no compiled-module path for the LSP's `beamtalk-lsp/fetchContent` sysroot
 * discovery to key off of — the runtime resolves `AliasMetadata.source_file`
 * (a package-relative display path) against its own recorded state instead.
 *
 * The actual content logic lives in `aliasSourceContentFor`
 * (`documentResolution.ts`) so it's covered by tests using a real
 * `vscode.Uri.parse`/`.path` — see that function's doc for why.
 */
export class AliasContentProvider implements vscode.TextDocumentContentProvider {
  async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    return aliasSourceContentFor(uri, aliasSourceFetcher());
  }
}

/**
 * Open a `type` alias's read-only source view via the `beamtalk-alias://`
 * virtual URI scheme (BT-3314/BT-3496). Thin wiring around
 * `resolveAliasDocument` (`documentResolution.ts`), which owns the actual
 * existence-check-then-open logic — see that function's doc.
 *
 * Returns undefined when there's nothing to show — callers fall back to
 * their normal "source not available" handling.
 */
export async function openAliasSourceDocument(
  info: TypeAliasInfo
): Promise<vscode.TextDocument | undefined> {
  const fetchAliasSource = aliasSourceFetcher();
  if (!fetchAliasSource) return undefined;
  const result = await resolveAliasDocument(info, fetchAliasSource);
  return result.kind === "opened" ? result.document : undefined;
}

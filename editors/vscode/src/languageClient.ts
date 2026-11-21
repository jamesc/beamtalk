// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  RevealOutputChannelOn,
  type ServerOptions,
} from "vscode-languageclient/node";
import { type DocumentMovedParams, planDocumentRetarget } from "./documentMoved";
import { state } from "./extensionState";
import { resolveServerPath, resolveStdlibSourceDir } from "./serverDiscovery";

/** Method name for the server's `beamtalk-lsp/documentMoved` notification (BT-3285). */
const DOCUMENT_MOVED_NOTIFICATION = "beamtalk-lsp/documentMoved";

/**
 * Handles the server's `beamtalk-lsp/documentMoved` notification (BT-3285):
 * sent for a `renameTo:`/`moveClass:to:` flush at the point the (now-removed)
 * `workspace/applyEdit` `RenameFile` op used to fire — that op silently
 * no-ops in VS Code once the runtime has already renamed the file on disk
 * and unlinked the old path (see docs/ADR/0114-class-and-method-rename.md's
 * LSP section). Closes every open tab at `params.oldUri` (there can be more
 * than one — the same file open in a split view, or a background tab that
 * isn't the active one in its group) and reopens `params.newUri` once per
 * closed tab, in that tab's own view column, restoring the cursor/scroll
 * position captured from a *visible* editor in that column where one was
 * captured (`planDocumentRetarget`, `documentMoved.ts`) — BT-3285's
 * "preserving view state ... where practical".
 */
async function handleDocumentMoved(params: DocumentMovedParams): Promise<void> {
  const oldUri = vscode.Uri.parse(params.oldUri).toString();
  const newUri = vscode.Uri.parse(params.newUri);

  const matchingTabs: vscode.Tab[] = [];
  for (const group of vscode.window.tabGroups.all) {
    for (const tab of group.tabs) {
      if (tab.input instanceof vscode.TabInputText && tab.input.uri.toString() === oldUri) {
        matchingTabs.push(tab);
      }
    }
  }
  if (matchingTabs.length === 0) {
    return;
  }

  const plans = planDocumentRetarget(
    matchingTabs.map((tab) => ({ uri: oldUri, viewColumn: tab.group.viewColumn })),
    vscode.window.visibleTextEditors
      .filter(
        (editor): editor is vscode.TextEditor & { viewColumn: vscode.ViewColumn } =>
          editor.document.uri.toString() === oldUri && editor.viewColumn !== undefined
      )
      .map((editor) => ({
        uri: oldUri,
        viewColumn: editor.viewColumn,
        selection: {
          startLine: editor.selection.start.line,
          startCharacter: editor.selection.start.character,
          endLine: editor.selection.end.line,
          endCharacter: editor.selection.end.character,
        },
      })),
    { oldUri, newUri: newUri.toString() }
  );

  // Close each matching tab individually (rather than one batched
  // `tabGroups.close(matchingTabs, ...)` call) so a `false` result can be
  // attributed to the specific tab it belongs to — closing as a single
  // array only reports one boolean for the whole batch, which loses which
  // tab it refers to. `preserveFocus: true` keeps focus from jumping around
  // mid-retarget; the explicit `showTextDocument` calls below focus the
  // reopened tabs instead. A tab's close can come back `false` when the
  // user is prompted to save a dirty file at `oldUri` and cancels — in that
  // case the tab is left open as-is rather than also opening `newUri`
  // alongside it, which would otherwise leave a stale dirty tab and a fresh
  // tab side by side in the same view column.
  for (const [index, tab] of matchingTabs.entries()) {
    const closed = await vscode.window.tabGroups.close(tab, true);
    if (!closed) {
      continue;
    }
    const plan = plans[index];
    await vscode.window.showTextDocument(newUri, {
      viewColumn: plan.viewColumn,
      selection: plan.selection
        ? new vscode.Range(
            new vscode.Position(plan.selection.startLine, plan.selection.startCharacter),
            new vscode.Position(plan.selection.endLine, plan.selection.endCharacter)
          )
        : undefined,
      preview: false,
    });
  }
}

/** Creates the LSP client and starts it. */
export async function startClient(context: vscode.ExtensionContext): Promise<void> {
  // Create the output channel early so discovery failures are visible.
  state.outputChannel ??= vscode.window.createOutputChannel("Beamtalk Language Server", {
    log: true,
  });
  state.traceOutputChannel ??= vscode.window.createOutputChannel("Beamtalk Language Server Trace", {
    log: true,
  });

  const resolved = await resolveServerPath(context);

  if (!resolved) {
    state.outputChannel.warn(
      "Could not find beamtalk-lsp. Searched: beamtalk.server.path setting, " +
        "sysroot via `beamtalk --print-sysroot`, and PATH. " +
        'Set "beamtalk.server.path" in settings, ensure `beamtalk-lsp` is on the extension host PATH, ' +
        "or ensure `beamtalk` is on the extension host PATH for sysroot discovery."
    );
    const action = await vscode.window.showWarningMessage(
      'Could not find beamtalk-lsp. Set "beamtalk.server.path" in settings, ensure `beamtalk-lsp` is on the extension host PATH, or ensure `beamtalk` is on PATH for sysroot discovery.',
      "Open Settings",
      "Install Beamtalk"
    );
    if (action === "Open Settings") {
      void vscode.commands.executeCommand("workbench.action.openSettings", "beamtalk.server.path");
    } else if (action === "Install Beamtalk") {
      void vscode.env.openExternal(
        vscode.Uri.parse("https://github.com/jamesc/beamtalk#installation")
      );
    }
    return;
  }

  const serverPath = resolved.serverPath;

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  if (resolved.warning) {
    state.outputChannel.warn(resolved.warning);
  }

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "beamtalk" },
      { scheme: "untitled", language: "beamtalk" },
      { scheme: "beamtalk-stdlib", language: "beamtalk" },
    ],
    outputChannel: state.outputChannel,
    traceOutputChannel: state.traceOutputChannel,
    revealOutputChannelOn: RevealOutputChannelOn.Never,
    initializationOptions: {
      stdlibSourceDir: resolveStdlibSourceDir(),
    },
  };

  state.client = new LanguageClient(
    "beamtalk",
    "Beamtalk Language Server",
    serverOptions,
    clientOptions
  );

  // Auto-restart up to 5 times on crash before giving up.
  state.client.clientOptions.errorHandler = state.client.createDefaultErrorHandler(5);

  state.outputChannel.info(`Starting language server: ${serverPath}`);

  try {
    await state.client.start();
    state.outputChannel.info("Language server started successfully");
    // BT-3285: retarget an open tab across a `renameTo:`/`moveClass:to:`
    // flush — see `handleDocumentMoved`'s doc for why this notification
    // exists instead of relying on `workspace/applyEdit`'s `RenameFile` op.
    state.client.onNotification(DOCUMENT_MOVED_NOTIFICATION, (params: DocumentMovedParams) => {
      handleDocumentMoved(params).catch((err) => {
        const message = err instanceof Error ? err.message : String(err);
        state.outputChannel?.warn(`Failed to handle documentMoved notification: ${message}`);
      });
    });
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    state.outputChannel.error(`Failed to start language server: ${message}`);
    vscode.window.showErrorMessage(
      `Failed to start Beamtalk language server (${serverPath}): ${message}`
    );
    state.client = undefined;
  }
}

// ─── Workspace ID derivation ─────────────────────────────────────────────────

/** Registers `beamtalk.restartServer`. */
export function registerServerCommands(context: vscode.ExtensionContext): void {
  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.restartServer", async () => {
      state.outputChannel?.info("Restarting language server…");
      if (state.client) {
        await state.client.stop();
        state.client = undefined;
      }
      await startClient(context);
    })
  );
}

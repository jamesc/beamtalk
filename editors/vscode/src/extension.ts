// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { state } from "./extensionState";
import { InspectorPanel } from "./inspectorPanel";
import { AliasContentProvider, StdlibContentProvider } from "./virtualDocuments";
import { BeamtalkTaskProvider } from "./taskProvider";
import { registerServerCommands, startClient } from "./languageClient";
import { registerLogUi } from "./logUi";
import { registerNavigationCommands } from "./navigationCommands";
import { setupAutoConnect } from "./workspaceConnection";
import { registerWorkspaceExplorer } from "./workspaceExplorer";
import { registerReplCommands, setReplRunning, setupTerminalMonitoring } from "./repl";

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  // Reset context state on each activation — VS Code persists setContext values
  // across extension host restarts, so we must re-initialise on every activation.
  setReplRunning(false);

  const stdlibProvider = new StdlibContentProvider();
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("beamtalk-stdlib", stdlibProvider)
  );

  const aliasProvider = new AliasContentProvider();
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("beamtalk-alias", aliasProvider)
  );

  context.subscriptions.push(
    vscode.tasks.registerTaskProvider("beamtalk", new BeamtalkTaskProvider())
  );

  registerServerCommands(context);
  registerLogUi(context);
  registerWorkspaceExplorer(context);
  registerNavigationCommands(context);
  registerReplCommands(context);

  // ─── Auto-connect + terminal monitoring (BT-1025) ──────────────────────────

  setupTerminalMonitoring(context);
  setupAutoConnect(context);

  await startClient(context);
}

export function deactivate(): Thenable<void> | undefined {
  const stop = state.client?.stop();
  if (state.portDebounceTimer !== null) {
    clearTimeout(state.portDebounceTimer);
    state.portDebounceTimer = null;
  }
  // Close all inspector panels before disposing the client so they don't
  // attempt to re-fetch after the connection is gone.
  InspectorPanel.disposeAll();
  // Providers are disposed via context.subscriptions; dispose() is idempotent
  // so no double-dispose risk, but we detach the client first to ensure no
  // late push events fire into disposed providers.
  state.workspaceWsClient?.dispose();
  state.workspaceWsClient = undefined;
  state.replTerminal = undefined;
  state.workspaceTreeProvider?.setClient(null);
  state.workspaceTreeProvider = undefined;
  state.transcriptViewProvider?.setClient(null);
  state.transcriptViewProvider = undefined;
  state.logLevelStatusBarItem?.dispose();
  state.logLevelStatusBarItem = undefined;
  state.logOutputChannel?.dispose();
  state.outputChannel?.dispose();
  state.traceOutputChannel?.dispose();
  state.logOutputChannel = undefined;
  state.outputChannel = undefined;
  state.traceOutputChannel = undefined;
  return stop;
}

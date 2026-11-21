// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import type * as vscode from "vscode";
import type { LanguageClient } from "vscode-languageclient/node";
import type { TranscriptViewProvider } from "./transcriptView";
import type { WorkspaceClient } from "./workspaceClient";
import type { WorkspaceTreeDataProvider } from "./workspaceTreeView";

/**
 * The extension host's shared mutable state (BT-5092).
 *
 * These were module-level `let`s in `extension.ts`. They live in one object so
 * the activation, language-client, workspace-connection, REPL and command
 * modules can share them without circular imports, and so tests can read and
 * reset them.
 */
export interface ExtensionState {
  /** The language-server client, once started. */
  client: LanguageClient | undefined;
  outputChannel: vscode.LogOutputChannel | undefined;
  traceOutputChannel: vscode.LogOutputChannel | undefined;
  /** Output channel for live runtime log streaming (BT-1433). */
  logOutputChannel: vscode.OutputChannel | undefined;
  /** Status bar item showing / changing the active log level. */
  logLevelStatusBarItem: vscode.StatusBarItem | undefined;
  workspaceTreeProvider: WorkspaceTreeDataProvider | undefined;
  transcriptViewProvider: TranscriptViewProvider | undefined;
  /** The active WorkspaceClient, set when a workspace port file is detected. */
  workspaceWsClient: WorkspaceClient | undefined;
  /** The integrated terminal running `beamtalk repl`, if started by this extension. */
  replTerminal: vscode.Terminal | undefined;
  /** True while `beamtalk repl` is actively running inside replTerminal. */
  replRunning: boolean;
  /** Debounce timer for port-file reads. */
  portDebounceTimer: ReturnType<typeof setTimeout> | null;
}

export const state: ExtensionState = {
  client: undefined,
  outputChannel: undefined,
  traceOutputChannel: undefined,
  logOutputChannel: undefined,
  logLevelStatusBarItem: undefined,
  workspaceTreeProvider: undefined,
  transcriptViewProvider: undefined,
  workspaceWsClient: undefined,
  replTerminal: undefined,
  replRunning: false,
  portDebounceTimer: null,
};

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { state } from "./extensionState";
import { registerLogLevelSync } from "./logLevelSync";

const LOG_LEVELS = [
  "debug",
  "info",
  "notice",
  "warning",
  "error",
  "critical",
  "alert",
  "emergency",
] as const;

type LogLevel = (typeof LOG_LEVELS)[number];

/**
 * The Beamtalk Logs output channel, the log-level status bar item and the
 * `beamtalk.setLogLevel` / `beamtalk.showLogs` commands (BT-1433).
 */
export function registerLogUi(context: vscode.ExtensionContext): void {
  // ─── Beamtalk Logs output channel + log level picker (BT-1433) ────────────

  state.logOutputChannel ??= vscode.window.createOutputChannel("Beamtalk Logs");

  state.logLevelStatusBarItem?.dispose();
  state.logLevelStatusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    50
  );
  state.logLevelStatusBarItem.command = "beamtalk.setLogLevel";
  state.logLevelStatusBarItem.tooltip = "Beamtalk log level — click to change";
  context.subscriptions.push(state.logLevelStatusBarItem);

  const updateLogLevelStatusBar = (level: string): void => {
    if (state.logLevelStatusBarItem) {
      state.logLevelStatusBarItem.text = `$(filter) ${level}`;
      state.logLevelStatusBarItem.show();
    }
  };

  {
    const config = vscode.workspace.getConfiguration("beamtalk");
    updateLogLevelStatusBar(config.get<string>("logs.level", "warning"));
  }

  // Keep the status bar and the server-side log filter in sync when the
  // setting is changed via Settings UI (BT-5082).
  context.subscriptions.push(
    registerLogLevelSync(
      vscode.workspace,
      () => state.workspaceWsClient,
      updateLogLevelStatusBar,
      (msg) => state.outputChannel?.warn(msg)
    )
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.setLogLevel", async () => {
      const config = vscode.workspace.getConfiguration("beamtalk");
      const current = config.get<string>("logs.level", "warning");
      const picked = await vscode.window.showQuickPick(
        LOG_LEVELS.map((l) => ({
          label: l,
          description: l === current ? "current" : undefined,
        })),
        { title: "Beamtalk: Set Log Level", placeHolder: "Select minimum log level" }
      );
      if (!picked) return;
      const newLevel = picked.label as LogLevel;
      await config.update("logs.level", newLevel, vscode.ConfigurationTarget.Global);
      // The onDidChangeConfiguration handler above updates the status bar and re-subscribes.
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.showLogs", () => {
      state.logOutputChannel?.show();
    })
  );
}

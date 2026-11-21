// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { workspacesBaseDir } from "./beamtalkRoot";
import { state } from "./extensionState";
import { InspectorPanel } from "./inspectorPanel";
import { type LogEntry, WorkspaceClient } from "./workspaceClient";
import { computeWorkspaceId } from "./workspaceId";
import { findProjectRoot } from "./projectRoot";
import { setReplRunning } from "./repl";

/**
 * Format a structured log entry for human-readable display in the Output panel.
 *
 * Example output:
 *   [14:23:01.234] [info] [runtime] beamtalk_supervisor:startChild/2 — Starting child
 *   [14:23:01.235] [debug] [Counter >> increment] incrementing
 */
function formatLogEntry(entry: LogEntry): string {
  // Extract time portion (HH:MM:SS.mmm) from ISO timestamp
  let timePart = "";
  if (entry.time && entry.time !== "unknown") {
    const tIdx = entry.time.indexOf("T");
    if (tIdx >= 0) {
      timePart = entry.time.slice(tIdx + 1).replace("Z", "");
    } else {
      timePart = entry.time;
    }
  }

  // Build the origin tag: class >> selector if available, with domain as context.
  // Falls back to domain alone, then mfa.
  let origin = "";
  if (entry.class) {
    origin = entry.selector ? `${entry.class} >> ${entry.selector}` : entry.class;
  } else if (entry.domain) {
    origin = entry.domain;
  } else if (entry.mfa) {
    origin = entry.mfa;
  }

  const parts: string[] = [];
  if (timePart) parts.push(`[${timePart}]`);
  parts.push(`[${entry.level}]`);
  if (origin) parts.push(`[${origin}]`);
  parts.push(entry.msg);

  return parts.join(" ");
}

// ─── Auto-connect via port file watcher ──────────────────────────────────────

/**
 * Connect the WorkspaceClient to the workspace identified by `workspaceId`.
 * Disposes any existing connection first.
 */
function connectWorkspace(workspaceId: string): void {
  // Always dispose the previous connection — even if the workspace ID matches,
  // the port number may have changed (workspace restart overwrites the port file).
  if (state.workspaceWsClient) {
    state.workspaceWsClient.dispose();
    state.workspaceWsClient = undefined;
    state.workspaceTreeProvider?.setClient(null);
    state.transcriptViewProvider?.setClient(null);
    InspectorPanel.setClient(null);
  }

  let newClient: WorkspaceClient;
  try {
    newClient = WorkspaceClient.fromFiles(workspaceId);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    state.outputChannel?.warn(
      `Failed to create WorkspaceClient for workspace ${workspaceId}: ${msg}`
    );
    return;
  }

  state.workspaceWsClient = newClient;
  state.workspaceTreeProvider?.setClient(newClient);
  state.transcriptViewProvider?.setClient(newClient);
  InspectorPanel.setClient(newClient);

  // Close inspector panels for actors that have stopped.
  // Stream log events to the Beamtalk Logs output channel (BT-1433).
  newClient.onPush((event) => {
    if (event.channel === "actors" && event.event === "stopped") {
      InspectorPanel.notifyActorStopped(event.data.pid);
    } else if (event.channel === "logs" && event.event === "entry") {
      state.logOutputChannel?.appendLine(formatLogEntry(event.data));
    }
  });

  // Surface terminal auth failures (BT-4373) — e.g. a protocol mismatch
  // between this extension and the workspace node. The client won't retry,
  // so without this the workspace views would just stay empty.
  newClient.onAuthError((message) => {
    state.outputChannel?.error(`Workspace ${workspaceId}: ${message}`);
    void vscode.window.showErrorMessage(`Beamtalk workspace connection refused: ${message}`);
  });

  // Subscribe to log streaming once connected (BT-1433).
  newClient.onConnectionChange((connectionState) => {
    if (connectionState === "connected") {
      const config = vscode.workspace.getConfiguration("beamtalk");
      const level = config.get<string>("logs.level", "warning");
      newClient.subscribeLogs(level).catch((err) => {
        state.outputChannel?.warn(
          `Failed to subscribe to logs: ${err instanceof Error ? err.message : String(err)}`
        );
      });
    }
  });

  newClient.connect();
  state.outputChannel?.info(`Connected to workspace: ${workspaceId}`);
}

/** Disconnect from the current workspace and show disconnected state. */
function disconnectWorkspace(reason: "port-removed"): void {
  if (!state.workspaceWsClient) {
    return;
  }

  const id = state.workspaceWsClient.id;
  state.workspaceWsClient.dispose();
  state.workspaceWsClient = undefined;
  state.workspaceTreeProvider?.setClient(null);
  state.transcriptViewProvider?.setClient(null);
  InspectorPanel.setClient(null);

  setReplRunning(false);
  state.outputChannel?.info(`Disconnected from workspace ${id}: ${reason}`);

  if (reason === "port-removed") {
    void vscode.window
      .showInformationMessage("Beamtalk workspace stopped.", "Restart Beamtalk Session")
      .then((action) => {
        if (action === "Restart Beamtalk Session") {
          void vscode.commands.executeCommand("beamtalk.startRepl");
        }
      });
  }
}

/**
 * Called when a port file appears or changes (workspace started or restarted).
 *
 * Debounces the read by a short delay — the Erlang runtime writes the port
 * and cookie files as separate operations, so the cookie may not exist yet
 * when the port file watcher fires.
 */

export async function handlePortFileAppeared(portUri: vscode.Uri): Promise<void> {
  const config = vscode.workspace.getConfiguration("beamtalk");
  if (!config.get<boolean>("workspace.autoConnect", true)) {
    return;
  }

  // The workspace ID is the directory name containing the port file.
  const workspaceId = path.basename(path.dirname(portUri.fsPath));

  // Only connect if this workspace matches the open project.
  const projectRoot = findProjectRoot();
  if (!projectRoot) {
    return;
  }
  const expectedId = computeWorkspaceId(projectRoot);
  if (!expectedId || expectedId !== workspaceId) {
    return;
  }

  // Debounce: wait briefly for the cookie file to be written before reading.
  if (state.portDebounceTimer !== null) {
    clearTimeout(state.portDebounceTimer);
  }
  state.portDebounceTimer = setTimeout(() => {
    state.portDebounceTimer = null;
    connectWorkspace(workspaceId);
  }, 200);
}

/** Called when a port file is removed (workspace stopped or timed out). */
function handlePortFileRemoved(portUri: vscode.Uri): void {
  const workspaceId = path.basename(path.dirname(portUri.fsPath));
  if (state.workspaceWsClient?.id !== workspaceId) {
    return;
  }
  disconnectWorkspace("port-removed");
}

/**
 * Register a file system watcher for workspace port files and trigger
 * an immediate connect if a port file already exists.
 */
export function setupAutoConnect(context: vscode.ExtensionContext): void {
  const workspacesDir = workspacesBaseDir();
  if (workspacesDir === undefined) {
    state.outputChannel?.warn(
      "Auto-connect disabled: cannot determine the Beamtalk home directory"
    );
    return;
  }

  // Ensure the directory exists so the FileSystemWatcher has something to watch.
  // On a fresh install, ~/.beamtalk/workspaces/ doesn't exist yet and inotify
  // (Linux) / FSEvents (macOS) can't watch a non-existent path.
  try {
    fs.mkdirSync(workspacesDir, { recursive: true });
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    state.outputChannel?.warn(`Auto-connect disabled: cannot create ${workspacesDir}: ${msg}`);
    return;
  }

  const pattern = new vscode.RelativePattern(vscode.Uri.file(workspacesDir), "*/port");
  const watcher = vscode.workspace.createFileSystemWatcher(pattern);
  context.subscriptions.push(watcher);

  context.subscriptions.push(watcher.onDidCreate((uri) => void handlePortFileAppeared(uri)));
  // onDidChange covers workspace restarts: the Erlang runtime overwrites the port
  // file in place, which fires onChange (not onCreate). We need to reconnect with
  // the new port number.
  context.subscriptions.push(watcher.onDidChange((uri) => void handlePortFileAppeared(uri)));
  context.subscriptions.push(watcher.onDidDelete((uri) => handlePortFileRemoved(uri)));

  // Check for a pre-existing port file (workspace already running at activation).
  const projectRoot = findProjectRoot();
  if (projectRoot) {
    const id = computeWorkspaceId(projectRoot);
    if (id) {
      const portPath = path.join(workspacesDir, id, "port");
      if (fs.existsSync(portPath)) {
        void handlePortFileAppeared(vscode.Uri.file(portPath));
      }
    }
  }
}

// ─── Start REPL command ───────────────────────────────────────────────────────

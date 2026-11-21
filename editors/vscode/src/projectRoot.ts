// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";

/**
 * Find the project root for the current VS Code workspace.
 * Prefers a workspace folder containing `beamtalk.toml`; falls back to the
 * first workspace folder so the Workspace Explorer connects even in projects
 * that haven't created a beamtalk.toml yet (e.g. when just running the REPL).
 * The workspace ID hash check in handlePortFileAppeared still guards against
 * connecting to an unrelated workspace.
 */
export function findProjectRoot(): string | null {
  const folders = vscode.workspace.workspaceFolders;
  if (!folders || folders.length === 0) {
    return null;
  }
  for (const folder of folders) {
    const tomlPath = path.join(folder.uri.fsPath, "beamtalk.toml");
    if (fs.existsSync(tomlPath)) {
      return folder.uri.fsPath;
    }
  }
  return folders[0].uri.fsPath;
}

/**
 * Expand a literal `${workspaceFolder}` placeholder in a settings value.
 *
 * VS Code only expands `${workspaceFolder}` in launch.json/tasks.json, not
 * in arbitrary settings values, so callers that accept paths like
 * `${workspaceFolder}/target/debug/beamtalk` must expand it themselves.
 * Returns `value` unchanged when no project root is found.
 */
export function expandWorkspaceFolder(value: string, projectRoot: string | null): string {
  return projectRoot ? value.replace(/\$\{workspaceFolder\}/g, projectRoot) : value;
}

// ─── Log output formatting (BT-1433) ─────────────────────────────────────────

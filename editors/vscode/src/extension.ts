// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

/**
 * Resolve the path to `beamtalk-lsp`.
 *
 * Resolution order:
 * 1. User-configured override (`beamtalk.server.path`)
 * 2. Bundled binary shipped inside the extension (`bin/beamtalk-lsp[.exe]`)
 * 3. Fallback to bare command name (requires PATH)
 */
function resolveServerPath(context: vscode.ExtensionContext): string {
  const config = vscode.workspace.getConfiguration("beamtalk");
  const override = config.get<string>("server.path", "");

  // 1. Explicit user override
  if (override && override !== "beamtalk-lsp") {
    return override;
  }

  // 2. Bundled binary
  const ext = process.platform === "win32" ? ".exe" : "";
  const bundled = path.join(
    context.extensionPath,
    "bin",
    `beamtalk-lsp${ext}`
  );
  if (fs.existsSync(bundled)) {
    return bundled;
  }

  // 3. Fall back to PATH
  return "beamtalk-lsp";
}

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  const serverPath = resolveServerPath(context);

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "beamtalk" },
      { scheme: "untitled", language: "beamtalk" },
    ],
  };

  client = new LanguageClient(
    "beamtalk",
    "Beamtalk Language Server",
    serverOptions,
    clientOptions
  );

  try {
    await client.start();
  } catch (error) {
    const message =
      error instanceof Error ? error.message : String(error);
    vscode.window.showErrorMessage(
      `Failed to start Beamtalk language server (${serverPath}): ${message}`
    );
    client = undefined;
  }
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

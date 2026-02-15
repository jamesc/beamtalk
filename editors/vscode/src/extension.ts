// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  const config = vscode.workspace.getConfiguration("beamtalk");
  const serverPath = config.get<string>("server.path", "beamtalk-lsp");

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

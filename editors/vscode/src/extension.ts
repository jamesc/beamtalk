// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;
let outputChannel: vscode.LogOutputChannel | undefined;
let traceOutputChannel: vscode.LogOutputChannel | undefined;

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
  // The old default was "beamtalk-lsp" (bare PATH lookup). Treat that the
  // same as empty so existing configs migrate to the bundled binary.
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

/** Creates the LSP client and starts it. */
async function startClient(
  context: vscode.ExtensionContext
): Promise<void> {
  const serverPath = resolveServerPath(context);

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  outputChannel ??= vscode.window.createOutputChannel(
    "Beamtalk Language Server",
    { log: true }
  );
  traceOutputChannel ??= vscode.window.createOutputChannel(
    "Beamtalk Language Server Trace",
    { log: true }
  );

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "beamtalk" },
      { scheme: "untitled", language: "beamtalk" },
    ],
    outputChannel,
    traceOutputChannel,
    revealOutputChannelOn: RevealOutputChannelOn.Never,
  };

  client = new LanguageClient(
    "beamtalk",
    "Beamtalk Language Server",
    serverOptions,
    clientOptions
  );

  // Auto-restart up to 5 times on crash before giving up.
  client.clientOptions.errorHandler =
    client.createDefaultErrorHandler(5);

  outputChannel.info(`Starting language server: ${serverPath}`);

  try {
    await client.start();
    outputChannel.info("Language server started successfully");
  } catch (error) {
    const message =
      error instanceof Error ? error.message : String(error);
    outputChannel.error(`Failed to start language server: ${message}`);
    vscode.window.showErrorMessage(
      `Failed to start Beamtalk language server (${serverPath}): ${message}`
    );
    client = undefined;
  }
}

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  context.subscriptions.push(
    vscode.commands.registerCommand(
      "beamtalk.restartServer",
      async () => {
        outputChannel?.info("Restarting language server…");
        if (client) {
          await client.stop();
          client = undefined;
        }
        await startClient(context);
      }
    )
  );

  await startClient(context);
}

export function deactivate(): Thenable<void> | undefined {
  const stop = client?.stop();
  outputChannel?.dispose();
  traceOutputChannel?.dispose();
  outputChannel = undefined;
  traceOutputChannel = undefined;
  return stop;
}

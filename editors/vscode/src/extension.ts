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

type ResolvedServerPath = {
  serverPath: string;
  warning?: string;
};

function withPlatformExecutable(pathLike: string): string {
  if (process.platform !== "win32") {
    return pathLike;
  }

  return pathLike.endsWith(".exe") ? pathLike : `${pathLike}.exe`;
}

function fileExists(pathLike: string): boolean {
  return fs.existsSync(pathLike) || fs.existsSync(withPlatformExecutable(pathLike));
}

/**
 * Resolve the path to `beamtalk-lsp`.
 *
 * Resolution order:
 * 1. User-configured override (`beamtalk.server.path`)
 * 2. Bundled binary shipped inside the extension (`bin/beamtalk-lsp[.exe]`)
 * 3. Fallback to bare command name (requires PATH)
 */
function resolveServerPath(context: vscode.ExtensionContext): ResolvedServerPath {
  const config = vscode.workspace.getConfiguration("beamtalk");
  const override = config.get<string>("server.path", "").trim();
  let warning: string | undefined;

  // 1. Explicit user override
  // The old default was "beamtalk-lsp" (bare PATH lookup). Treat that the
  // same as empty so existing configs migrate to the bundled binary.
  if (override && override !== "beamtalk-lsp") {
    const isPathLike =
      override.includes("/") ||
      override.includes("\\") ||
      override.startsWith(".") ||
      path.isAbsolute(override);

    if (!isPathLike) {
      return { serverPath: override };
    }

    const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
    const candidates = [
      path.isAbsolute(override) ? override : undefined,
      workspaceRoot ? path.resolve(workspaceRoot, override) : undefined,
      path.resolve(context.extensionPath, override),
      path.resolve(override),
    ].filter((candidate): candidate is string => Boolean(candidate));

    const resolved = candidates.find((candidate) => fileExists(candidate));
    if (resolved) {
      return { serverPath: withPlatformExecutable(resolved) };
    }

    warning =
      `Configured beamtalk.server.path does not exist: ${override}. ` +
      "Falling back to bundled beamtalk-lsp (or PATH if bundled binary is missing).";
    // Path-like override was invalid: warn and fall through to bundled/PATH.
  }

  // 2. Bundled binary
  const ext = process.platform === "win32" ? ".exe" : "";
  const bundled = path.join(
    context.extensionPath,
    "bin",
    `beamtalk-lsp${ext}`
  );
  if (fs.existsSync(bundled)) {
    return { serverPath: bundled, warning };
  }

  // 3. Fall back to PATH
  return { serverPath: "beamtalk-lsp", warning };
}

/** Creates the LSP client and starts it. */
async function startClient(
  context: vscode.ExtensionContext
): Promise<void> {
  const resolved = resolveServerPath(context);
  const serverPath = resolved.serverPath || "beamtalk-lsp";

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

  if (resolved.warning) {
    outputChannel.warn(resolved.warning);
  }

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

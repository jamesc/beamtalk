// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { spawnSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  RevealOutputChannelOn,
  type ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;
let outputChannel: vscode.LogOutputChannel | undefined;
let traceOutputChannel: vscode.LogOutputChannel | undefined;

type ResolvedServerPath = {
  serverPath: string;
  warning?: string;
} | null;

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
 * Locate `beamtalk-lsp` by running `beamtalk --print-sysroot`.
 *
 * Returns the absolute path to `{sysroot}/bin/beamtalk-lsp[.exe]`, or null
 * if `beamtalk` is not on PATH or the binary is absent from the sysroot.
 */
function findLspViaSysroot(): string | null {
  try {
    const result = spawnSync("beamtalk", ["--print-sysroot"], {
      encoding: "utf8",
      timeout: 5000,
    });
    if (result.status !== 0 || !result.stdout?.trim()) {
      return null;
    }
    const sysroot = result.stdout.trim();
    const ext = process.platform === "win32" ? ".exe" : "";
    const lspPath = path.join(sysroot, "bin", `beamtalk-lsp${ext}`);
    return fs.existsSync(lspPath) ? lspPath : null;
  } catch {
    return null;
  }
}

/**
 * Resolve the path to `beamtalk-lsp`.
 *
 * Resolution order:
 * 1. User-configured override (`beamtalk.server.path`)
 * 2. Sysroot discovery via `beamtalk --print-sysroot` → `{sysroot}/bin/beamtalk-lsp`
 * 3. Bare `beamtalk-lsp` command name (requires PATH)
 *
 * Returns null if Beamtalk is not installed and no override is configured.
 */
function resolveServerPath(context: vscode.ExtensionContext): ResolvedServerPath {
  const config = vscode.workspace.getConfiguration("beamtalk");
  const override = config.get<string>("server.path", "").trim();
  let warning: string | undefined;

  // 1. Explicit user override
  if (override) {
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
      "Falling back to PATH discovery.";
    // Path-like override was invalid: warn and fall through.
  }

  // 2. Sysroot discovery via `beamtalk --print-sysroot`
  const sysrootLsp = findLspViaSysroot();
  if (sysrootLsp) {
    return { serverPath: sysrootLsp, warning };
  }

  // 3. Fall back to bare PATH lookup — only if beamtalk-lsp is available
  // (spawnSync will report error if not found, so we check upfront)
  const probe = spawnSync(withPlatformExecutable("beamtalk-lsp"), ["--version"], {
    encoding: "utf8",
    timeout: 3000,
  });
  if (!probe.error) {
    return { serverPath: "beamtalk-lsp", warning };
  }

  // Beamtalk not found
  return null;
}

/**
 * TextDocumentContentProvider for `beamtalk-stdlib://` virtual URIs.
 *
 * Fetches content from the LSP server via the `beamtalk-lsp/fetchContent`
 * custom request. URIs must have the form `beamtalk-stdlib:///ClassName.bt`.
 */
class StdlibContentProvider implements vscode.TextDocumentContentProvider {
  async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    if (!client) {
      return "";
    }
    try {
      const result = await client.sendRequest<{ content: string }>("beamtalk-lsp/fetchContent", {
        uri: uri.toString(),
      });
      return result.content;
    } catch (err) {
      outputChannel?.warn(`Failed to fetch stdlib content for ${uri}: ${err}`);
      return "";
    }
  }
}

/** Creates the LSP client and starts it. */
async function startClient(context: vscode.ExtensionContext): Promise<void> {
  const resolved = resolveServerPath(context);

  if (!resolved) {
    const action = await vscode.window.showInformationMessage(
      "Install Beamtalk to enable language features (hover, go-to-definition, diagnostics).",
      "Install Beamtalk"
    );
    if (action === "Install Beamtalk") {
      vscode.env.openExternal(vscode.Uri.parse("https://github.com/jamesc/beamtalk#installation"));
    }
    return;
  }

  const serverPath = resolved.serverPath;

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  outputChannel ??= vscode.window.createOutputChannel("Beamtalk Language Server", { log: true });
  traceOutputChannel ??= vscode.window.createOutputChannel("Beamtalk Language Server Trace", {
    log: true,
  });

  if (resolved.warning) {
    outputChannel.warn(resolved.warning);
  }

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "beamtalk" },
      { scheme: "untitled", language: "beamtalk" },
      { scheme: "beamtalk-stdlib", language: "beamtalk" },
    ],
    outputChannel,
    traceOutputChannel,
    revealOutputChannelOn: RevealOutputChannelOn.Never,
  };

  client = new LanguageClient("beamtalk", "Beamtalk Language Server", serverOptions, clientOptions);

  // Auto-restart up to 5 times on crash before giving up.
  client.clientOptions.errorHandler = client.createDefaultErrorHandler(5);

  outputChannel.info(`Starting language server: ${serverPath}`);

  try {
    await client.start();
    outputChannel.info("Language server started successfully");
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    outputChannel.error(`Failed to start language server: ${message}`);
    vscode.window.showErrorMessage(
      `Failed to start Beamtalk language server (${serverPath}): ${message}`
    );
    client = undefined;
  }
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const stdlibProvider = new StdlibContentProvider();
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("beamtalk-stdlib", stdlibProvider)
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.restartServer", async () => {
      outputChannel?.info("Restarting language server…");
      if (client) {
        await client.stop();
        client = undefined;
      }
      await startClient(context);
    })
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

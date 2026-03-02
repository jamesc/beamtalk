// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { execFile } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  RevealOutputChannelOn,
  type ServerOptions,
} from "vscode-languageclient/node";
import { WorkspaceTreeDataProvider } from "./workspaceTreeView";

let client: LanguageClient | undefined;
let outputChannel: vscode.LogOutputChannel | undefined;
let traceOutputChannel: vscode.LogOutputChannel | undefined;
let workspaceTreeProvider: WorkspaceTreeDataProvider | undefined;

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
 * Locate `beamtalk-lsp` by running `beamtalk --print-sysroot` asynchronously.
 *
 * Returns the absolute path to `{sysroot}/bin/beamtalk-lsp[.exe]`, or null
 * if `beamtalk` is not on PATH or the binary is absent from the sysroot.
 */
function findLspViaSysroot(): Promise<string | null> {
  return new Promise((resolve) => {
    execFile(
      "beamtalk",
      ["--print-sysroot"],
      { encoding: "utf8", timeout: 5000, windowsHide: true },
      (error, stdout) => {
        if (error || !stdout?.trim()) {
          resolve(null);
          return;
        }
        const sysroot = stdout.trim();
        const ext = process.platform === "win32" ? ".exe" : "";
        const lspPath = path.join(sysroot, "bin", `beamtalk-lsp${ext}`);
        resolve(fs.existsSync(lspPath) ? lspPath : null);
      }
    );
  });
}

/**
 * Check if `beamtalk-lsp` is available on PATH, asynchronously.
 */
function lspIsOnPath(): Promise<boolean> {
  return new Promise((resolve) => {
    execFile(
      withPlatformExecutable("beamtalk-lsp"),
      ["--version"],
      { timeout: 3000, windowsHide: true },
      (error) => resolve(!error)
    );
  });
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
async function resolveServerPath(context: vscode.ExtensionContext): Promise<ResolvedServerPath> {
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
      "Falling back to automatic discovery (sysroot, then PATH).";
    // Path-like override was invalid: warn and fall through.
  }

  // 2. Sysroot discovery via `beamtalk --print-sysroot`
  const sysrootLsp = await findLspViaSysroot();
  if (sysrootLsp) {
    return { serverPath: sysrootLsp, warning };
  }

  // 3. Fall back to bare PATH lookup — only if beamtalk-lsp is available
  if (await lspIsOnPath()) {
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
      return `// ${uri.toString()}\n// Language server is not connected. Install Beamtalk to enable stdlib navigation.\n`;
    }
    try {
      const result = await client.sendRequest<{ content: string }>("beamtalk-lsp/fetchContent", {
        uri: uri.toString(),
      });
      return result.content;
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      outputChannel?.warn(`Failed to fetch stdlib content for ${uri}: ${message}`);
      return `// Failed to load ${uri.toString()}\n// ${message}\n`;
    }
  }
}

/** Provides preconfigured build and test tasks for Beamtalk projects. */
class BeamtalkTaskProvider implements vscode.TaskProvider {
  provideTasks(): vscode.Task[] {
    const buildTask = new vscode.Task(
      { type: "beamtalk", task: "build" },
      vscode.TaskScope.Workspace,
      "build",
      "beamtalk",
      new vscode.ShellExecution("beamtalk", ["build", "."]),
      "$beamtalk"
    );
    buildTask.group = vscode.TaskGroup.Build;

    const testTask = new vscode.Task(
      { type: "beamtalk", task: "test" },
      vscode.TaskScope.Workspace,
      "test",
      "beamtalk",
      new vscode.ShellExecution("beamtalk", ["test"]),
      "$beamtalk"
    );
    testTask.group = vscode.TaskGroup.Test;

    return [buildTask, testTask];
  }

  resolveTask(): vscode.Task | undefined {
    return undefined;
  }
}

/** Creates the LSP client and starts it. */
async function startClient(context: vscode.ExtensionContext): Promise<void> {
  const resolved = await resolveServerPath(context);

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
    vscode.tasks.registerTaskProvider("beamtalk", new BeamtalkTaskProvider())
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

  // ─── Workspace Explorer (BT-1023) ──────────────────────────────────────────

  workspaceTreeProvider = new WorkspaceTreeDataProvider();
  context.subscriptions.push(workspaceTreeProvider);

  context.subscriptions.push(
    vscode.window.registerTreeDataProvider("beamtalk.workspaceExplorer", workspaceTreeProvider)
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.refreshWorkspace", () => {
      void workspaceTreeProvider?.refresh();
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.killActor", (_node) => {
      // Auto-connect + REPL wiring (BT-1024) will implement this fully.
      void vscode.window.showInformationMessage("Kill actor: not yet connected to a workspace.");
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.inspectBinding", (_node) => {
      // Auto-connect + REPL wiring (BT-1024) will implement this fully.
      void vscode.window.showInformationMessage("Inspect binding: not yet connected to a workspace.");
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.reloadClass", (_node) => {
      // Auto-connect + REPL wiring (BT-1024) will implement this fully.
      void vscode.window.showInformationMessage("Reload class: not yet connected to a workspace.");
    })
  );

  await startClient(context);
}

export function deactivate(): Thenable<void> | undefined {
  const stop = client?.stop();
  // workspaceTreeProvider is disposed via context.subscriptions; dispose() is
  // idempotent so no double-dispose risk, but we detach the client first to
  // ensure no late push events fire into the disposed emitter.
  workspaceTreeProvider?.setClient(null);
  workspaceTreeProvider = undefined;
  outputChannel?.dispose();
  traceOutputChannel?.dispose();
  outputChannel = undefined;
  traceOutputChannel = undefined;
  return stop;
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as crypto from "node:crypto";
import { execFile } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import * as vscode from "vscode";
import { findMethodDeclaration, findStateVarDeclaration } from "./textUtils";
import {
  LanguageClient,
  type LanguageClientOptions,
  RevealOutputChannelOn,
  type ServerOptions,
} from "vscode-languageclient/node";
import { InspectorPanel } from "./inspectorPanel";
import { TranscriptViewProvider } from "./transcriptView";
import { WorkspaceClient } from "./workspaceClient";
import type { LogEntry } from "./workspaceClient";
import type {
  ActorItemNode,
  BindingItemNode,
  ClassItemNode,
  MethodItemNode,
  StateVarItemNode,
} from "./workspaceTreeView";
import { WorkspaceTreeDataProvider } from "./workspaceTreeView";

let client: LanguageClient | undefined;
let outputChannel: vscode.LogOutputChannel | undefined;
let traceOutputChannel: vscode.LogOutputChannel | undefined;
/** Output channel for live runtime log streaming (BT-1433). */
let logOutputChannel: vscode.OutputChannel | undefined;
/** Status bar item showing / changing the active log level. */
let logLevelStatusBarItem: vscode.StatusBarItem | undefined;

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
let workspaceTreeProvider: WorkspaceTreeDataProvider | undefined;
let transcriptViewProvider: TranscriptViewProvider | undefined;

/** The active WorkspaceClient, set when a workspace port file is detected. */
let workspaceWsClient: WorkspaceClient | undefined;
/** The integrated terminal running `beamtalk repl`, if started by this extension. */
let replTerminal: vscode.Terminal | undefined;
/** True while `beamtalk repl` is actively running inside replTerminal. */
let replRunning = false;

/** Update replRunning and sync the VS Code context so toolbar buttons respond. */
function setReplRunning(value: boolean): void {
  replRunning = value;
  void vscode.commands.executeCommand("setContext", "beamtalk.replRunning", value);
}

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
 *
 * @param beamtalkBin - Path or command name for the `beamtalk` CLI binary.
 *   Defaults to bare `"beamtalk"` (PATH lookup). Override with
 *   `beamtalk.binary.path` to use a dev build such as `./target/debug/beamtalk`.
 */
function findLspViaSysroot(beamtalkBin = "beamtalk"): Promise<string | null> {
  return new Promise((resolve) => {
    execFile(
      beamtalkBin,
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
      ["--help"],
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
 *    Uses `beamtalk.binary.path` if configured, otherwise bare `beamtalk`.
 * 3. Bare `beamtalk-lsp` command name (requires PATH)
 *
 * Returns null if Beamtalk is not installed and no override is configured.
 */
async function resolveServerPath(context: vscode.ExtensionContext): Promise<ResolvedServerPath> {
  const config = vscode.workspace.getConfiguration("beamtalk");
  const projectRoot = findProjectRoot();
  const expandVars = (s: string) =>
    projectRoot ? s.replace(/\$\{workspaceFolder\}/g, projectRoot) : s;
  const override = expandVars(config.get<string>("server.path", "").trim());
  const beamtalkBin = expandVars(config.get<string>("binary.path", "").trim()) || "beamtalk";
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

  // 2. Sysroot discovery via `beamtalk --print-sysroot` (or configured binary).
  //    In dev mode, `beamtalk.binary.path` should point to `./target/debug/beamtalk`.
  const sysrootLsp = await findLspViaSysroot(beamtalkBin);
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
  // Create the output channel early so discovery failures are visible.
  outputChannel ??= vscode.window.createOutputChannel("Beamtalk Language Server", { log: true });
  traceOutputChannel ??= vscode.window.createOutputChannel("Beamtalk Language Server Trace", {
    log: true,
  });

  const resolved = await resolveServerPath(context);

  if (!resolved) {
    outputChannel.warn(
      "Could not find beamtalk-lsp. Searched: beamtalk.server.path setting, " +
        "sysroot via `beamtalk --print-sysroot`, and PATH. " +
        "Set beamtalk.server.path in settings or ensure `beamtalk` is on the extension host PATH."
    );
    const action = await vscode.window.showWarningMessage(
      "Could not find beamtalk-lsp. If Beamtalk is installed, set \"beamtalk.server.path\" in settings.",
      "Open Settings",
      "Install Beamtalk"
    );
    if (action === "Open Settings") {
      void vscode.commands.executeCommand(
        "workbench.action.openSettings",
        "beamtalk.server.path"
      );
    } else if (action === "Install Beamtalk") {
      void vscode.env.openExternal(
        vscode.Uri.parse("https://github.com/jamesc/beamtalk#installation")
      );
    }
    return;
  }

  const serverPath = resolved.serverPath;

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

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

// ─── Workspace ID derivation ─────────────────────────────────────────────────

/**
 * Compute the workspace ID for a project root directory.
 *
 * Replicates the Rust implementation in `beamtalk-workspace`:
 * SHA256 of the canonicalized absolute path, first 12 hex characters.
 */
function computeWorkspaceId(projectRoot: string): string | null {
  try {
    let canonical = fs.realpathSync(projectRoot);
    // On Windows, fs.realpathSync() returns plain paths (e.g. "C:\foo") while
    // Rust's std::fs::canonicalize() prepends the UNC extended-path prefix
    // "\\?\" (e.g. "\\?\C:\foo"). Add the prefix here so both sides hash the
    // same string and auto-connect works on Windows.
    if (process.platform === "win32" && !canonical.startsWith("\\\\?\\")) {
      canonical = `\\\\?\\${canonical}`;
    }
    const hash = crypto.createHash("sha256").update(canonical).digest("hex");
    return hash.slice(0, 12);
  } catch {
    return null;
  }
}

/**
 * Find the project root for the current VS Code workspace.
 * Prefers a workspace folder containing `beamtalk.toml`; falls back to the
 * first workspace folder so the Workspace Explorer connects even in projects
 * that haven't created a beamtalk.toml yet (e.g. when just running the REPL).
 * The workspace ID hash check in handlePortFileAppeared still guards against
 * connecting to an unrelated workspace.
 */
function findProjectRoot(): string | null {
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

// ─── Log output formatting (BT-1433) ─────────────────────────────────────────

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
  if (workspaceWsClient) {
    workspaceWsClient.dispose();
    workspaceWsClient = undefined;
    workspaceTreeProvider?.setClient(null);
    transcriptViewProvider?.setClient(null);
  }

  let newClient: WorkspaceClient;
  try {
    newClient = WorkspaceClient.fromFiles(workspaceId);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    outputChannel?.warn(`Failed to create WorkspaceClient for workspace ${workspaceId}: ${msg}`);
    return;
  }

  workspaceWsClient = newClient;
  workspaceTreeProvider?.setClient(newClient);
  transcriptViewProvider?.setClient(newClient);

  // Close inspector panels for actors that have stopped.
  // Stream log events to the Beamtalk Logs output channel (BT-1433).
  newClient.onPush((event) => {
    if (event.channel === "actors" && event.event === "stopped") {
      InspectorPanel.notifyActorStopped(event.data.pid);
    } else if (event.channel === "logs" && event.event === "entry") {
      logOutputChannel?.appendLine(formatLogEntry(event.data));
    }
  });

  // Subscribe to log streaming once connected (BT-1433).
  newClient.onConnectionChange((state) => {
    if (state === "connected") {
      const config = vscode.workspace.getConfiguration("beamtalk");
      const level = config.get<string>("logs.level", "warning");
      newClient.subscribeLogs(level).catch((err) => {
        outputChannel?.warn(
          `Failed to subscribe to logs: ${err instanceof Error ? err.message : String(err)}`
        );
      });
    }
  });

  newClient.connect();
  outputChannel?.info(`Connected to workspace: ${workspaceId}`);
}

/** Disconnect from the current workspace and show disconnected state. */
function disconnectWorkspace(reason: "port-removed"): void {
  if (!workspaceWsClient) {
    return;
  }

  const id = workspaceWsClient.id;
  workspaceWsClient.dispose();
  workspaceWsClient = undefined;
  workspaceTreeProvider?.setClient(null);
  transcriptViewProvider?.setClient(null);

  setReplRunning(false);
  outputChannel?.info(`Disconnected from workspace ${id}: ${reason}`);

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
let portDebounceTimer: ReturnType<typeof setTimeout> | null = null;

async function handlePortFileAppeared(portUri: vscode.Uri): Promise<void> {
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
  if (portDebounceTimer !== null) {
    clearTimeout(portDebounceTimer);
  }
  portDebounceTimer = setTimeout(() => {
    portDebounceTimer = null;
    connectWorkspace(workspaceId);
  }, 200);
}

/** Called when a port file is removed (workspace stopped or timed out). */
function handlePortFileRemoved(portUri: vscode.Uri): void {
  const workspaceId = path.basename(path.dirname(portUri.fsPath));
  if (workspaceWsClient?.id !== workspaceId) {
    return;
  }
  disconnectWorkspace("port-removed");
}

/**
 * Register a file system watcher for workspace port files and trigger
 * an immediate connect if a port file already exists.
 */
function setupAutoConnect(context: vscode.ExtensionContext): void {
  const workspacesDir = path.join(os.homedir(), ".beamtalk", "workspaces");

  // Ensure the directory exists so the FileSystemWatcher has something to watch.
  // On a fresh install, ~/.beamtalk/workspaces/ doesn't exist yet and inotify
  // (Linux) / FSEvents (macOS) can't watch a non-existent path.
  try {
    fs.mkdirSync(workspacesDir, { recursive: true });
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    outputChannel?.warn(`Auto-connect disabled: cannot create ${workspacesDir}: ${msg}`);
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

const SESSION_ID_PATTERN = /\[beamtalk\] session: (\S+)/;

/**
 * Read chunks from a shell execution until the session ID is found.
 * Updates `workspaceTreeProvider` with the captured session ID so bindings
 * from the session are shown in the sidebar.
 */
async function captureSessionId(
  execution: vscode.TerminalShellExecution,
  terminal: vscode.Terminal
): Promise<void> {
  // Buffer the tail of the previous chunk to handle session markers split
  // across arbitrary terminal chunk boundaries.
  let tail = "";
  try {
    for await (const chunk of execution.read()) {
      const text = tail + chunk;
      const match = SESSION_ID_PATTERN.exec(text);
      if (match) {
        const sessionId = match[1];
        outputChannel?.info(`Captured session ID: ${sessionId}`);
        // Only act if this is still our active REPL terminal.
        if (replTerminal !== terminal) {
          break;
        }
        setReplRunning(true);
        workspaceTreeProvider?.setSessionId(sessionId);
        // Don't break — keep reading. The REPL may reconnect and emit a new
        // session ID (BT-1021); we need to pick up the updated value.
        tail = "";
      } else {
        // Keep the last 80 chars to catch markers spanning chunk boundaries.
        tail = text.length > 80 ? text.slice(-80) : text;
      }
    }
  } catch {
    // Stream ended (terminal closed or shell integration disconnected) — no-op.
  }

  // Stream ended — the REPL process exited (e.g. user typed :q).
  // Clear the session so bindings disappear and the Start button returns.
  if (replTerminal === terminal) {
    setReplRunning(false);
    workspaceTreeProvider?.setSessionId(null);
    outputChannel?.info("REPL session ended; session bindings cleared.");
  }
}

export { findMethodDeclaration, findStateVarDeclaration } from "./textUtils";

function replCommand(): string {
  const config = vscode.workspace.getConfiguration("beamtalk");
  const ephemeral = config.get<boolean>("repl.ephemeral", false);
  // beamtalk.binary.path lets dev builds point to e.g. ./target/debug/beamtalk
  // so that `beamtalk repl` picks up freshly-compiled Erlang runtime code.
  let beamtalkBin = config.get<string>("binary.path", "").trim() || "beamtalk";
  // VS Code does not expand ${workspaceFolder} in settings values, only in
  // launch.json/tasks.json. Expand it here so users can write paths like
  // "${workspaceFolder}/target/debug/beamtalk" in their settings.
  const projectRoot = findProjectRoot();
  if (projectRoot) {
    beamtalkBin = beamtalkBin.replace(/\$\{workspaceFolder\}/g, projectRoot);
  }
  // Quote the binary path in case it contains spaces (e.g. "/path/to my/beamtalk").
  const quoted = beamtalkBin.includes(" ") ? `"${beamtalkBin}"` : beamtalkBin;
  return ephemeral ? `${quoted} repl -e` : `${quoted} repl`;
}

/**
 * Open an integrated terminal running `beamtalk repl` and capture the session ID
 * from its output so the Workspace Explorer can show REPL bindings.
 *
 * If the REPL terminal is already open but the REPL has stopped (e.g. after a
 * workspace disconnect), re-run `beamtalk repl` inside the existing terminal
 * rather than ignoring the request.
 */
function startReplCommand(context: vscode.ExtensionContext): void {
  // Reuse the existing terminal if the REPL is still actively running.
  if (replTerminal && replTerminal.exitStatus === undefined && replRunning) {
    replTerminal.show();
    return;
  }

  // If the shell terminal is alive but the REPL has stopped, close it and open a
  // fresh one so we always start from the correct project root cwd.
  if (replTerminal && replTerminal.exitStatus === undefined && !replRunning) {
    replTerminal.dispose();
    replTerminal = undefined;
  }

  const cwd = vscode.workspace.workspaceFolders?.[0]?.uri;
  replTerminal = vscode.window.createTerminal({
    name: "Beamtalk Session",
    iconPath: new vscode.ThemeIcon("beaker"),
    cwd,
  });
  replTerminal.show();
  setReplRunning(true);

  const terminal = replTerminal;

  if (terminal.shellIntegration) {
    // Shell integration is already active — use executeCommand so we can read output.
    const execution = terminal.shellIntegration.executeCommand(replCommand());
    void captureSessionId(execution, terminal);
  } else {
    // Shell integration not yet ready. Wait for it to become available, or fall
    // back to sendText after a timeout so the REPL always starts.
    let started = false;
    // onDidChangeTerminalShellIntegration requires VS Code 1.93+; guard at runtime.
    if (!vscode.window.onDidChangeTerminalShellIntegration) {
      terminal.sendText(replCommand());
      return;
    }
    const integrationDisposable = vscode.window.onDidChangeTerminalShellIntegration(
      ({ terminal: t, shellIntegration }) => {
        if (t !== terminal || started) {
          return;
        }
        started = true;
        integrationDisposable.dispose();
        clearTimeout(fallbackTimer);
        const execution = shellIntegration.executeCommand(replCommand());
        void captureSessionId(execution, terminal);
      }
    );
    context.subscriptions.push(integrationDisposable);

    // Fallback: start REPL via sendText if shell integration doesn't activate.
    const fallbackTimer = setTimeout(() => {
      if (!started && replTerminal === terminal) {
        integrationDisposable.dispose();
        terminal.sendText(replCommand());
      }
    }, 2000);
    context.subscriptions.push({ dispose: () => clearTimeout(fallbackTimer) });
  }
}

// ─── Terminal lifecycle monitoring ────────────────────────────────────────────

/**
 * Set up listeners for terminal lifecycle and shell execution events.
 *
 * - On REPL terminal close: clear session ID and refresh bindings (the workspace
 *   itself stays up; actors/classes remain visible).
 * - Corner case: if a user manually runs `beamtalk repl` in any integrated
 *   terminal, monitor the execution output and adopt the session ID.
 */
function setupTerminalMonitoring(context: vscode.ExtensionContext): void {
  // Detect REPL terminal closure.
  context.subscriptions.push(
    vscode.window.onDidCloseTerminal((terminal) => {
      if (terminal !== replTerminal) {
        return;
      }
      replTerminal = undefined;
      setReplRunning(false);
      // Clear session ID — bindings from the closed session are gone.
      workspaceTreeProvider?.setSessionId(null);
      outputChannel?.info("Beamtalk Session terminal closed; session bindings cleared.");
    })
  );

  // Corner case: capture session ID from any terminal running `beamtalk repl`.
  // onDidStartTerminalShellExecution requires VS Code 1.93+; guard at runtime.
  if (!vscode.window.onDidStartTerminalShellExecution) {
    return;
  }
  context.subscriptions.push(
    vscode.window.onDidStartTerminalShellExecution(async (event) => {
      const cmdLine = event.execution.commandLine.value;
      if (!cmdLine.includes("beamtalk repl")) {
        return;
      }
      // Adopt this terminal as our REPL terminal if we don't already have one.
      if (!replTerminal || replTerminal.exitStatus !== undefined) {
        replTerminal = event.terminal;
      }
      void captureSessionId(event.execution, event.terminal);
    })
  );
}

// ─── Extension activation ─────────────────────────────────────────────────────

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  // Reset context state on each activation — VS Code persists setContext values
  // across extension host restarts, so we must re-initialise on every activation.
  setReplRunning(false);

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

  // ─── Beamtalk Logs output channel + log level picker (BT-1433) ────────────

  logOutputChannel ??= vscode.window.createOutputChannel("Beamtalk Logs");

  logLevelStatusBarItem?.dispose();
  logLevelStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 50);
  logLevelStatusBarItem.command = "beamtalk.setLogLevel";
  logLevelStatusBarItem.tooltip = "Beamtalk log level — click to change";
  context.subscriptions.push(logLevelStatusBarItem);

  const updateLogLevelStatusBar = (level: string): void => {
    if (logLevelStatusBarItem) {
      logLevelStatusBarItem.text = `$(filter) ${level}`;
      logLevelStatusBarItem.show();
    }
  };

  {
    const config = vscode.workspace.getConfiguration("beamtalk");
    updateLogLevelStatusBar(config.get<string>("logs.level", "warning"));
  }

  // Keep the status bar in sync when the setting is changed via Settings UI.
  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration("beamtalk.logs.level")) {
        const config = vscode.workspace.getConfiguration("beamtalk");
        updateLogLevelStatusBar(config.get<string>("logs.level", "warning"));
      }
    })
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
      updateLogLevelStatusBar(newLevel);
      if (workspaceWsClient) {
        workspaceWsClient.subscribeLogs(newLevel).catch((err) => {
          outputChannel?.warn(
            `Failed to update log subscription: ${err instanceof Error ? err.message : String(err)}`
          );
        });
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.showLogs", () => {
      logOutputChannel?.show();
    })
  );

  // ─── Workspace Explorer (BT-1023) ──────────────────────────────────────────

  workspaceTreeProvider = new WorkspaceTreeDataProvider();
  context.subscriptions.push(workspaceTreeProvider);

  context.subscriptions.push(
    vscode.window.createTreeView("beamtalk.workspaceExplorer", {
      treeDataProvider: workspaceTreeProvider,
    })
  );

  // ─── Transcript View (BT-1024) ─────────────────────────────────────────────

  transcriptViewProvider = new TranscriptViewProvider();
  context.subscriptions.push(transcriptViewProvider);

  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      TranscriptViewProvider.viewType,
      transcriptViewProvider
    )
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.clearTranscript", () => {
      transcriptViewProvider?.clear();
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.refreshWorkspace", () => {
      void workspaceTreeProvider?.refresh();
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.killActor", (_node) => {
      // Rich actor kill requires Phase 2 (inspector panel).
      void vscode.window.showInformationMessage("Kill actor: not yet connected to a workspace.");
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.inspectBinding", (node?: BindingItemNode) => {
      if (!node || node.kind !== "binding-item") {
        void vscode.window.showInformationMessage("Select a binding in Workspace Explorer first.");
        return;
      }
      const sessionId =
        workspaceTreeProvider?.currentSessionId ?? workspaceWsClient?.currentSessionId ?? null;
      InspectorPanel.show(workspaceWsClient ?? null, {
        kind: "binding",
        name: node.name,
        value: node.value,
        sessionId,
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.inspectActor", (node?: ActorItemNode) => {
      if (!node || node.kind !== "actor-item") {
        void vscode.window.showInformationMessage("Select an actor in Workspace Explorer first.");
        return;
      }
      InspectorPanel.show(workspaceWsClient ?? null, {
        kind: "actor",
        info: node.info,
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.openClassSource", async (node?: ClassItemNode) => {
      const sourceFile = node?.info.source_file;
      if (!sourceFile || sourceFile === "unknown") {
        await vscode.window.showInformationMessage("Source not available for this class.");
        return;
      }
      const uri = vscode.Uri.file(sourceFile);
      let document: vscode.TextDocument;
      try {
        document = await vscode.workspace.openTextDocument(uri);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        await vscode.window.showErrorMessage(`Could not open source: ${sourceFile}: ${msg}`);
        return;
      }

      const className = node?.info.name;
      if (!className) return;

      // Find the class declaration position via regex.
      // Note: executeDocumentSymbolProvider is not used here because openTextDocument
      // does not trigger textDocument/didOpen to the LSP — symbols are only available
      // for files already open in an editor. The regex is the reliable primary approach.
      let position: vscode.Position | undefined;
      const text = document.getText();
      const escaped = className.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
      // Beamtalk class declarations use `SuperClass subclass: ClassName` syntax.
      const pattern = new RegExp(`\\bsubclass:\\s+(${escaped})(?=\\s|$)`, "g");
      let classMatch = pattern.exec(text);
      while (classMatch !== null) {
        const lineStart = text.lastIndexOf("\n", classMatch.index) + 1;
        const linePrefix = text.slice(lineStart, classMatch.index).trimStart();
        if (!linePrefix.startsWith("//")) {
          position = document.positionAt(classMatch.index + classMatch[0].indexOf(className));
          break;
        }
        classMatch = pattern.exec(text);
      }

      await vscode.window.showTextDocument(document, {
        selection: position ? new vscode.Range(position, position) : undefined,
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.reloadClass", async (node?: ClassItemNode) => {
      if (!workspaceWsClient) {
        await vscode.window.showInformationMessage("Not connected to a Beamtalk workspace.");
        return;
      }
      const sourceFile = node?.info.source_file;
      if (!sourceFile || sourceFile === "unknown") {
        await vscode.window.showInformationMessage("Source not available for this class.");
        return;
      }
      try {
        const result = await workspaceWsClient.reload(sourceFile);
        const names = result.classes.map((c) => c.name).filter(Boolean);
        const baseMessage =
          names.length > 0 ? `Reloaded: ${names.join(", ")}` : "Reload completed.";
        const warningText = result.warnings.length > 0 ? ` (${result.warnings.join("; ")})` : "";
        await vscode.window.showInformationMessage(`${baseMessage}${warningText}`);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        await vscode.window.showErrorMessage(`Reload failed: ${msg}`);
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.navigateToMethod", async (node: MethodItemNode) => {
      const { classInfo, method } = node;

      // Only navigate if the runtime recorded a source file path.
      // "unknown" means the class was defined in the REPL or loaded without a path.
      const sourceFile = classInfo.source_file;
      if (!sourceFile || sourceFile === "unknown") {
        await vscode.window.showInformationMessage(
          `No source file recorded for class ${classInfo.name}`
        );
        return;
      }

      const uri = vscode.Uri.file(sourceFile);
      let document: vscode.TextDocument;
      try {
        document = await vscode.workspace.openTextDocument(uri);
      } catch {
        await vscode.window.showInformationMessage(
          `Cannot open source for ${classInfo.name}: ${sourceFile}`
        );
        return;
      }

      // Find the method declaration position by scanning line by line.
      // Beamtalk declarations: `class selector =>` (class-side), `selector =>` (instance-side).
      // LSP go-to-definition is used *after* showTextDocument below for precise navigation.
      const text = document.getText();
      let declOffset = findMethodDeclaration(text, method.selector, method.side);
      if (declOffset === -1) declOffset = text.indexOf(method.selector);
      if (declOffset === -1) {
        await vscode.window.showInformationMessage(
          `Method '${method.selector}' not found in source for ${classInfo.name}`
        );
        return;
      }
      const position = document.positionAt(declOffset);

      // Try LSP go-to-definition for a precise location. If LSP is unavailable or
      // returns no results, fall back to the text-search position — don't fail.
      let locations: Array<vscode.Location | vscode.LocationLink> | undefined;
      try {
        locations = await vscode.commands.executeCommand<
          Array<vscode.Location | vscode.LocationLink>
        >("vscode.executeDefinitionProvider", uri, position);
      } catch {
        // LSP not available — fall through to direct navigation.
      }

      if (locations && locations.length > 0) {
        const loc = locations[0];
        const targetUri = "targetUri" in loc ? loc.targetUri : loc.uri;
        const targetRange = "targetRange" in loc ? loc.targetRange : loc.range;
        await vscode.window.showTextDocument(targetUri, { selection: targetRange });
      } else {
        // LSP unavailable or returned no results — navigate to the method declaration.
        await vscode.window.showTextDocument(document, {
          selection: new vscode.Range(position, position),
        });
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand(
      "beamtalk.navigateToStateVar",
      async (node?: StateVarItemNode) => {
        if (!node || node.kind !== "state-item") {
          await vscode.window.showInformationMessage(
            "Select a state variable in Workspace Explorer first."
          );
          return;
        }
        const { classInfo, stateVar } = node;
        const sourceFile = classInfo.source_file;
        if (!sourceFile || sourceFile === "unknown") {
          await vscode.window.showInformationMessage(
            `No source file recorded for class ${classInfo.name}`
          );
          return;
        }
        const uri = vscode.Uri.file(sourceFile);
        let document: vscode.TextDocument;
        try {
          document = await vscode.workspace.openTextDocument(uri);
        } catch {
          await vscode.window.showInformationMessage(
            `Cannot open source for ${classInfo.name}: ${sourceFile}`
          );
          return;
        }
        const text = document.getText();
        let declOffset = findStateVarDeclaration(text, stateVar.name);
        if (declOffset === -1) declOffset = text.indexOf(stateVar.name);
        if (declOffset === -1) {
          await vscode.window.showInformationMessage(
            `State variable '${stateVar.name}' not found in source for ${classInfo.name}`
          );
          return;
        }
        const position = document.positionAt(declOffset);
        await vscode.window.showTextDocument(document, {
          selection: new vscode.Range(position, position),
        });
      }
    )
  );

  // ─── Start / Stop REPL commands (BT-1025) ──────────────────────────────────

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.startRepl", () => startReplCommand(context))
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.stopRepl", () => {
      // Close the REPL terminal — clears the session and bindings,
      // but leaves the workspace connection up so classes/actors remain visible.
      if (replTerminal && replTerminal.exitStatus === undefined) {
        replTerminal.dispose();
      }
    })
  );

  // ─── Auto-connect + terminal monitoring (BT-1025) ──────────────────────────

  setupTerminalMonitoring(context);
  setupAutoConnect(context);

  await startClient(context);
}

export function deactivate(): Thenable<void> | undefined {
  const stop = client?.stop();
  if (portDebounceTimer !== null) {
    clearTimeout(portDebounceTimer);
    portDebounceTimer = null;
  }
  // Close all inspector panels before disposing the client so they don't
  // attempt to re-fetch after the connection is gone.
  InspectorPanel.disposeAll();
  // Providers are disposed via context.subscriptions; dispose() is idempotent
  // so no double-dispose risk, but we detach the client first to ensure no
  // late push events fire into disposed providers.
  workspaceWsClient?.dispose();
  workspaceWsClient = undefined;
  replTerminal = undefined;
  workspaceTreeProvider?.setClient(null);
  workspaceTreeProvider = undefined;
  transcriptViewProvider?.setClient(null);
  transcriptViewProvider = undefined;
  logLevelStatusBarItem?.dispose();
  logLevelStatusBarItem = undefined;
  logOutputChannel?.dispose();
  outputChannel?.dispose();
  traceOutputChannel?.dispose();
  logOutputChannel = undefined;
  outputChannel = undefined;
  traceOutputChannel = undefined;
  return stop;
}

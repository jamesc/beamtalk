// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { state } from "./extensionState";
import { expandWorkspaceFolder, findProjectRoot } from "./projectRoot";

/** Update replRunning and sync the VS Code context so toolbar buttons respond. */
export function setReplRunning(value: boolean): void {
  state.replRunning = value;
  void vscode.commands.executeCommand("setContext", "beamtalk.replRunning", value);
}

const SESSION_ID_PATTERN = /\[beamtalk\] session: ([A-Za-z0-9_-]+)(?=\s)/;

/**
 * Feed one terminal chunk to the session-id scraper (BT-5130). The id must be
 * followed by whitespace within the buffered text, so a marker split across
 * chunks is never accepted as a prefix; the incomplete tail is carried over.
 */
export function scanSessionChunk(
  tail: string,
  chunk: string
): { sessionId: string | undefined; tail: string } {
  const text = tail + chunk;
  const match = SESSION_ID_PATTERN.exec(text);
  if (match) {
    return { sessionId: match[1], tail: "" };
  }
  // Keep the last 80 chars to catch markers spanning chunk boundaries.
  return { sessionId: undefined, tail: text.length > 80 ? text.slice(-80) : text };
}

/** The slice of `WorkspaceClient` closing a REPL session needs. */
export interface SessionCloser {
  readonly state: string;
  closeSession(sessionId: string): Promise<unknown>;
}

/**
 * End the node session of a REPL that has stopped (BT-5199). A REPL killed with
 * its terminal never sends `close` itself, so without this its session stays
 * resumable until the node's idle bound (4 h by default). Clears the recorded
 * id first, so a terminal close after the REPL exited sends one `close`, not two.
 */
export function closeReplSession(
  client: SessionCloser | undefined,
  warn: (msg: string) => void
): void {
  const sessionId = state.replSessionId;
  state.replSessionId = undefined;
  if (sessionId === undefined || !client || client.state !== "connected") return;
  client.closeSession(sessionId).catch((err) => {
    warn(
      `Failed to close REPL session ${sessionId}: ${err instanceof Error ? err.message : String(err)}`
    );
  });
}

/** How much recent REPL output `captureSessionId` keeps for the reconnect check. */
const RECENT_OUTPUT_CHARS = 400;

/**
 * True when `sessionId`'s marker in `recent` directly follows the CLI's
 * "Reconnected (new session)" notice — the marker the REPL prints after a
 * reconnect gave it a new session (BT-1021), as opposed to one in user output.
 */
export function followsReconnectNotice(recent: string, sessionId: string): boolean {
  const escaped = sessionId.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  return new RegExp(
    `Reconnected \\(new session\\)\\.[^\\n]*\\r?\\n\\[beamtalk\\] session: ${escaped}(?=\\s)`
  ).test(recent);
}

/**
 * Read chunks from a shell execution until the session ID is found.
 * Updates `workspaceTreeProvider` with the captured session ID so bindings
 * from the session are shown in the sidebar.
 */
export async function captureSessionId(
  execution: vscode.TerminalShellExecution,
  terminal: vscode.Terminal
): Promise<void> {
  // Buffer the tail of the previous chunk to handle session markers split
  // across arbitrary terminal chunk boundaries.
  let tail = "";
  // True only when the execution's output ran to its end, i.e. the REPL
  // process exited. A thrown read (shell integration dropped) says nothing
  // about the REPL, which may still be running (BT-5199).
  let executionEnded = false;
  // The session `close` ends when the REPL exits (BT-5199): the first marker
  // of a run (the REPL's startup line), or a later one the REPL printed right
  // after its "Reconnected (new session)" notice. Any other later marker may
  // be user output (`Transcript show:` of a marker-shaped string), so it moves
  // the sidebar as before but never picks the session `close` ends.
  let closeTargetRecorded = false;
  // The most recent output, so a later marker can be checked against the
  // reconnect notice printed just before it (possibly in an earlier chunk).
  let recent = "";
  try {
    for await (const chunk of execution.read()) {
      recent = (recent + chunk).slice(-RECENT_OUTPUT_CHARS);
      const scanned = scanSessionChunk(tail, chunk);
      const sessionId = scanned.sessionId;
      tail = scanned.tail;
      if (sessionId !== undefined) {
        state.outputChannel?.info(`Captured session ID: ${sessionId}`);
        // Only act if this is still our active REPL terminal.
        if (state.replTerminal !== terminal) {
          break;
        }
        setReplRunning(true);
        if (!closeTargetRecorded || followsReconnectNotice(recent, sessionId)) {
          closeTargetRecorded = true;
          state.replSessionId = sessionId;
        }
        state.workspaceTreeProvider?.setSessionId(sessionId);
        // Don't break — keep reading. The REPL may reconnect and emit a new
        // session ID (BT-1021); we need to pick up the updated value.
      }
    }
    executionEnded = true;
  } catch {
    // Stream ended (terminal closed or shell integration disconnected) — no-op.
  }

  // Stream ended — the REPL process exited (e.g. user typed :q).
  // Clear the session so bindings disappear and the Start button returns.
  if (state.replTerminal === terminal) {
    setReplRunning(false);
    if (executionEnded) {
      closeReplSession(state.workspaceWsClient, (msg) => state.outputChannel?.warn(msg));
    }
    state.workspaceTreeProvider?.setSessionId(null);
    state.outputChannel?.info("REPL session ended; session bindings cleared.");
  }
}

function replCommand(): string {
  const config = vscode.workspace.getConfiguration("beamtalk");
  const ephemeral = config.get<boolean>("repl.ephemeral", false);
  // beamtalk.binary.path lets dev builds point to e.g. ./target/debug/beamtalk
  // so that `beamtalk repl` picks up freshly-compiled Erlang runtime code.
  const beamtalkBin = expandWorkspaceFolder(
    config.get<string>("binary.path", "").trim() || "beamtalk",
    findProjectRoot()
  );
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
export function startReplCommand(context: vscode.ExtensionContext): void {
  // Reuse the existing terminal if the REPL is still actively running.
  if (state.replTerminal && state.replTerminal.exitStatus === undefined && state.replRunning) {
    state.replTerminal.show();
    return;
  }

  // If the shell terminal is alive but the REPL has stopped, close it and open a
  // fresh one so we always start from the correct project root cwd.
  if (state.replTerminal && state.replTerminal.exitStatus === undefined && !state.replRunning) {
    state.replTerminal.dispose();
    state.replTerminal = undefined;
  }

  const cwd = vscode.workspace.workspaceFolders?.[0]?.uri;
  state.replTerminal = vscode.window.createTerminal({
    name: "Beamtalk Session",
    iconPath: new vscode.ThemeIcon("beaker"),
    cwd,
  });
  state.replTerminal.show();
  setReplRunning(true);

  const terminal = state.replTerminal;

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
      if (!started && state.replTerminal === terminal) {
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
export function setupTerminalMonitoring(context: vscode.ExtensionContext): void {
  // Detect REPL terminal closure.
  context.subscriptions.push(
    vscode.window.onDidCloseTerminal((terminal) => {
      if (terminal !== state.replTerminal) {
        return;
      }
      state.replTerminal = undefined;
      setReplRunning(false);
      closeReplSession(state.workspaceWsClient, (msg) => state.outputChannel?.warn(msg));
      // Clear session ID — bindings from the closed session are gone.
      state.workspaceTreeProvider?.setSessionId(null);
      state.outputChannel?.info("Beamtalk Session terminal closed; session bindings cleared.");
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
      if (!state.replTerminal || state.replTerminal.exitStatus !== undefined) {
        state.replTerminal = event.terminal;
      }
      void captureSessionId(event.execution, event.terminal);
    })
  );
}

/** Registers `beamtalk.startRepl` and `beamtalk.stopRepl` (BT-1025). */
export function registerReplCommands(context: vscode.ExtensionContext): void {
  // ─── Start / Stop REPL commands (BT-1025) ──────────────────────────────────

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.startRepl", () => startReplCommand(context))
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.stopRepl", () => {
      // Close the REPL terminal — clears the session and bindings,
      // but leaves the workspace connection up so classes/actors remain visible.
      if (state.replTerminal && state.replTerminal.exitStatus === undefined) {
        state.replTerminal.dispose();
      }
    })
  );
}

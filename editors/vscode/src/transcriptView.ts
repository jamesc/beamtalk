// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import type * as vscode from "vscode";
import type { PushEvent, WorkspaceClient } from "./workspaceClient";

const MAX_LINES = 10_000;

/**
 * TranscriptViewProvider implements a sidebar WebviewView that streams live
 * Transcript output from the Beamtalk workspace (ADR 0046).
 *
 * Renders as an auto-scrolling `<pre>` with a clear button. Enforces a
 * max-lines cap of 10,000 lines to prevent unbounded DOM growth from runaway
 * actors writing to Transcript.
 *
 * Usage:
 *   const provider = new TranscriptViewProvider();
 *   vscode.window.registerWebviewViewProvider(TranscriptViewProvider.viewType, provider);
 *   provider.setClient(client); // called by auto-connect logic (BT-1024)
 */
export class TranscriptViewProvider implements vscode.WebviewViewProvider, vscode.Disposable {
  static readonly viewType = "beamtalk.transcript";

  private view: vscode.WebviewView | undefined;
  /** Text chunks buffered before the WebviewView is first resolved. */
  private readonly pendingChunks: string[] = [];
  private readonly disposeHandlers: Array<() => void> = [];
  private disposed = false;

  // ─── WebviewViewProvider ─────────────────────────────────────────────────

  /**
   * Called by VSCode when the view becomes visible for the first time (or is
   * shown after being hidden). Initialises the webview HTML and replays any
   * transcript output buffered while the view was not yet visible.
   */
  resolveWebviewView(
    webviewView: vscode.WebviewView,
    _context: vscode.WebviewViewResolveContext<unknown>,
    _token: vscode.CancellationToken
  ): void {
    this.view = webviewView;
    webviewView.webview.options = { enableScripts: true };
    webviewView.webview.html = this._buildHtml();

    // Replay any chunks buffered before the view was resolved.
    if (this.pendingChunks.length > 0) {
      const buffered = this.pendingChunks.join("");
      this.pendingChunks.length = 0;
      this.pendingLineCount = 0;
      webviewView.webview.postMessage({ command: "append", text: buffered });
    }

    webviewView.webview.onDidReceiveMessage((msg: unknown) => {
      if (
        typeof msg === "object" &&
        msg !== null &&
        (msg as { command?: unknown }).command === "clear"
      ) {
        // The webview has already cleared its own DOM; just clear our buffer.
        this.pendingChunks.length = 0;
        this.pendingLineCount = 0;
      }
    });

    webviewView.onDidDispose(() => {
      this.view = undefined;
    });
  }

  // ─── Public API ──────────────────────────────────────────────────────────

  /**
   * Attach a WorkspaceClient to receive `transcript` push events.
   * Pass null to detach (e.g. on workspace disconnect).
   */
  setClient(client: WorkspaceClient | null): void {
    for (const d of this.disposeHandlers) d();
    this.disposeHandlers.length = 0;

    if (!client) return;

    const dispose = client.onPush((event: PushEvent) => {
      if (event.channel === "transcript") {
        this._appendText(event.text);
      }
    });
    this.disposeHandlers.push(dispose);
  }

  /** Clear the transcript display and the internal pre-resolve buffer. */
  clear(): void {
    this.pendingChunks.length = 0;
    this.pendingLineCount = 0;
    this.view?.webview.postMessage({ command: "clear" });
  }

  dispose(): void {
    if (this.disposed) return;
    this.disposed = true;
    for (const d of this.disposeHandlers) d();
    this.disposeHandlers.length = 0;
  }

  // ─── Private ─────────────────────────────────────────────────────────────

  private pendingLineCount = 0;

  private _appendText(text: string): void {
    if (this.view) {
      this.view.webview.postMessage({ command: "append", text });
    } else {
      this.pendingChunks.push(text);
      // Count at least 1 per chunk so newline-free chunks (e.g. a long single
      // line) still advance the counter and keep the buffer bounded.
      this.pendingLineCount += Math.max(1, (text.match(/\n/g) || []).length);
      // Apply the same MAX_LINES cap to the pre-resolve buffer to bound memory
      // if a runaway actor writes to Transcript before the panel is opened.
      if (this.pendingLineCount > MAX_LINES) {
        const allLines = this.pendingChunks.join("").split("\n");
        const keep = allLines.slice(allLines.length - MAX_LINES);
        this.pendingChunks.length = 0;
        this.pendingChunks.push(keep.join("\n"));
        this.pendingLineCount = MAX_LINES;
      }
    }
  }

  private _buildHtml(): string {
    // MAX_LINES is inlined so the webview JS doesn't need a separate channel.
    const maxLines = MAX_LINES;
    const trimTo = Math.floor(maxLines * 0.9);

    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="Content-Security-Policy"
        content="default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline';">
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      display: flex;
      flex-direction: column;
      height: 100vh;
      font-family: var(--vscode-editor-font-family, monospace);
      font-size: var(--vscode-editor-font-size, 12px);
      background: var(--vscode-sideBar-background);
      color: var(--vscode-sideBar-foreground);
    }
    #toolbar {
      display: flex;
      justify-content: flex-end;
      padding: 4px 6px;
      border-bottom: 1px solid var(--vscode-sideBarSectionHeader-border, transparent);
      flex-shrink: 0;
    }
    button {
      background: transparent;
      border: 1px solid var(--vscode-button-border, transparent);
      color: var(--vscode-icon-foreground);
      cursor: pointer;
      padding: 2px 6px;
      border-radius: 2px;
      font-size: 11px;
    }
    button:hover { background: var(--vscode-toolbar-hoverBackground); }
    #output {
      flex: 1;
      overflow-y: auto;
      padding: 6px;
      white-space: pre-wrap;
      word-break: break-all;
    }
    #output:empty::before {
      content: 'No transcript output yet.';
      color: var(--vscode-disabledForeground);
      font-style: italic;
    }
  </style>
</head>
<body>
  <div id="toolbar">
    <button id="clearBtn" title="Clear transcript">Clear</button>
  </div>
  <pre id="output"></pre>
  <script>
    (function () {
      const vscode = acquireVsCodeApi();
      const output = document.getElementById('output');
      const MAX_LINES = ${maxLines};
      const TRIM_TO = ${trimTo};
      let lineCount = 0;
      let autoScroll = true;

      output.addEventListener('scroll', () => {
        const atBottom = output.scrollHeight - output.scrollTop - output.clientHeight < 10;
        autoScroll = atBottom;
      });

      document.getElementById('clearBtn').addEventListener('click', () => {
        output.textContent = '';
        lineCount = 0;
        autoScroll = true;
        vscode.postMessage({ command: 'clear' });
      });

      window.addEventListener('message', (event) => {
        const msg = event.data;
        if (msg.command === 'append') {
          // Count at least 1 per chunk so newline-free chunks still advance
          // the counter and keep the DOM bounded.
          lineCount += Math.max(1, (msg.text.match(/\n/g) || []).length);
          output.appendChild(document.createTextNode(msg.text));

          // Enforce max-lines cap: trim to TRIM_TO lines when cap is exceeded.
          // Using a trim-to threshold amortises the cost of full DOM rebuilds.
          if (lineCount > MAX_LINES) {
            const allLines = output.textContent.split('\n');
            const startIdx = allLines.length > TRIM_TO ? allLines.length - TRIM_TO : 0;
            output.textContent = allLines.slice(startIdx).join('\n');
            lineCount = TRIM_TO;
          }

          if (autoScroll) {
            output.scrollTop = output.scrollHeight;
          }
        } else if (msg.command === 'clear') {
          output.textContent = '';
          lineCount = 0;
          autoScroll = true;
        }
      });
    }());
  </script>
</body>
</html>`;
  }
}

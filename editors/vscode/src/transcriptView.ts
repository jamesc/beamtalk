// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import type * as vscode from "vscode";
import type { PushEvent, WorkspaceClient } from "./workspaceClient";

const MAX_LINES = 10_000;
const TRIM_TO = Math.floor(MAX_LINES * 0.9);

/**
 * TranscriptViewProvider implements a sidebar WebviewView that streams live
 * Transcript output from the Beamtalk workspace (ADR 0046).
 *
 * Uses webview.html reassignment instead of postMessage to work correctly with
 * workspace extensions in remote (WSL) environments where postMessage from the
 * extension host to the webview is not bridged.
 *
 * Usage:
 *   const provider = new TranscriptViewProvider();
 *   vscode.window.registerWebviewViewProvider(TranscriptViewProvider.viewType, provider);
 *   provider.setClient(client); // called by auto-connect logic (BT-1024)
 */
export class TranscriptViewProvider implements vscode.WebviewViewProvider, vscode.Disposable {
  static readonly viewType = "beamtalk.transcript";

  private static readonly DEBOUNCE_MS = 50;

  private view: vscode.WebviewView | undefined;
  private accumText = "";
  private lineCount = 0;
  private readonly disposeHandlers: Array<() => void> = [];
  private disposed = false;
  private debounceTimer: ReturnType<typeof setTimeout> | undefined;

  // ─── WebviewViewProvider ─────────────────────────────────────────────────

  resolveWebviewView(
    webviewView: vscode.WebviewView,
    _context: vscode.WebviewViewResolveContext<unknown>,
    _token: vscode.CancellationToken
  ): void {
    this.view = webviewView;
    webviewView.webview.options = { enableScripts: true };
    webviewView.webview.html = this._buildHtml();

    webviewView.onDidDispose(() => {
      this.view = undefined;
    });
  }

  // ─── Public API ──────────────────────────────────────────────────────────

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

  clear(): void {
    if (this.debounceTimer !== undefined) {
      clearTimeout(this.debounceTimer);
      this.debounceTimer = undefined;
    }
    this.accumText = "";
    this.lineCount = 0;
    if (this.view) {
      this.view.webview.html = this._buildHtml();
    }
  }

  dispose(): void {
    if (this.disposed) return;
    this.disposed = true;
    if (this.debounceTimer !== undefined) {
      clearTimeout(this.debounceTimer);
      this.debounceTimer = undefined;
    }
    for (const d of this.disposeHandlers) d();
    this.disposeHandlers.length = 0;
  }

  // ─── Private ─────────────────────────────────────────────────────────────

  private _appendText(text: string): void {
    this.accumText += text;
    this.lineCount += (text.match(/\n/g) ?? []).length;

    if (this.lineCount > MAX_LINES) {
      const lines = this.accumText.split("\n");
      const start = lines.length > TRIM_TO ? lines.length - TRIM_TO : 0;
      const kept = lines.slice(start);
      this.accumText = kept.join("\n");
      this.lineCount = kept.length - 1;
    }

    if (!this.view) return;

    if (this.debounceTimer === undefined) {
      this.debounceTimer = setTimeout(() => {
        this.debounceTimer = undefined;
        if (this.view) {
          this.view.webview.html = this._buildHtml();
        }
      }, TranscriptViewProvider.DEBOUNCE_MS);
    }
  }

  private _escapeHtml(text: string): string {
    return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }

  private _buildHtml(): string {
    const content = this._escapeHtml(this.accumText);
    const isEmpty = this.accumText.length === 0;

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
    #output {
      flex: 1;
      overflow-y: auto;
      padding: 6px;
      white-space: pre-wrap;
      word-break: break-all;
    }
    .empty {
      color: var(--vscode-disabledForeground);
      font-style: italic;
    }
  </style>
</head>
<body>
  <pre id="output">${isEmpty ? '<span class="empty">No transcript output yet.</span>' : content}</pre>
  <script>
    (function () {
      const el = document.getElementById('output');
      // Restore scroll: only auto-scroll if user was already at the bottom
      // (or hasn't scrolled yet). Preserves the ability to scroll up to inspect.
      const atBottom = sessionStorage.getItem('scrollAtBottom') !== 'false';
      if (atBottom) {
        el.scrollTop = el.scrollHeight;
      }
      el.addEventListener('scroll', () => {
        const near = el.scrollHeight - el.scrollTop - el.clientHeight < 20;
        sessionStorage.setItem('scrollAtBottom', near ? 'true' : 'false');
      });
    }());
  </script>
</body>
</html>`;
  }
}

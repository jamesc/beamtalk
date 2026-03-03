// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as crypto from "node:crypto";
import * as vscode from "vscode";
import type { ActorInfo, WorkspaceClient } from "./workspaceClient";

// ─── Target types ─────────────────────────────────────────────────────────────

export interface BindingTarget {
  readonly kind: "binding";
  readonly name: string;
  readonly value: unknown;
  /** Session ID used to re-fetch the binding value on refresh. */
  readonly sessionId: string | null;
}

export interface ActorTarget {
  readonly kind: "actor";
  readonly info: ActorInfo;
}

export type InspectTarget = BindingTarget | ActorTarget;

// ─── InspectorPanel ───────────────────────────────────────────────────────────

/**
 * Rich inspector WebviewPanel for Beamtalk workspace bindings and actors
 * (ADR 0046, Phase 2, BT-1028).
 *
 * One panel is kept per target (binding name or actor PID). Clicking "Inspect"
 * again on the same target reveals the existing panel instead of creating a
 * duplicate. The panel renders the object graph with expandable nested
 * structures; a manual refresh button re-fetches live data from the workspace.
 *
 * No framework dependency — plain HTML/CSS/JS only.
 */
export class InspectorPanel {
  /** Open panels keyed by target key ("binding:<name>" or "actor:<pid>"). */
  private static readonly _panels = new Map<string, InspectorPanel>();

  private readonly _panel: vscode.WebviewPanel;
  private _target: InspectTarget;
  private _client: WorkspaceClient | null;
  private _disposed = false;
  /** Monotonic counter — incremented on each _refetch() call to discard stale responses. */
  private _refreshGeneration = 0;

  private constructor(
    panel: vscode.WebviewPanel,
    target: InspectTarget,
    client: WorkspaceClient | null
  ) {
    this._panel = panel;
    this._target = target;
    this._client = client;

    this._panel.onDidDispose(() => this._onPanelDispose());

    this._panel.webview.onDidReceiveMessage((msg: unknown) => {
      if (
        typeof msg === "object" &&
        msg !== null &&
        (msg as Record<string, unknown>).type === "refresh"
      ) {
        void this._refetch();
      }
    });
  }

  // ─── Static API ──────────────────────────────────────────────────────────

  /**
   * Open (or reveal) the inspector panel for the given target.
   *
   * If a panel already exists for this target it is revealed and its content
   * is refreshed: bindings are re-rendered with the latest value passed in;
   * actor panels trigger a live re-fetch from the workspace. Otherwise a new
   * panel is created in the editor area (ViewColumn.Two to keep the tree
   * view visible).
   */
  static show(client: WorkspaceClient | null, target: InspectTarget): void {
    const key = InspectorPanel._targetKey(target);
    const existing = InspectorPanel._panels.get(key);

    if (existing) {
      existing._client = client;
      existing._panel.reveal(vscode.ViewColumn.Two);
      if (target.kind === "binding") {
        // Update to the latest value passed in and re-render immediately.
        existing._target = target;
        existing._postContent(renderValue(target.value));
      } else {
        // Actor: re-fetch live state so the panel is always up to date on reveal.
        void existing._refetch();
      }
      return;
    }

    const title = InspectorPanel._panelTitle(target);
    const panel = vscode.window.createWebviewPanel(
      "beamtalk.inspector",
      title,
      vscode.ViewColumn.Two,
      {
        enableScripts: true,
        retainContextWhenHidden: true,
      }
    );

    const inspector = new InspectorPanel(panel, target, client);
    InspectorPanel._panels.set(key, inspector);

    if (target.kind === "binding") {
      // Value is immediately available — render it directly.
      inspector._initHtml(renderValue(target.value));
    } else {
      // Actor state must be fetched from the workspace.
      inspector._initHtml(renderLoading());
      void inspector._refetch();
    }
  }

  /**
   * Close the inspector panel for a specific actor PID (called when the actor
   * is stopped so the panel doesn't show stale state).
   */
  static notifyActorStopped(pid: string): void {
    const key = `actor:${pid}`;
    InspectorPanel._panels.get(key)?.dispose();
  }

  /** Dispose all open inspector panels (called on extension deactivation). */
  static disposeAll(): void {
    for (const inspector of [...InspectorPanel._panels.values()]) {
      inspector.dispose();
    }
  }

  /** Dispose this panel explicitly (also triggered by `panel.onDidDispose`). */
  dispose(): void {
    if (this._disposed) return;
    this._disposed = true;
    InspectorPanel._panels.delete(InspectorPanel._targetKey(this._target));
    this._panel.dispose();
  }

  // ─── Private helpers ─────────────────────────────────────────────────────

  private _onPanelDispose(): void {
    if (this._disposed) return;
    this._disposed = true;
    InspectorPanel._panels.delete(InspectorPanel._targetKey(this._target));
  }

  /** Set the full HTML for the panel (called once on creation). */
  private _initHtml(initialContent: string): void {
    const nonce = crypto.randomBytes(16).toString("hex");
    const title =
      this._target.kind === "binding"
        ? `Binding: ${this._target.name}`
        : `Actor: ${this._target.info.class}  ${this._target.info.pid}`;

    this._panel.webview.html = buildPanelHtml(nonce, title, initialContent);
  }

  /**
   * Post an `update` message to the webview to replace the content area.
   * This preserves script state (e.g. scroll position) unlike a full HTML reset.
   */
  private _postContent(html: string): void {
    void this._panel.webview.postMessage({ type: "update", html });
  }

  /** Re-fetch live data from the workspace and refresh the content area. */
  private async _refetch(): Promise<void> {
    if (this._disposed) return;

    const generation = ++this._refreshGeneration;
    this._postContent(renderLoading());

    try {
      const content = await this._fetchContent();
      if (!this._disposed && generation === this._refreshGeneration) {
        this._postContent(content);
      }
    } catch (err) {
      if (!this._disposed && generation === this._refreshGeneration) {
        const message = err instanceof Error ? err.message : String(err);
        this._postContent(renderError(message));
      }
    }
  }

  private async _fetchContent(): Promise<string> {
    if (this._target.kind === "binding") {
      const client = this._client;
      const sessionId = this._target.sessionId;
      if (!client || !sessionId) {
        return renderValue(this._target.value);
      }
      const bindings = await client.bindings(sessionId);
      const value = (bindings as Record<string, unknown>)[this._target.name];
      return renderValue(value);
    }

    // Actor
    const client = this._client;
    if (!client) {
      return renderError("Not connected to workspace");
    }
    const state = await client.inspect(this._target.info.pid);
    return renderValue(state);
  }

  private static _targetKey(target: InspectTarget): string {
    return target.kind === "binding" ? `binding:${target.name}` : `actor:${target.info.pid}`;
  }

  private static _panelTitle(target: InspectTarget): string {
    return target.kind === "binding"
      ? `Inspect: ${target.name}`
      : `Inspect: ${target.info.class}  ${target.info.pid}`;
  }
}

// ─── HTML generation ──────────────────────────────────────────────────────────

/** Escape a string for safe inclusion in HTML text content. */
function escapeHtml(str: string): string {
  return str
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

/**
 * Render an arbitrary value as an HTML fragment.
 *
 * - Primitives (string, number, boolean, nil) → syntax-highlighted `<span>`.
 * - Arrays → indexed `<ol>` inside a `<details>` element (expandable).
 * - Objects/Dictionaries → `<dl>` of key→value pairs inside `<details>`.
 * - Nested objects are recursively expanded, open by default for the first
 *   two levels and collapsed for deeper nesting.
 */
export function renderValue(value: unknown, depth = 0): string {
  if (value === null || value === undefined) {
    return `<span class="bt-nil">nil</span>`;
  }

  if (typeof value === "string") {
    // Truncate very long strings to keep the panel readable.
    const display = value.length > 500 ? `${value.slice(0, 500)}…` : value;
    return `<span class="bt-string">'${escapeHtml(display)}'</span>`;
  }

  if (typeof value === "number") {
    return `<span class="bt-number">${value}</span>`;
  }

  if (typeof value === "boolean") {
    return `<span class="bt-boolean">${value}</span>`;
  }

  if (Array.isArray(value)) {
    if (value.length === 0) {
      return `<span class="bt-empty">[]</span>`;
    }
    const open = depth < 2 ? " open" : "";
    const items = value
      .map(
        (v, i) =>
          `<li><span class="bt-index">${i}</span><span class="bt-arrow"> → </span>${renderValue(v, depth + 1)}</li>`
      )
      .join("");
    return `<details${open}><summary><span class="bt-type">Array</span><span class="bt-count">(${value.length})</span></summary><ol class="bt-array">${items}</ol></details>`;
  }

  if (typeof value === "object") {
    const entries = Object.entries(value as Record<string, unknown>);
    if (entries.length === 0) {
      return `<span class="bt-empty">{}</span>`;
    }
    const open = depth < 2 ? " open" : "";
    const rows = entries
      .map(
        ([k, v]) =>
          `<div class="bt-entry"><dt class="bt-key">${escapeHtml(k)}</dt><dd class="bt-val">${renderValue(v, depth + 1)}</dd></div>`
      )
      .join("");
    return `<details${open}><summary><span class="bt-type">Dictionary</span><span class="bt-count">(${entries.length})</span></summary><dl class="bt-dict">${rows}</dl></details>`;
  }

  return `<span>${escapeHtml(String(value))}</span>`;
}

function renderLoading(): string {
  return `<p class="bt-loading">Loading…</p>`;
}

function renderError(message: string): string {
  return `<p class="bt-error">Error: ${escapeHtml(message)}</p>`;
}

// ─── Panel HTML template ──────────────────────────────────────────────────────

function buildPanelHtml(nonce: string, title: string, initialContent: string): string {
  return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'nonce-${nonce}';">
<style>
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

  body {
    font-family: var(--vscode-font-family, sans-serif);
    font-size: var(--vscode-font-size, 13px);
    color: var(--vscode-foreground);
    background: var(--vscode-editor-background);
    padding: 12px 16px;
  }

  /* ── Header ─────────────────────────────────────────────────────────── */
  #bt-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 12px;
    padding-bottom: 8px;
    border-bottom: 1px solid var(--vscode-editorWidget-border, #454545);
    gap: 8px;
  }

  #bt-title {
    font-weight: 600;
    font-size: 1.05em;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    color: var(--vscode-foreground);
  }

  #bt-refresh {
    flex-shrink: 0;
    padding: 3px 10px;
    background: var(--vscode-button-secondaryBackground, #3a3d41);
    color: var(--vscode-button-secondaryForeground, #cccccc);
    border: 1px solid var(--vscode-button-border, transparent);
    border-radius: 2px;
    cursor: pointer;
    font-size: 12px;
  }
  #bt-refresh:hover {
    background: var(--vscode-button-secondaryHoverBackground, #45494e);
  }

  /* ── Content ─────────────────────────────────────────────────────────── */
  #bt-content {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: var(--vscode-editor-font-size, 13px);
  }

  /* ── Value types ─────────────────────────────────────────────────────── */
  .bt-string  { color: var(--vscode-debugTokenExpression-string,  #ce9178); }
  .bt-number  { color: var(--vscode-debugTokenExpression-number,  #b5cea8); }
  .bt-boolean { color: var(--vscode-debugTokenExpression-boolean, #4fc1ff); }
  .bt-nil     { color: var(--vscode-debugTokenExpression-name,    #9cdcfe); font-style: italic; }
  .bt-empty   { color: var(--vscode-disabledForeground, #858585); }

  /* ── Containers ──────────────────────────────────────────────────────── */
  details { margin: 2px 0; }
  summary {
    cursor: pointer;
    user-select: none;
    list-style: none;
    display: flex;
    align-items: center;
    gap: 4px;
    padding: 1px 0;
  }
  summary::-webkit-details-marker { display: none; }
  summary::before {
    content: "▶";
    font-size: 0.7em;
    transition: transform 0.1s;
    color: var(--vscode-foreground);
    opacity: 0.6;
    width: 12px;
    text-align: center;
    flex-shrink: 0;
  }
  details[open] > summary::before { transform: rotate(90deg); }

  .bt-type  { color: var(--vscode-symbolIcon-classForeground, #ee9d28); font-weight: 600; }
  .bt-count { color: var(--vscode-disabledForeground, #858585); font-size: 0.9em; }

  /* ── Dictionary ──────────────────────────────────────────────────────── */
  dl.bt-dict { margin-left: 16px; margin-top: 2px; }
  .bt-entry  { display: flex; flex-wrap: wrap; align-items: baseline; gap: 4px; padding: 1px 0; }
  dt.bt-key  { color: var(--vscode-debugTokenExpression-name, #9cdcfe); flex-shrink: 0; }
  dt.bt-key::after { content: " →"; color: var(--vscode-disabledForeground, #858585); }
  dd.bt-val  { margin: 0; }

  /* ── Array ───────────────────────────────────────────────────────────── */
  ol.bt-array { margin-left: 16px; margin-top: 2px; list-style: none; }
  ol.bt-array li { display: flex; align-items: baseline; gap: 4px; padding: 1px 0; }
  .bt-index   { color: var(--vscode-disabledForeground, #858585); min-width: 24px; text-align: right; flex-shrink: 0; }
  .bt-arrow   { color: var(--vscode-disabledForeground, #858585); flex-shrink: 0; }

  /* ── States ──────────────────────────────────────────────────────────── */
  .bt-loading { color: var(--vscode-disabledForeground, #858585); font-style: italic; }
  .bt-error   { color: var(--vscode-errorForeground, #f44747); }
</style>
</head>
<body>
<div id="bt-header">
  <span id="bt-title">${escapeHtml(title)}</span>
  <button id="bt-refresh">↻ Refresh</button>
</div>
<div id="bt-content">${initialContent}</div>
<script nonce="${nonce}">
  const vscode = acquireVsCodeApi();

  document.getElementById('bt-refresh').addEventListener('click', function() {
    vscode.postMessage({ type: 'refresh' });
  });

  window.addEventListener('message', function(event) {
    const msg = event.data;
    if (msg && msg.type === 'update') {
      document.getElementById('bt-content').innerHTML = msg.html;
    }
  });
</script>
</body>
</html>`;
}

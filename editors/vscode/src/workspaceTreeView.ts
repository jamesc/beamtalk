// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import type {
  ActorInfo,
  BindingsMap,
  ClassInfo,
  ConnectionState,
  PushEvent,
  WorkspaceClient,
} from "./workspaceClient";

// ─── Node Types ───────────────────────────────────────────────────────────────

export interface ConnectedRootNode {
  readonly kind: "connected-root";
}

export interface DisconnectedRootNode {
  readonly kind: "disconnected-root";
}

export interface BindingsSectionNode {
  readonly kind: "bindings-section";
}

export interface ActorsSectionNode {
  readonly kind: "actors-section";
}

export interface ClassesSectionNode {
  readonly kind: "classes-section";
}

export interface BindingItemNode {
  readonly kind: "binding-item";
  readonly name: string;
  readonly value: unknown;
}

export interface ActorItemNode {
  readonly kind: "actor-item";
  readonly info: ActorInfo;
}

export interface ClassItemNode {
  readonly kind: "class-item";
  readonly info: ClassInfo;
}

export interface InspectFieldNode {
  readonly kind: "inspect-field";
  readonly key: string;
  readonly value: unknown;
  readonly parentId: string;
}

export type WorkspaceNode =
  | ConnectedRootNode
  | DisconnectedRootNode
  | BindingsSectionNode
  | ActorsSectionNode
  | ClassesSectionNode
  | BindingItemNode
  | ActorItemNode
  | ClassItemNode
  | InspectFieldNode;

// ─── Singleton section nodes (stable references for onDidChangeTreeData) ─────

const CONNECTED_ROOT: ConnectedRootNode = { kind: "connected-root" };
const DISCONNECTED_ROOT: DisconnectedRootNode = { kind: "disconnected-root" };
const BINDINGS_SECTION: BindingsSectionNode = { kind: "bindings-section" };
const ACTORS_SECTION: ActorsSectionNode = { kind: "actors-section" };
const CLASSES_SECTION: ClassesSectionNode = { kind: "classes-section" };

// ─── WorkspaceTreeDataProvider ────────────────────────────────────────────────

/**
 * TreeDataProvider for the Beamtalk Workspace Explorer sidebar view (ADR 0046).
 *
 * Displays live workspace state — bindings, actors, and loaded classes — as a
 * native VSCode TreeView. Updates are event-driven via WorkspaceClient push
 * channels: no polling.
 *
 * Usage:
 *   const provider = new WorkspaceTreeDataProvider();
 *   vscode.window.registerTreeDataProvider("beamtalk.workspaceExplorer", provider);
 *   provider.setClient(client); // called by auto-connect logic (BT-1024)
 */
export class WorkspaceTreeDataProvider
  implements vscode.TreeDataProvider<WorkspaceNode>, vscode.Disposable
{
  private readonly _onDidChangeTreeData = new vscode.EventEmitter<
    WorkspaceNode | undefined | null
  >();
  readonly onDidChangeTreeData: vscode.Event<WorkspaceNode | undefined | null> =
    this._onDidChangeTreeData.event;

  private client: WorkspaceClient | null = null;
  private connectionState: ConnectionState = "disconnected";
  private bindings: BindingsMap = {};
  private actors: ActorInfo[] = [];
  private classes: ClassInfo[] = [];
  private disposed = false;

  /** Cached inspect results keyed by "actor:<pid>". */
  private readonly inspectCache = new Map<string, Record<string, unknown>>();

  private readonly disposeHandlers: Array<() => void> = [];

  /** Monotonic counter to discard stale class-list fetches. */
  private classFetchGeneration = 0;

  /** Monotonic counter to discard stale _fetchInitialData calls. */
  private initialFetchGeneration = 0;

  // ─── Public API ──────────────────────────────────────────────────────────

  /**
   * Attach a WorkspaceClient and begin listening for push events.
   * Pass null to detach and show a disconnected tree.
   */
  setClient(client: WorkspaceClient | null): void {
    // Remove previous event handlers
    for (const dispose of this.disposeHandlers) {
      dispose();
    }
    this.disposeHandlers.length = 0;

    this.client = client;

    if (!client) {
      this._resetState("disconnected");
      return;
    }

    // React to connection state changes
    const disposeConn = client.onConnectionChange((state) => {
      this.connectionState = state;
      if (state === "connected") {
        void this._fetchInitialData(client);
      } else if (state === "disconnected") {
        this._resetState("disconnected");
      } else {
        // reconnecting — keep stale data visible but fire to update description
        this._onDidChangeTreeData.fire(undefined);
      }
    });
    this.disposeHandlers.push(disposeConn);

    // React to push events from the workspace
    const disposePush = client.onPush((event) => {
      this._handlePush(event, client);
    });
    this.disposeHandlers.push(disposePush);

    // If the client already has a session (connected before setClient was called)
    // fetch initial data immediately rather than waiting for the next state change.
    if (client.currentSessionId) {
      this.connectionState = "connected";
      void this._fetchInitialData(client);
    }
  }

  /**
   * Explicitly refresh the bindings section.
   * Call this after a REPL eval completes so the tree reflects new bindings.
   */
  async refreshBindings(): Promise<void> {
    if (!this.client || this.connectionState !== "connected") {
      return;
    }
    const sessionId = this.client.currentSessionId;
    if (!sessionId) {
      return;
    }
    try {
      this.bindings = await this.client.bindings(sessionId);
      this._onDidChangeTreeData.fire(BINDINGS_SECTION);
    } catch {
      // Ignore transient refresh errors
    }
  }

  /** Refresh all sections by re-fetching actors, classes, and bindings. */
  async refresh(): Promise<void> {
    if (!this.client || this.connectionState !== "connected") {
      return;
    }
    this.inspectCache.clear();
    await this._fetchInitialData(this.client);
  }

  dispose(): void {
    if (this.disposed) return;
    this.disposed = true;
    this.client = null;
    for (const dispose of this.disposeHandlers) {
      dispose();
    }
    this.disposeHandlers.length = 0;
    this._onDidChangeTreeData.dispose();
  }

  // ─── TreeDataProvider ────────────────────────────────────────────────────

  getTreeItem(element: WorkspaceNode): vscode.TreeItem {
    switch (element.kind) {
      case "connected-root":
        return this._connectedRootItem();
      case "disconnected-root":
        return this._disconnectedRootItem();
      case "bindings-section":
        return this._bindingsSectionItem();
      case "actors-section":
        return this._actorsSectionItem();
      case "classes-section":
        return this._classesSectionItem();
      case "binding-item":
        return this._bindingItem(element);
      case "actor-item":
        return this._actorItem(element);
      case "class-item":
        return this._classItem(element);
      case "inspect-field":
        return this._inspectFieldItem(element);
    }
  }

  async getChildren(element?: WorkspaceNode): Promise<WorkspaceNode[]> {
    // Root level: show stale data while reconnecting so users don't lose context
    if (!element) {
      return this.connectionState !== "disconnected" ? [CONNECTED_ROOT] : [DISCONNECTED_ROOT];
    }

    switch (element.kind) {
      case "connected-root":
        return [BINDINGS_SECTION, ACTORS_SECTION, CLASSES_SECTION];

      case "bindings-section":
        return Object.entries(this.bindings)
          .sort(([a], [b]) => a.localeCompare(b))
          .map(([name, value]) => ({
            kind: "binding-item" as const,
            name,
            value,
          }));

      case "actors-section":
        return this.actors.map((info) => ({ kind: "actor-item" as const, info }));

      case "classes-section":
        return this.classes.map((info) => ({ kind: "class-item" as const, info }));

      case "actor-item": {
        const cacheKey = `actor:${element.info.pid}`;
        const cached = this.inspectCache.get(cacheKey);
        if (cached) {
          return this._inspectFields(cached, cacheKey);
        }
        const activeClient = this.client;
        if (!activeClient || this.connectionState !== "connected") {
          return [];
        }
        try {
          const state = await activeClient.inspect(element.info.pid);
          // Guard: client may have changed while inspect was in-flight
          if (this.client !== activeClient) return [];
          this.inspectCache.set(cacheKey, state);
          return this._inspectFields(state, cacheKey);
        } catch {
          return [];
        }
      }

      default:
        return [];
    }
  }

  // ─── Private: TreeItem builders ──────────────────────────────────────────

  private _connectedRootItem(): vscode.TreeItem {
    const item = new vscode.TreeItem("Workspace", vscode.TreeItemCollapsibleState.Expanded);
    if (this.connectionState === "reconnecting") {
      item.description = "◌ Reconnecting…";
      item.iconPath = new vscode.ThemeIcon("circle-outline");
    } else {
      item.description = "● Connected";
      item.iconPath = new vscode.ThemeIcon(
        "circle-filled",
        new vscode.ThemeColor("testing.iconPassed")
      );
    }
    item.contextValue = "connected-root";
    return item;
  }

  private _disconnectedRootItem(): vscode.TreeItem {
    const item = new vscode.TreeItem("Workspace", vscode.TreeItemCollapsibleState.None);
    item.description = "○ Disconnected";
    item.iconPath = new vscode.ThemeIcon("circle-outline");
    item.contextValue = "disconnected-root";
    return item;
  }

  private _bindingsSectionItem(): vscode.TreeItem {
    const count = Object.keys(this.bindings).length;
    const item = new vscode.TreeItem(
      "Bindings",
      count > 0 ? vscode.TreeItemCollapsibleState.Expanded : vscode.TreeItemCollapsibleState.None
    );
    item.description = count > 0 ? `(${count})` : "";
    item.iconPath = new vscode.ThemeIcon("symbol-variable");
    item.contextValue = "bindings-section";
    return item;
  }

  private _actorsSectionItem(): vscode.TreeItem {
    const count = this.actors.length;
    const item = new vscode.TreeItem(
      "Actors",
      count > 0 ? vscode.TreeItemCollapsibleState.Expanded : vscode.TreeItemCollapsibleState.None
    );
    item.description = count > 0 ? `(${count} running)` : "(none)";
    item.iconPath = new vscode.ThemeIcon("pulse");
    item.contextValue = "actors-section";
    return item;
  }

  private _classesSectionItem(): vscode.TreeItem {
    const count = this.classes.length;
    // Collapsed by default per ADR 0046 (avoid information overload for newcomers)
    const item = new vscode.TreeItem("Classes", vscode.TreeItemCollapsibleState.Collapsed);
    item.description = count > 0 ? `(${count} loaded)` : "(none)";
    item.iconPath = new vscode.ThemeIcon("symbol-class");
    item.contextValue = "classes-section";
    return item;
  }

  private _bindingItem(node: BindingItemNode): vscode.TreeItem {
    const item = new vscode.TreeItem(node.name, vscode.TreeItemCollapsibleState.None);
    item.description = this._displayValue(node.value);
    item.iconPath = new vscode.ThemeIcon("symbol-constant");
    item.contextValue = "binding-item";
    item.tooltip = `${node.name}: ${this._displayValue(node.value)}`;
    item.command = {
      command: "beamtalk.inspectBinding",
      title: "Inspect",
      arguments: [node],
    };
    return item;
  }

  private _actorItem(node: ActorItemNode): vscode.TreeItem {
    const label = `${node.info.class}  ${node.info.pid}`;
    const item = new vscode.TreeItem(label, vscode.TreeItemCollapsibleState.Collapsed);
    item.iconPath = new vscode.ThemeIcon("vm");
    item.contextValue = "actor-item";
    item.tooltip = new vscode.MarkdownString(
      `**Actor** \`${node.info.class}\`\n\nPID: \`${node.info.pid}\`${
        node.info.spawned_at
          ? `\n\nSpawned: ${new Date(node.info.spawned_at * 1000).toLocaleTimeString()}`
          : ""
      }`
    );
    return item;
  }

  private _classItem(node: ClassItemNode): vscode.TreeItem {
    const item = new vscode.TreeItem(node.info.name, vscode.TreeItemCollapsibleState.None);
    item.iconPath = new vscode.ThemeIcon("symbol-class");
    item.contextValue = "class-item";
    item.tooltip = node.info.source_file ? `Source: ${node.info.source_file}` : node.info.name;
    if (node.info.actor_count !== undefined && node.info.actor_count > 0) {
      item.description = `${node.info.actor_count} instance${node.info.actor_count !== 1 ? "s" : ""}`;
    }
    return item;
  }

  private _inspectFieldItem(node: InspectFieldNode): vscode.TreeItem {
    const item = new vscode.TreeItem(node.key, vscode.TreeItemCollapsibleState.None);
    item.description = this._displayValue(node.value);
    item.iconPath = new vscode.ThemeIcon("symbol-field");
    item.contextValue = "inspect-field";
    item.tooltip = `${node.key}: ${this._displayValue(node.value)}`;
    return item;
  }

  // ─── Private: helpers ────────────────────────────────────────────────────

  private _inspectFields(state: Record<string, unknown>, parentId: string): InspectFieldNode[] {
    return Object.entries(state).map(([key, value]) => ({
      kind: "inspect-field" as const,
      key,
      value,
      parentId,
    }));
  }

  /** Format an unknown value for display as a TreeItem description. */
  private _displayValue(value: unknown): string {
    if (value === null || value === undefined) {
      return "nil";
    }
    if (typeof value === "string") {
      return `'${value}'`;
    }
    if (typeof value === "number" || typeof value === "boolean") {
      return String(value);
    }
    if (Array.isArray(value)) {
      return `Array(${value.length})`;
    }
    if (typeof value === "object") {
      const keys = Object.keys(value as object);
      return keys.length > 0
        ? `{${keys.slice(0, 3).join(", ")}${keys.length > 3 ? "…" : ""}}`
        : "{}";
    }
    return String(value);
  }

  private _resetState(state: ConnectionState): void {
    this.initialFetchGeneration++;
    this.connectionState = state;
    this.bindings = {};
    this.actors = [];
    this.classes = [];
    this.inspectCache.clear();
    this._onDidChangeTreeData.fire(undefined);
  }

  /** Fetch bindings, actors, and classes from a newly-connected client. */
  private async _fetchInitialData(client: WorkspaceClient): Promise<void> {
    const gen = ++this.initialFetchGeneration;
    const sessionId = client.currentSessionId;
    const [bindingsResult, actorsResult, classesResult] = await Promise.allSettled([
      sessionId ? client.bindings(sessionId) : Promise.resolve<BindingsMap>({}),
      client.actors(),
      client.classes(),
    ]);

    // Guard: client may have been detached, or a newer fetch superseded this one.
    if (this.client !== client || this.initialFetchGeneration !== gen) {
      return;
    }

    if (bindingsResult.status === "fulfilled") {
      this.bindings = bindingsResult.value;
    }
    if (actorsResult.status === "fulfilled") {
      this.actors = actorsResult.value;
    }
    if (classesResult.status === "fulfilled") {
      this.classes = classesResult.value;
    }

    this._onDidChangeTreeData.fire(undefined);
  }

  private _handlePush(event: PushEvent, client: WorkspaceClient): void {
    if (event.channel === "actors") {
      if (event.event === "spawned") {
        // Guard against duplicate spawned events
        if (!this.actors.some((a) => a.pid === event.data.pid)) {
          this.actors.push(event.data);
        }
        this.inspectCache.delete(`actor:${event.data.pid}`);
        this._onDidChangeTreeData.fire(ACTORS_SECTION);
      } else if (event.event === "stopped") {
        this.actors = this.actors.filter((a) => a.pid !== event.data.pid);
        this.inspectCache.delete(`actor:${event.data.pid}`);
        this._onDidChangeTreeData.fire(ACTORS_SECTION);
      }
    } else if (event.channel === "classes" && event.event === "loaded") {
      // Re-fetch the full class list — the event only carries the new class name,
      // not the complete list with actor_count metadata.
      // Use a generation counter to discard stale responses when multiple
      // class-loaded events arrive in quick succession.
      const gen = ++this.classFetchGeneration;
      void client
        .classes()
        .then((classes) => {
          if (this.client !== client || this.classFetchGeneration !== gen) return;
          this.classes = classes;
          this._onDidChangeTreeData.fire(CLASSES_SECTION);
        })
        .catch(() => {
          // Transient failure: next class-loaded event will retry.
        });
    }
  }
}

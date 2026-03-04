// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { extractMethodDocComment, extractStateVarInfo } from "./textUtils";
import type {
  ActorInfo,
  BindingsMap,
  ClassInfo,
  ConnectionState,
  MethodInfo,
  StateVarInfo,
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

export interface MethodGroupNode {
  readonly kind: "method-group";
  readonly side: "instance" | "class";
  readonly classInfo: ClassInfo;
  readonly methods: MethodItemNode[];
}

export interface MethodItemNode {
  readonly kind: "method-item";
  readonly method: MethodInfo;
  readonly classInfo: ClassInfo;
}

export interface StateGroupNode {
  readonly kind: "state-group";
  readonly classInfo: ClassInfo;
  readonly stateVars: StateVarItemNode[];
}

export interface StateVarItemNode {
  readonly kind: "state-item";
  readonly stateVar: StateVarInfo;
  readonly classInfo: ClassInfo;
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
  | StateGroupNode
  | StateVarItemNode
  | MethodGroupNode
  | MethodItemNode
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
  /** Active session ID captured from terminal output; used to query session bindings. */
  private sessionId: string | null = null;

  /** Cached inspect results keyed by "actor:<pid>". */
  private readonly inspectCache = new Map<string, Record<string, unknown>>();

  /** Cached methods+stateVars results keyed by class name. */
  private readonly methodsCache = new Map<
    string,
    { methods: MethodInfo[]; stateVars: StateVarInfo[] }
  >();

  private readonly disposeHandlers: Array<() => void> = [];

  /** Monotonic counter to discard stale class-list fetches. */
  private classFetchGeneration = 0;

  /** Monotonic counter to discard stale _fetchInitialData calls. */
  private initialFetchGeneration = 0;

  // ─── Public API ──────────────────────────────────────────────────────────

  /** The active session ID, or null if no session is running. */
  get currentSessionId(): string | null {
    return this.sessionId;
  }

  /**
   * Set the session ID captured from `beamtalk repl` stdout.
   * This session ID is used for bindings queries so the sidebar shows
   * the session's variables rather than the extension's own (empty) session.
   * Pass null to clear (e.g. when the session terminal is closed).
   */
  setSessionId(id: string | null): void {
    const wasAttached = this.sessionId !== null;
    this.sessionId = id;
    if (this.client && this.connectionState === "connected") {
      const isAttached = id !== null;
      if (isAttached !== wasAttached) {
        // Bindings section appears or disappears — refresh the whole root.
        this._onDidChangeTreeData.fire(CONNECTED_ROOT);
      }
      void this.refreshBindings();
    }
  }

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
      this.sessionId = null;
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
    const activeClient = this.client;
    // Prefer the active session ID (for the user's variables) over the extension's
    // own WS session (which has no bindings — no code is eval'd through it).
    const sessionId = this.sessionId ?? activeClient.currentSessionId;
    if (!sessionId) {
      return;
    }
    try {
      const nextBindings = await activeClient.bindings(sessionId);
      // Guard: client or connection may have changed while awaiting.
      // Re-derive the effective session ID to check for staleness.
      const currentEffective = this.sessionId ?? activeClient.currentSessionId;
      if (
        this.client !== activeClient ||
        this.connectionState !== "connected" ||
        currentEffective !== sessionId
      ) {
        return;
      }
      this.bindings = nextBindings;
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
    this.methodsCache.clear();
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
      case "state-group":
        return this._stateGroupItem(element);
      case "state-item":
        return this._stateVarItem(element);
      case "method-group":
        return this._methodGroupItem(element);
      case "method-item":
        return this._methodItem(element);
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
        return [
          ...(this.sessionId !== null ? [BINDINGS_SECTION] : []),
          ACTORS_SECTION,
          CLASSES_SECTION,
        ];

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

      case "class-item": {
        const cached = this.methodsCache.get(element.info.name);
        if (cached) {
          return this._classChildren(cached, element.info);
        }
        const activeClient = this.client;
        if (!activeClient || this.connectionState !== "connected") {
          return [];
        }
        try {
          const result = await activeClient.methods(element.info.name);
          // Guard: client may have changed while methods() was in-flight
          if (this.client !== activeClient) return [];
          this.methodsCache.set(element.info.name, result);
          return this._classChildren(result, element.info);
        } catch {
          return [];
        }
      }

      case "state-group":
        return element.stateVars;

      case "method-group":
        return element.methods;

      default:
        return [];
    }
  }

  // ─── resolveTreeItem (lazy hover tooltips) ───────────────────────────────

  async resolveTreeItem(
    item: vscode.TreeItem,
    element: WorkspaceNode,
    _token: vscode.CancellationToken
  ): Promise<vscode.TreeItem | undefined> {
    if (element.kind === "class-item") {
      item.tooltip =
        (await this._lspHoverTooltip(element.info.source_file, element.info.name, "class")) ??
        this._classTooltipFallback(element.info);
      return item;
    }
    if (element.kind === "method-item") {
      item.tooltip =
        (await this._lspHoverTooltip(
          element.classInfo.source_file,
          element.method.selector,
          element.method.side === "class" ? "class-method" : "method"
        )) ??
        (await this._methodDocCommentTooltip(element)) ??
        this._methodTooltipFallback(element.method);
      return item;
    }
    if (element.kind === "state-item") {
      item.tooltip = await this._stateVarTooltip(element);
      return item;
    }
    return undefined;
  }

  private async _lspHoverTooltip(
    sourceFile: string | undefined,
    symbol: string,
    kind: "class" | "method" | "class-method" | "field"
  ): Promise<vscode.MarkdownString | undefined> {
    if (!sourceFile || sourceFile === "unknown") return undefined;
    try {
      const uri = vscode.Uri.file(sourceFile);
      await vscode.workspace.openTextDocument(uri);

      // Use the LSP document symbol provider to find the exact position — no regex.
      const docSymbols = await vscode.commands.executeCommand<
        vscode.DocumentSymbol[] | vscode.SymbolInformation[]
      >("vscode.executeDocumentSymbolProvider", uri);

      const pos = this._findSymbolPosition(docSymbols ?? [], symbol, kind);
      if (!pos) return undefined;

      const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
        "vscode.executeHoverProvider",
        uri,
        pos
      );
      if (!hovers || hovers.length === 0) return undefined;
      const md = new vscode.MarkdownString();
      for (const hover of hovers) {
        const contents = Array.isArray(hover.contents) ? hover.contents : [hover.contents];
        for (const c of contents) {
          if (typeof c === "string") md.appendMarkdown(c);
          else if (c && "value" in c && c.value) md.appendMarkdown(c.value);
        }
      }
      return md.value ? md : undefined;
    } catch {
      return undefined;
    }
  }

  /** Find the position of a class, method, or field symbol from document symbols. */
  private _findSymbolPosition(
    symbols: vscode.DocumentSymbol[] | vscode.SymbolInformation[],
    name: string,
    kind: "class" | "method" | "class-method" | "field"
  ): vscode.Position | undefined {
    const targetKind =
      kind === "class"
        ? [vscode.SymbolKind.Class]
        : kind === "field"
          ? [vscode.SymbolKind.Field]
          : [vscode.SymbolKind.Method, vscode.SymbolKind.Function];

    for (const sym of symbols) {
      // Class symbols are named "ClassName (class)" per ADR 0013 — strip the suffix for matching.
      const symBaseName =
        sym.kind === vscode.SymbolKind.Class ? sym.name.replace(/ \(class\)$/, "") : sym.name;
      if (targetKind.includes(sym.kind) && symBaseName === name) {
        if ("range" in sym) {
          // DocumentSymbol
          return sym.selectionRange.start;
        } else {
          // SymbolInformation
          return sym.location.range.start;
        }
      }
      // Recurse into children (DocumentSymbol only)
      if ("children" in sym && sym.children.length > 0) {
        const found = this._findSymbolPosition(sym.children, name, kind);
        if (found) return found;
      }
    }
    return undefined;
  }

  private _classTooltipFallback(info: ClassInfo): vscode.MarkdownString {
    const md = new vscode.MarkdownString(`**${info.name}**`);
    if (info.actor_count !== undefined && info.actor_count > 0) {
      md.appendMarkdown(
        `\n\n${info.actor_count} running instance${info.actor_count !== 1 ? "s" : ""}`
      );
    }
    return md;
  }

  private _methodTooltipFallback(method: MethodInfo): vscode.MarkdownString {
    return new vscode.MarkdownString(
      `**${method.selector}**\n\n_${method.side === "instance" ? "instance" : "class"} method_`
    );
  }

  /**
   * Read source text and extract `///` doc comment for the method.
   * Used as a fallback when LSP hover is unavailable (file not open in editor).
   */
  private async _methodDocCommentTooltip(
    element: MethodItemNode
  ): Promise<vscode.MarkdownString | undefined> {
    const sourceFile = element.classInfo.source_file;
    if (!sourceFile || sourceFile === "unknown") return undefined;
    try {
      const doc = await vscode.workspace.openTextDocument(vscode.Uri.file(sourceFile));
      const comment = extractMethodDocComment(
        doc.getText(),
        element.method.selector,
        element.method.side
      );
      if (!comment) return undefined;
      const md = new vscode.MarkdownString(comment);
      return md;
    } catch {
      return undefined;
    }
  }

  /**
   * Build a tooltip for a state variable item by reading its declaration from source.
   * Shows the default value and inline comment (e.g. `// List of parameter name strings`).
   */
  private async _stateVarTooltip(element: StateVarItemNode): Promise<vscode.MarkdownString> {
    const { classInfo, stateVar } = element;
    const fallback = new vscode.MarkdownString(`**${stateVar.name}**\n\n_state variable_`);
    if (!classInfo.source_file || classInfo.source_file === "unknown") return fallback;
    try {
      const uri = vscode.Uri.file(classInfo.source_file);
      const doc = await vscode.workspace.openTextDocument(uri);
      const info = extractStateVarInfo(doc.getText(), stateVar.name);
      if (!info) return fallback;
      const md = new vscode.MarkdownString(`**${stateVar.name}**`);
      if (info.defaultValue !== undefined) {
        md.appendMarkdown(`\n\nDefault: \`${info.defaultValue}\``);
      }
      if (info.comment) {
        md.appendMarkdown(`\n\n${info.comment}`);
      }
      return md;
    } catch {
      return fallback;
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
    const item = new vscode.TreeItem(node.info.name, vscode.TreeItemCollapsibleState.Collapsed);
    item.iconPath = new vscode.ThemeIcon("symbol-class");
    const hasSource = !!node.info.source_file && node.info.source_file !== "unknown";
    item.contextValue = hasSource ? "class-item" : "class-item-no-source";
    if (node.info.actor_count !== undefined && node.info.actor_count > 0) {
      item.description = `${node.info.actor_count} instance${node.info.actor_count !== 1 ? "s" : ""}`;
    }
    return item;
  }

  private _methodGroupItem(node: MethodGroupNode): vscode.TreeItem {
    const label = node.side === "instance" ? "Instance Methods" : "Class Methods";
    const state =
      node.methods.length > 0
        ? vscode.TreeItemCollapsibleState.Expanded
        : vscode.TreeItemCollapsibleState.None;
    const item = new vscode.TreeItem(label, state);
    item.iconPath = new vscode.ThemeIcon(
      node.side === "instance" ? "symbol-method" : "symbol-namespace"
    );
    item.description = node.methods.length > 0 ? `(${node.methods.length})` : "(none)";
    item.contextValue = "method-group";
    return item;
  }

  private _methodItem(node: MethodItemNode): vscode.TreeItem {
    const item = new vscode.TreeItem(node.method.selector, vscode.TreeItemCollapsibleState.None);
    item.iconPath = new vscode.ThemeIcon("symbol-method");
    const hasSource = !!node.classInfo.source_file && node.classInfo.source_file !== "unknown";
    item.contextValue = hasSource ? "method-item" : "method-item-no-source";
    if (hasSource) {
      item.command = {
        command: "beamtalk.navigateToMethod",
        title: "Go to Definition",
        arguments: [node],
      };
    }
    return item;
  }

  private _stateGroupItem(node: StateGroupNode): vscode.TreeItem {
    const count = node.stateVars.length;
    const state =
      count > 0 ? vscode.TreeItemCollapsibleState.Expanded : vscode.TreeItemCollapsibleState.None;
    const item = new vscode.TreeItem("State", state);
    item.iconPath = new vscode.ThemeIcon("symbol-field");
    item.description = count > 0 ? `(${count})` : "(none)";
    item.contextValue = "state-group";
    return item;
  }

  private _stateVarItem(node: StateVarItemNode): vscode.TreeItem {
    const item = new vscode.TreeItem(node.stateVar.name, vscode.TreeItemCollapsibleState.None);
    item.iconPath = new vscode.ThemeIcon("symbol-field");
    const hasSource = !!node.classInfo.source_file && node.classInfo.source_file !== "unknown";
    item.contextValue = hasSource ? "state-item" : "state-item-no-source";
    if (hasSource) {
      item.command = {
        command: "beamtalk.navigateToStateVar",
        title: "Go to Definition",
        arguments: [node],
      };
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

  private _classChildren(
    result: { methods: MethodInfo[]; stateVars: StateVarInfo[] },
    classInfo: ClassInfo
  ): WorkspaceNode[] {
    const { methods, stateVars } = result;
    const instance = methods.filter((m) => m.side === "instance");
    const classSide = methods.filter((m) => m.side === "class");
    const toMethodItems = (ms: MethodInfo[]): MethodItemNode[] =>
      ms.map((m) => ({ kind: "method-item" as const, method: m, classInfo }));
    const stateVarItems: StateVarItemNode[] = stateVars.map((sv) => ({
      kind: "state-item" as const,
      stateVar: sv,
      classInfo,
    }));
    return [
      { kind: "state-group" as const, classInfo, stateVars: stateVarItems },
      {
        kind: "method-group" as const,
        side: "instance",
        classInfo,
        methods: toMethodItems(instance),
      },
      {
        kind: "method-group" as const,
        side: "class",
        classInfo,
        methods: toMethodItems(classSide),
      },
    ];
  }

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
    this.classFetchGeneration++;
    this.connectionState = state;
    this.bindings = {};
    this.actors = [];
    this.classes = [];
    this.inspectCache.clear();
    this.methodsCache.clear();
    this._onDidChangeTreeData.fire(undefined);
  }

  /** Fetch bindings, actors, and classes from a newly-connected client. */
  private async _fetchInitialData(client: WorkspaceClient): Promise<void> {
    const gen = ++this.initialFetchGeneration;
    // Prefer the active session ID (for the user's variables) over the extension's own WS session.
    const sessionId = this.sessionId ?? client.currentSessionId;
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
      // Only apply bindings if the effective session ID hasn't changed while we
      // were awaiting. If setSessionId() was called during the fetch, a
      // refreshBindings() call has already applied the correct bindings; applying
      // these (stale, empty-session) results would overwrite them incorrectly.
      const currentEffectiveSession = this.sessionId ?? client.currentSessionId;
      if (currentEffectiveSession === sessionId) {
        this.bindings = bindingsResult.value;
      }
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
    if (event.channel === "bindings" && event.event === "changed") {
      // Server emits this after every successful eval — refresh only if the event
      // belongs to the session we're currently tracking.
      const effectiveSession = this.sessionId ?? client.currentSessionId;
      if (!effectiveSession || event.data.session === effectiveSession) {
        void this.refreshBindings();
      }
    } else if (event.channel === "actors") {
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
      // Invalidate cached methods for the reloaded class — its methods may have changed.
      this.methodsCache.delete(event.data.class);
      // Re-fetch the full class list — the event only carries the new class name,
      // not the complete list with actor_count metadata.
      // Use a generation counter to discard stale responses when multiple
      // class-loaded events arrive in quick succession.
      const gen = ++this.classFetchGeneration;
      void client
        .classes()
        .then((classes) => {
          if (
            this.client !== client ||
            this.connectionState !== "connected" ||
            this.classFetchGeneration !== gen
          )
            return;
          this.classes = classes;
          this._onDidChangeTreeData.fire(CLASSES_SECTION);
        })
        .catch(() => {
          // Transient failure: next class-loaded event will retry.
        });
    }
  }
}

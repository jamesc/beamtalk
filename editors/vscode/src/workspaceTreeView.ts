// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { GenerationTracker } from "./generationTracker";
import {
  type DeclarationRef,
  findSymbolPosition,
  resolveDeclarationOffsetSync,
} from "./symbolLookup";
import {
  extractMethodDocComment,
  extractStateVarDocComment,
  extractStateVarInfo,
} from "./textUtils";
import type {
  ActorInfo,
  BindingsMap,
  ClassInfo,
  ClassOrigin,
  ConnectionState,
  InheritedMethodInfo,
  MethodInfo,
  PushEvent,
  StateVarInfo,
  TypeAliasInfo,
  WorkspaceClient,
} from "./workspaceClient";

/** Every class origin the Workspace Explorer can filter by (BT-2552 badges). */
export const ALL_CLASS_ORIGINS: readonly ClassOrigin[] = ["project", "dependency", "stdlib"];

/**
 * True for a `source_status` that the backend documents as having no
 * openable source at all — `synthetic` (a compiler-generated accessor) or
 * `unindexed_runtime_fun` (a native/runtime-only method, e.g. one injected
 * for every `Actor subclass:` to support supervision — see
 * beamtalk_repl_ops_browse.erl's own "no openable source" doc comment).
 * Neither can be hovered or navigated to via source-text search: there is
 * no declaration anywhere to find.
 */
function hasNoOpenableSource(status: MethodInfo["source_status"]): boolean {
  return status === "synthetic" || status === "unindexed_runtime_fun";
}

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

/** ADR 0108 Phase 8 (BT-2903): the "Type Aliases" section, sibling to Classes. */
export interface TypeAliasesSectionNode {
  readonly kind: "type-aliases-section";
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

/**
 * ADR 0108 Phase 8 (BT-2903): one `type` alias declaration. Unlike
 * `ClassItemNode`, this is always a leaf — an alias produces no BEAM module,
 * so it has no methods/state to expand into.
 */
export interface TypeAliasItemNode {
  readonly kind: "type-alias-item";
  readonly info: TypeAliasInfo;
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
  /**
   * BT-3478: set only for an entry under an `InheritedMethodGroupNode` — the
   * ancestor class that actually declares the method (shown in the item's
   * description/tooltip). `classInfo` above is that same defining class's
   * info (so "Go to Definition" opens its real source, not the receiving
   * subclass's), not the class the "Inherited" group is nested under.
   */
  readonly definingClass?: string;
}

/**
 * One of the two collapsed-by-default "Inherited" groups (BT-3478), sibling
 * to the existing `MethodGroupNode`s — same tree depth, not a per-superclass
 * sub-tree. Unlike `MethodGroupNode`, this carries no embedded methods: its
 * children are fetched lazily, only when the group itself is expanded, via
 * the `inherited-methods` op (kept off the eager per-class-item `methods`
 * fetch on purpose).
 */
export interface InheritedMethodGroupNode {
  readonly kind: "inherited-method-group";
  readonly side: "instance" | "class";
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
  | TypeAliasesSectionNode
  | BindingItemNode
  | ActorItemNode
  | ClassItemNode
  | TypeAliasItemNode
  | StateGroupNode
  | StateVarItemNode
  | MethodGroupNode
  | MethodItemNode
  | InheritedMethodGroupNode
  | InspectFieldNode;

// ─── Singleton section nodes (stable references for onDidChangeTreeData) ─────

const CONNECTED_ROOT: ConnectedRootNode = { kind: "connected-root" };
const DISCONNECTED_ROOT: DisconnectedRootNode = { kind: "disconnected-root" };
const BINDINGS_SECTION: BindingsSectionNode = { kind: "bindings-section" };
const ACTORS_SECTION: ActorsSectionNode = { kind: "actors-section" };
const CLASSES_SECTION: ClassesSectionNode = { kind: "classes-section" };
const TYPE_ALIASES_SECTION: TypeAliasesSectionNode = { kind: "type-aliases-section" };

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
  /**
   * Opens a stdlib class's source via the LSP's `beamtalk-stdlib://` virtual
   * URI scheme (`openStdlibDocumentForClass` in extension.ts), injected at
   * activation since it needs the `LanguageClient`, not the workspace
   * protocol `client` above. Without this, every hover/doc-comment lookup
   * for a class whose `source_file` the runtime never tracks (all
   * compiled-in stdlib classes) has nothing to read and falls straight to
   * the hardcoded fallback tooltip — see `_resolveClassDocument`.
   */
  private stdlibDocumentOpener:
    | ((classInfo: ClassInfo) => Promise<vscode.TextDocument | undefined>)
    | null = null;
  private connectionState: ConnectionState = "disconnected";
  private bindings: BindingsMap = {};
  private actors: ActorInfo[] = [];
  private classes: ClassInfo[] = [];
  /**
   * Origins the "Classes" section shows. Defaults to all three (no
   * filtering) — see `setClassOriginFilter`. A class with no `source_origin`
   * (an older server that predates the field) is always shown regardless of
   * this filter, so it never silently disappears.
   */
  private classOriginFilter: ReadonlySet<ClassOrigin> = new Set(ALL_CLASS_ORIGINS);
  /** ADR 0108 Phase 8 (BT-2903): every loaded package's declared `type` aliases. */
  private typeAliases: TypeAliasInfo[] = [];
  private disposed = false;
  /** Active session ID captured from terminal output; used to query session bindings. */
  private sessionId: string | null = null;

  /** Cached inspect results keyed by "actor:<pid>". */
  private readonly inspectCache = new Map<string, Record<string, unknown>>();
  /**
   * Guards `inspectCache` against a stale-write race: a `getChildren`
   * `inspect()` fetch in flight when a "spawned"/"stopped" push invalidates
   * the same pid must not overwrite that invalidation once it resolves.
   */
  private readonly inspectGen = new GenerationTracker();

  /** Cached methods+stateVars results keyed by class name. */
  private readonly methodsCache = new Map<
    string,
    { methods: MethodInfo[]; stateVars: StateVarInfo[] }
  >();
  /** Guards `methodsCache` against the same stale-write race, keyed by class name. */
  private readonly methodsGen = new GenerationTracker();

  /**
   * Cached inherited-methods results keyed by class name (BT-3478). Holds
   * both instance- and class-side entries together — the two
   * `InheritedMethodGroupNode`s for the same class share one fetch, split by
   * `side` when building each group's children.
   */
  private readonly inheritedMethodsCache = new Map<string, InheritedMethodInfo[]>();
  /** Guards `inheritedMethodsCache` against the same stale-write race as `methodsGen`. */
  private readonly inheritedMethodsGen = new GenerationTracker();

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
  /**
   * Inject the stdlib virtual-URI document opener (see `stdlibDocumentOpener`
   * above). Pass null to clear. Independent of `setClient`/the workspace
   * protocol connection — it only needs the LSP `LanguageClient`, which
   * extension.ts wires up once at activation.
   */
  setStdlibDocumentOpener(
    opener: ((classInfo: ClassInfo) => Promise<vscode.TextDocument | undefined>) | null
  ): void {
    this.stdlibDocumentOpener = opener;
  }

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
      // Skip a redundant full-tree fire if we're already disconnected — e.g.
      // `connectWorkspace`/`disconnectWorkspace` call `workspaceWsClient.dispose()`
      // (which synchronously drives this same client's still-registered
      // `onConnectionChange("disconnected")` below, already firing a reset)
      // immediately followed by `setClient(null)` on the very next line.
      if (this.connectionState !== "disconnected") {
        this._resetState("disconnected");
      }
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
        // reconnecting — keep stale data visible, but only the root item's
        // description ("Reconnecting…") actually changed, so refresh just
        // that node rather than forcing every expanded node to re-fetch.
        this._onDidChangeTreeData.fire(CONNECTED_ROOT);
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

  /** The origins currently shown in the "Classes" section. */
  get classFilter(): ReadonlySet<ClassOrigin> {
    return this.classOriginFilter;
  }

  /**
   * Restrict the "Classes" section to the given origins (stdlib/project/dependency).
   * An empty set is treated as "no filter" (show everything) rather than an
   * empty tree — a filter picker with nothing checked is more useful reset to
   * its default than left showing zero classes.
   */
  setClassOriginFilter(origins: ReadonlySet<ClassOrigin>): void {
    this.classOriginFilter = origins.size > 0 ? new Set(origins) : new Set(ALL_CLASS_ORIGINS);
    this._onDidChangeTreeData.fire(CLASSES_SECTION);
  }

  /** Refresh all sections by re-fetching actors, classes, and bindings. */
  async refresh(): Promise<void> {
    if (!this.client || this.connectionState !== "connected") {
      return;
    }
    this.inspectCache.clear();
    this.methodsCache.clear();
    this.inheritedMethodsCache.clear();
    // Discard any per-item fetch (actor inspect / class methods) still in
    // flight from before this wholesale clear — without this, one resolving
    // afterward would repopulate the cache with a pre-refresh result.
    this.inspectGen.bumpAll();
    this.methodsGen.bumpAll();
    this.inheritedMethodsGen.bumpAll();
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
      case "type-aliases-section":
        return this._typeAliasesSectionItem();
      case "binding-item":
        return this._bindingItem(element);
      case "actor-item":
        return this._actorItem(element);
      case "class-item":
        return this._classItem(element);
      case "type-alias-item":
        return this._typeAliasItem(element);
      case "state-group":
        return this._stateGroupItem(element);
      case "state-item":
        return this._stateVarItem(element);
      case "method-group":
        return this._methodGroupItem(element);
      case "method-item":
        return this._methodItem(element);
      case "inherited-method-group":
        return this._inheritedMethodGroupItem(element);
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
          TYPE_ALIASES_SECTION,
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
        return this._filteredClasses().map((info) => ({ kind: "class-item" as const, info }));

      case "type-aliases-section":
        return this.typeAliases.map((info) => ({ kind: "type-alias-item" as const, info }));

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
        // Captured before the request so a "spawned"/"stopped" push that
        // invalidates this same pid while inspect() is in flight is detected
        // once it resolves, instead of overwriting the invalidation.
        const token = this.inspectGen.token(cacheKey);
        try {
          const state = await activeClient.inspect(element.info.pid);
          // Guard: client may have changed, or this pid's cache entry may
          // have been invalidated, while inspect() was in-flight.
          if (this.client !== activeClient) return [];
          if (!this.inspectGen.isCurrent(cacheKey, token)) return [];
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
        // Captured before the request so a "classes/loaded" or
        // "classes/removed" push that invalidates this same class while
        // methods() is in flight is detected once it resolves, instead of
        // overwriting the invalidation with a stale pre-reload result.
        const token = this.methodsGen.token(element.info.name);
        try {
          const result = await activeClient.methods(element.info.name);
          // Guard: client may have changed, or this class's cache entry may
          // have been invalidated, while methods() was in-flight.
          if (this.client !== activeClient) return [];
          if (!this.methodsGen.isCurrent(element.info.name, token)) return [];
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

      case "inherited-method-group": {
        const className = element.classInfo.name;
        const cached = this.inheritedMethodsCache.get(className);
        if (cached) {
          return this._inheritedMethodItems(cached, element.side);
        }
        const activeClient = this.client;
        if (!activeClient || this.connectionState !== "connected") {
          return [];
        }
        // Same in-flight-invalidation guard as the local "class-item" fetch
        // above, keyed by class name (shared by both sides' groups, since
        // one fetch covers both).
        const token = this.inheritedMethodsGen.token(className);
        try {
          const result = await activeClient.inheritedMethods(className);
          if (this.client !== activeClient) return [];
          if (!this.inheritedMethodsGen.isCurrent(className, token)) return [];
          this.inheritedMethodsCache.set(className, result);
          return this._inheritedMethodItems(result, element.side);
        } catch {
          return [];
        }
      }

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
        (await this._lspHoverTooltip(element.info, element.info.name, "class")) ??
        this._classTooltipFallback(element.info);
      return item;
    }
    if (element.kind === "method-item") {
      // BT-3444: a `synthetic` method (a compiler-generated accessor) or an
      // `unindexed_runtime_fun` one (a native/runtime-only method the
      // backend explicitly documents as having "no openable source" —
      // beamtalk_repl_ops_browse.erl) has no declaration anywhere in
      // source, so the LSP-hover / doc-comment lookups below would only
      // ever fail (there is nothing at any position to hover over or read
      // a `///` comment from) — go straight to the wire-supplied
      // signature/doc (BT-2735's synthetic-only resolution — always empty
      // for `unindexed_runtime_fun`, hence the fallback wording below)
      // instead of paying for two guaranteed-empty lookups.
      if (hasNoOpenableSource(element.method.source_status)) {
        item.tooltip = this._appendDefiningClass(
          this._noSourceMethodTooltip(element.method),
          element.definingClass
        );
        return item;
      }
      item.tooltip = this._appendDefiningClass(
        (await this._lspHoverTooltip(
          element.classInfo,
          element.method.selector,
          element.method.side === "class" ? "class-method" : "method",
          { side: element.method.side, declaredLine: element.method.line }
        )) ??
          (await this._methodDocCommentTooltip(element)) ??
          this._methodTooltipFallback(element.method),
        element.definingClass
      );
      return item;
    }
    if (element.kind === "state-item") {
      item.tooltip = await this._stateVarTooltip(element);
      return item;
    }
    return undefined;
  }

  /**
   * Open the document a class's source lives in, for hover/doc-comment
   * lookups. Tries the real `source_file` first; falls back to the injected
   * `stdlibDocumentOpener` (the `beamtalk-stdlib://` virtual URI scheme) for
   * compiled-in stdlib classes, which the runtime never records a real
   * `source_file` for — the same fallback `_hasNavigableSource`/
   * `beamtalk.openClassSource` already use for navigation. Every hover path
   * below (`_lspHoverTooltip`, `_methodDocCommentTooltip`, `_stateVarTooltip`)
   * goes through this, so a stdlib-defined class or a method/state var
   * inherited from one gets the same doc-comment treatment as a local one —
   * previously they fell straight to the hardcoded fallback tooltip.
   */
  private async _resolveClassDocument(
    classInfo: ClassInfo
  ): Promise<vscode.TextDocument | undefined> {
    const sourceFile = classInfo.source_file;
    if (sourceFile && sourceFile !== "unknown") {
      try {
        return await vscode.workspace.openTextDocument(vscode.Uri.file(sourceFile));
      } catch {
        return undefined;
      }
    }
    if (!this.stdlibDocumentOpener) return undefined;
    try {
      return await this.stdlibDocumentOpener(classInfo);
    } catch {
      return undefined;
    }
  }

  private async _lspHoverTooltip(
    classInfo: ClassInfo,
    symbol: string,
    kind: "class" | "method" | "class-method",
    decl?: { side?: "instance" | "class"; declaredLine?: number }
  ): Promise<vscode.MarkdownString | undefined> {
    try {
      const doc = await this._resolveClassDocument(classInfo);
      if (!doc) return undefined;
      const uri = doc.uri;

      // Fast path: locate the declaration the same way `navigateToMethod` /
      // `navigateToStateVar` already do (BT-3439's real-line-first, then
      // regex, then plain text-search chain) and hover at that single
      // position directly. This skips `executeDocumentSymbolProvider`
      // entirely — a full-file symbol computation that every sidebar hover
      // otherwise paid for on top of the hover request itself. Since
      // `resolveTreeItem` is only ever invoked once per tree item, an
      // attempt slow enough to outlast the mouse's dwell time effectively
      // never shows a tooltip at all rather than just showing one late.
      const fastOffset = this._declarationOffset(doc.getText(), kind, symbol, decl);
      if (fastOffset !== -1) {
        const fast = await this._hoverAt(uri, doc.positionAt(fastOffset));
        if (fast) return fast;
      }

      // Fallback: the LSP document symbol provider finds the exact position
      // (used when there's no declared line yet, or the fast text search
      // missed — e.g. a class compiled before BT-3439's real-line field).
      const docSymbols = await vscode.commands.executeCommand<
        vscode.DocumentSymbol[] | vscode.SymbolInformation[]
      >("vscode.executeDocumentSymbolProvider", uri);
      const pos = findSymbolPosition(docSymbols ?? [], symbol, kind);
      if (!pos) return undefined;
      return await this._hoverAt(uri, pos);
    } catch {
      return undefined;
    }
  }

  /**
   * Resolve a class/method declaration to a text offset via source search —
   * no LSP round trip (see `resolveDeclarationOffsetSync`). State-var hover
   * ("field") never reaches here: the `state-item` case in `resolveTreeItem`
   * goes through `_stateVarTooltip` instead, which reads source text
   * directly, so this only ever needs to handle the two kinds
   * `_lspHoverTooltip` is actually called with.
   */
  private _declarationOffset(
    text: string,
    kind: "class" | "method" | "class-method",
    symbol: string,
    decl?: { side?: "instance" | "class"; declaredLine?: number }
  ): number {
    const ref: DeclarationRef =
      kind === "class"
        ? { kind: "class", name: symbol }
        : {
            kind: "method",
            side: decl?.side ?? (kind === "class-method" ? "class" : "instance"),
            selector: symbol,
          };
    return resolveDeclarationOffsetSync(text, ref, decl?.declaredLine);
  }

  /** Run the hover provider at a position and flatten the result into one MarkdownString. */
  private async _hoverAt(
    uri: vscode.Uri,
    pos: vscode.Position
  ): Promise<vscode.MarkdownString | undefined> {
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

  /**
   * BT-3478: append the defining-class attribution line to an inherited
   * method's tooltip. A no-op (returns `tooltip` unchanged) for a local
   * method, where `definingClass` is `undefined`.
   */
  private _appendDefiningClass(
    tooltip: vscode.MarkdownString,
    definingClass: string | undefined
  ): vscode.MarkdownString {
    if (!definingClass) return tooltip;
    tooltip.appendMarkdown(`\n\n_Inherited from ${definingClass}_`);
    return tooltip;
  }

  private _methodTooltipFallback(method: MethodInfo): vscode.MarkdownString {
    return new vscode.MarkdownString(
      `**${method.selector}**\n\n_${method.side === "instance" ? "instance-side" : "class-side"}_`
    );
  }

  /**
   * BT-3444: tooltip for a method with no openable source — `synthetic` (a
   * compiler-generated accessor) or `unindexed_runtime_fun` (a
   * native/runtime-only method). Built entirely from the `methods` ws op's
   * wire-supplied `signature`/`doc` (resolved server-side for `synthetic`
   * rows only, BT-2735 — always absent for `unindexed_runtime_fun`) — never
   * a file read or LSP round trip, since there is no declaration in source
   * to read one from.
   */
  private _noSourceMethodTooltip(method: MethodInfo): vscode.MarkdownString {
    const md = new vscode.MarkdownString(`**${method.signature ?? method.selector}**`);
    const reason =
      method.source_status === "synthetic" ? "compiler-generated" : "no source available";
    md.appendMarkdown(
      `\n\n_${method.side === "instance" ? "instance-side" : "class-side"} · ${reason}_`
    );
    if (method.doc) {
      md.appendMarkdown(`\n\n${method.doc}`);
    }
    return md;
  }

  /**
   * Read source text and extract `///` doc comment for the method.
   * Used as a fallback when LSP hover is unavailable (file not open in editor).
   */
  private async _methodDocCommentTooltip(
    element: MethodItemNode
  ): Promise<vscode.MarkdownString | undefined> {
    try {
      const doc = await this._resolveClassDocument(element.classInfo);
      if (!doc) return undefined;
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
    try {
      const doc = await this._resolveClassDocument(classInfo);
      if (!doc) return fallback;
      const text = doc.getText();
      const info = extractStateVarInfo(text, stateVar.name);
      const docComment = extractStateVarDocComment(text, stateVar.name);
      if (!info && !docComment) return fallback;
      const md = new vscode.MarkdownString(`**${stateVar.name}**`);
      if (info?.defaultValue !== undefined) {
        md.appendMarkdown(`\n\nDefault: \`${info.defaultValue}\``);
      }
      if (docComment) {
        md.appendMarkdown(`\n\n${docComment}`);
      } else if (info?.comment) {
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
    const total = this.classes.length;
    const shown = this._filteredClasses().length;
    const isFiltered = shown !== total;
    // Collapsed by default per ADR 0046 (avoid information overload for newcomers)
    const item = new vscode.TreeItem("Classes", vscode.TreeItemCollapsibleState.Collapsed);
    if (total === 0) {
      item.description = "(none)";
    } else if (isFiltered) {
      item.description = `(${shown} of ${total})`;
    } else {
      item.description = `(${total} loaded)`;
    }
    item.iconPath = new vscode.ThemeIcon(isFiltered ? "filter" : "symbol-class");
    // A distinct contextValue when a filter is active lets view/item/context
    // menus (package.json) offer a "Clear Filter" action only when there is
    // one to clear.
    item.contextValue = isFiltered ? "classes-section-filtered" : "classes-section";
    return item;
  }

  /** The classes currently visible under "Classes", after `classOriginFilter`. */
  private _filteredClasses(): ClassInfo[] {
    if (this.classOriginFilter.size >= ALL_CLASS_ORIGINS.length) {
      return this.classes;
    }
    return this.classes.filter(
      (c) => c.source_origin === undefined || this.classOriginFilter.has(c.source_origin)
    );
  }

  // ADR 0108 Phase 8 (BT-2903): "Type Aliases (N)" — a sibling section to
  // "Classes (N loaded)". Deliberately no "loaded" qualifier: an alias
  // produces no BEAM module, so "loaded" would misdescribe what the count
  // means (every declared alias appears here, not a subset that happened to
  // get pulled into the running image).
  private _typeAliasesSectionItem(): vscode.TreeItem {
    const count = this.typeAliases.length;
    const item = new vscode.TreeItem("Type Aliases", vscode.TreeItemCollapsibleState.Collapsed);
    item.description = count > 0 ? `(${count})` : "(none)";
    item.iconPath = new vscode.ThemeIcon("symbol-interface");
    item.contextValue = "type-aliases-section";
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

  /**
   * Whether `beamtalk.openClassSource`/`navigateToMethod`/`navigateToStateVar`
   * can find something to open for this class. The runtime never reports a
   * real `source_file` for compiled-in stdlib classes, but those commands
   * fall back to the LSP's `beamtalk-stdlib://` virtual URI scheme
   * (`openStdlibDocumentForClass` in extension.ts) whenever `source_origin`
   * is `"stdlib"` — so a plain `source_file` check alone under-reports
   * navigability and leaves stdlib rows (direct or inherited) inert.
   */
  private _hasNavigableSource(info: ClassInfo): boolean {
    return (
      (!!info.source_file && info.source_file !== "unknown") || info.source_origin === "stdlib"
    );
  }

  private _classItem(node: ClassItemNode): vscode.TreeItem {
    const item = new vscode.TreeItem(node.info.name, vscode.TreeItemCollapsibleState.Collapsed);
    item.iconPath = new vscode.ThemeIcon("symbol-class");
    const hasSource = this._hasNavigableSource(node.info);
    item.contextValue = hasSource ? "class-item" : "class-item-no-source";
    if (node.info.actor_count !== undefined && node.info.actor_count > 0) {
      item.description = `${node.info.actor_count} instance${node.info.actor_count !== 1 ? "s" : ""}`;
    }
    return item;
  }

  // ADR 0108 Phase 8 (BT-2903): one `type` alias row. Always a leaf
  // (`TreeItemCollapsibleState.None`) — an alias has no methods/state to
  // expand into. The expansion (right-hand side) shows inline as the
  // description, mirroring how a class's instance count is shown; the doc
  // comment (if any) and internal flag surface in the tooltip only, so the
  // row itself stays a single compact line.
  private _typeAliasItem(node: TypeAliasItemNode): vscode.TreeItem {
    const item = new vscode.TreeItem(node.info.name, vscode.TreeItemCollapsibleState.None);
    item.iconPath = new vscode.ThemeIcon("symbol-interface");
    const hasSource = !!node.info.source_file && node.info.source_file !== "unknown";
    item.contextValue = hasSource ? "type-alias-item" : "type-alias-item-no-source";
    if (hasSource) {
      item.command = {
        command: "beamtalk.navigateToTypeAlias",
        title: "Go to Definition",
        arguments: [node],
      };
    }
    if (node.info.expansion) {
      item.description = `= ${node.info.expansion}`;
    }
    const tooltipLines = [`**type ${node.info.name}** = \`${node.info.expansion ?? "?"}\``];
    if (node.info.doc) {
      tooltipLines.push("", node.info.doc);
    }
    if (node.info.internal) {
      tooltipLines.push("", "_internal — package-private_");
    }
    if (node.info.source_file) {
      tooltipLines.push("", node.info.source_file);
    }
    item.tooltip = new vscode.MarkdownString(tooltipLines.join("\n"));
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

  /**
   * BT-3478: collapsed-by-default sibling to `_methodGroupItem` — the count
   * isn't known until expanded (lazy fetch), unlike the local groups, so
   * this never auto-expands even when non-empty.
   */
  private _inheritedMethodGroupItem(node: InheritedMethodGroupNode): vscode.TreeItem {
    const label =
      node.side === "instance" ? "Inherited Instance Methods" : "Inherited Class Methods";
    const item = new vscode.TreeItem(label, vscode.TreeItemCollapsibleState.Collapsed);
    item.iconPath = new vscode.ThemeIcon(
      node.side === "instance" ? "symbol-method" : "symbol-namespace"
    );
    item.contextValue = "inherited-method-group";
    return item;
  }

  private _methodItem(node: MethodItemNode): vscode.TreeItem {
    const item = new vscode.TreeItem(node.method.selector, vscode.TreeItemCollapsibleState.None);
    // BT-3444: a `synthetic` method (e.g. a `Value subclass:`'s
    // compiler-generated field accessor) or an `unindexed_runtime_fun` one
    // (a native/runtime-only method — e.g. HTTPClient class>>supervisionSpec,
    // injected for every Actor subclass to support supervision, with no
    // corresponding text anywhere in HTTPClient.bt) has no user-written
    // declaration anywhere in the class's source file, unlike every other
    // row here — badge it visibly distinct (gear icon + muted description)
    // and never wire up "Go to Definition", which would otherwise fail to
    // find the selector in source and surface a "not found" message
    // (BT-3439's navigateToMethod) — previously only `synthetic` got this
    // treatment, so an `unindexed_runtime_fun` row looked like a normal,
    // clickable method that silently did nothing useful. Mirrors the
    // LiveView IDE method list's `derived` badge for the same fact (BT-2714).
    // BT-3478: for an inherited entry, attribute the defining class as a
    // label (never a tree level — the "Inherited" groups stay flat).
    const definingSuffix = node.definingClass ? ` · ${node.definingClass}` : "";
    if (node.method.source_status === "synthetic") {
      item.iconPath = new vscode.ThemeIcon("gear");
      item.description = `compiler-generated${definingSuffix}`;
      item.contextValue = "method-item-synthetic";
      return item;
    }
    if (node.method.source_status === "unindexed_runtime_fun") {
      item.iconPath = new vscode.ThemeIcon("gear");
      item.description = `no source available${definingSuffix}`;
      item.contextValue = "method-item-unindexed";
      return item;
    }
    item.iconPath = new vscode.ThemeIcon("symbol-method");
    const hasSource = this._hasNavigableSource(node.classInfo);
    item.contextValue = hasSource ? "method-item" : "method-item-no-source";
    if (node.definingClass) {
      item.description = node.definingClass;
    }
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
    const hasSource = this._hasNavigableSource(node.classInfo);
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
      // BT-3478: flat sibling groups, same depth as the two above — lazily
      // fetched only when expanded (see getChildren's "inherited-method-group"
      // case), not eagerly built here like the local groups.
      { kind: "inherited-method-group" as const, side: "instance", classInfo },
      { kind: "inherited-method-group" as const, side: "class", classInfo },
    ];
  }

  /**
   * Build inherited method-item nodes for one side of an
   * `InheritedMethodGroupNode` (BT-3478), from the full (both-sides) fetch
   * result cached per receiving class.
   *
   * `classInfo` on each node is the *defining* class's info, looked up from
   * the already-loaded "Classes" section — not the receiving class the
   * group is nested under — so `beamtalk.navigateToMethod` opens the real
   * declaration (and correctly finds no source, matching the local-method
   * "no source" affordance, if the defining class isn't loaded/known).
   */
  private _inheritedMethodItems(
    all: InheritedMethodInfo[],
    side: "instance" | "class"
  ): MethodItemNode[] {
    return all
      .filter((m) => m.side === side)
      .map((m) => ({
        kind: "method-item" as const,
        method: m,
        classInfo: this._findClassInfo(m.definingClass),
        definingClass: m.definingClass,
      }));
  }

  /** Look up a loaded class's info by name, falling back to a source-less stub. */
  private _findClassInfo(name: string): ClassInfo {
    return this.classes.find((c) => c.name === name) ?? { name };
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
    this.typeAliases = [];
    this.inspectCache.clear();
    this.methodsCache.clear();
    this.inheritedMethodsCache.clear();
    this.inspectGen.bumpAll();
    this.methodsGen.bumpAll();
    this.inheritedMethodsGen.bumpAll();
    this._onDidChangeTreeData.fire(undefined);
  }

  /** Fetch bindings, actors, classes, and type aliases from a newly-connected client. */
  private async _fetchInitialData(client: WorkspaceClient): Promise<void> {
    const gen = ++this.initialFetchGeneration;
    // Prefer the active session ID (for the user's variables) over the extension's own WS session.
    const sessionId = this.sessionId ?? client.currentSessionId;
    const [bindingsResult, actorsResult, classesResult, typeAliasesResult] =
      await Promise.allSettled([
        sessionId ? client.bindings(sessionId) : Promise.resolve<BindingsMap>({}),
        client.actors(),
        client.classes(),
        // ADR 0108 Phase 8 (BT-2903): no push channel exists for alias changes
        // (unlike `class-loaded`), so this is refresh-on-connect only — a new
        // `type` declaration appears on the next reconnect/refresh, not live.
        client.typeAliases(),
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
    if (typeAliasesResult.status === "fulfilled") {
      this.typeAliases = typeAliasesResult.value;
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
        this.inspectGen.bump(`actor:${event.data.pid}`);
        this._onDidChangeTreeData.fire(ACTORS_SECTION);
      } else if (event.event === "stopped") {
        this.actors = this.actors.filter((a) => a.pid !== event.data.pid);
        this.inspectCache.delete(`actor:${event.data.pid}`);
        this.inspectGen.bump(`actor:${event.data.pid}`);
        this._onDidChangeTreeData.fire(ACTORS_SECTION);
      }
    } else if (event.channel === "classes" && event.event === "loaded") {
      // Invalidate cached methods for the reloaded class — its methods may have changed.
      this.methodsCache.delete(event.data.class);
      this.methodsGen.bump(event.data.class);
      // BT-3478: also invalidate its own inherited-methods entry. Known gap
      // (shared with the local-methods cache above): reloading an ancestor
      // does not invalidate a subclass's cached inherited view — a full
      // hierarchy walk on every reload wasn't justified for this op's
      // initial ship; `refresh()` / reconnect always sees the current state.
      this.inheritedMethodsCache.delete(event.data.class);
      this.inheritedMethodsGen.bump(event.data.class);
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
    } else if (event.channel === "classes" && event.event === "removed") {
      // BT-2531 (server-side) fires this when a class's process shuts down
      // (e.g. `removeFromSystem`/unload) — remove it immediately rather than
      // waiting for a "loaded" event that will never come for this class.
      // Unlike "loaded", no re-fetch is needed: the removed class is simply
      // absent from the list, no new metadata to read.
      this.classes = this.classes.filter((c) => c.name !== event.data.class);
      this.methodsCache.delete(event.data.class);
      this.methodsGen.bump(event.data.class);
      this.inheritedMethodsCache.delete(event.data.class);
      this.inheritedMethodsGen.bump(event.data.class);
      this._onDidChangeTreeData.fire(CLASSES_SECTION);
    }
  }
}

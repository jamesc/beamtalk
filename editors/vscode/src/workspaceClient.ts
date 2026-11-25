// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { execFile } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import type * as WsModule from "ws";
import { workspacesBaseDir } from "./beamtalkRoot";

// ─── Public types ────────────────────────────────────────────────────────────

export type ConnectionState = "connected" | "disconnected" | "reconnecting";

export interface ActorInfo {
  pid: string;
  class: string;
  /** Unix seconds; `null` on the wire for an actor that never recorded a start time. */
  spawned_at?: number | null;
  /** Backing module; sent by `actors`, absent from the `spawned` push frame. */
  module?: string;
  /** `true` for an actor the runtime started itself (`TranscriptStream`); the wire omits it otherwise. */
  system?: boolean;
}

export interface ActorStoppedInfo {
  pid: string;
  class: string;
  reason: string;
}

/** Where a class's source comes from — drives the Workspace Explorer's class filter. */
export type ClassOrigin = "stdlib" | "project" | "dependency";

export interface ClassInfo {
  name: string;
  source_file?: string;
  actor_count?: number;
  source_origin?: ClassOrigin;
}

/**
 * A `type` alias declaration (ADR 0108 Phase 8, BT-2903), as returned by the
 * `browse-type-aliases` op's `AliasRow` shape: `name`, `expansion` (the
 * alias's right-hand side, rendered to Beamtalk display form), `doc`,
 * `source_file`, `internal`, and — per `alias_row/3`'s BT-3496 extension,
 * mirroring `ClassRow`/`NativeModuleRow` — `package`/`source_origin`.
 * Aliases erase entirely at compile time (no BEAM module, no live process) —
 * unlike `ClassInfo`, there is no `actor_count` or anything else live to
 * report.
 *
 * Unlike `ClassInfo.source_file`, this `source_file` is never an absolute,
 * directly-openable path — it's a package-relative display path (BT-3496;
 * see `browseAliasSource`'s doc). `package`/`source_origin` are optional
 * only for backward compatibility with a server predating BT-3496 that
 * doesn't send them; a current server always includes both.
 */
export interface TypeAliasInfo {
  name: string;
  expansion?: string;
  doc?: string;
  source_file?: string;
  internal?: boolean;
  package?: string;
  source_origin?: ClassOrigin;
}

export interface MethodInfo {
  name: string;
  selector: string;
  side: "instance" | "class";
  /**
   * BT-3439: the method's real declaration line, from `beamtalk_xref`'s
   * compiled index. `undefined` for a class compiled before this field
   * existed, or a `ClassBuilder`-built class with no compiler to derive a
   * line from — callers fall back to the source-text regex guess in that
   * case (see `findMethodDeclaration` in `textUtils.ts`).
   */
  line?: number;
  /**
   * BT-3444: the xref tag verbatim (`indexed` | `synthetic` |
   * `unindexed_runtime_fun`) — the same fact the LiveView IDE method list
   * already badges (BT-2714). A `synthetic` method (e.g. a `Value
   * subclass:`'s compiler-generated field accessor) has no user-written
   * declaration anywhere in `classInfo.source_file`, so it renders with a
   * distinct icon/tooltip in the sidebar and never wires up
   * `beamtalk.navigateToMethod` (there is nothing to navigate to).
   */
  source_status?: "indexed" | "synthetic" | "unindexed_runtime_fun";
  /** BT-3444: compiler-derived signature, resolved only for `synthetic` rows. */
  signature?: string;
  /** BT-3444: compiler-derived doc, resolved only for `synthetic` rows. */
  doc?: string;
}

/**
 * An inherited (non-local) method, as returned by the `inherited-methods` op
 * (BT-3478). Same shape as `MethodInfo` plus `defining_class` — the ancestor
 * class that actually declares the method, for the sidebar's "Inherited"
 * groups to attribute each entry (a label, not a tree level — the groups
 * stay flat, matching the local method groups' depth).
 */
export interface InheritedMethodInfo extends MethodInfo {
  definingClass: string;
}

export interface StateVarInfo {
  name: string;
  /** BT-3439: see `MethodInfo.line` — same real-vs-fallback story, but for
   * instance-variable declarations. */
  line?: number;
}

export type BindingsMap = Record<string, unknown>;

export interface SessionInfo {
  id: string;
}

export interface LogEntry {
  level: string;
  time: string;
  msg: string;
  domain?: string;
  class?: string;
  selector?: string;
  mfa?: string;
  pid?: string;
}

export type PushEvent =
  | { channel: "actors"; event: "spawned"; data: ActorInfo }
  | { channel: "actors"; event: "stopped"; data: ActorStoppedInfo }
  | { channel: "classes"; event: "loaded"; data: { class: string } }
  | { channel: "classes"; event: "removed"; data: { class: string } }
  | { channel: "bindings"; event: "changed"; data: { session: string } }
  | { channel: "transcript"; text: string }
  | { channel: "logs"; event: "entry"; data: LogEntry };

// ─── WebSocket abstraction for testability ───────────────────────────────────

/** Callbacks registered by WorkspaceClient when creating a WebSocket. */
export interface WebSocketCallbacks {
  onOpen(): void;
  onClose(code: number, reason: string): void;
  onMessage(data: string): void;
  onError(err: Error): void;
}

/** Minimal handle returned by WebSocketFactory — just send and close. */
export interface IWebSocket {
  readonly readyState: number;
  send(data: string): void;
  close(): void;
}

/**
 * Factory that creates a WebSocket connection and wires up lifecycle callbacks.
 * Inject a custom factory in tests to avoid real network I/O.
 */
export type WebSocketFactory = (url: string, callbacks: WebSocketCallbacks) => IWebSocket;

// ─── Auth handshake constants ────────────────────────────────────────────────

/**
 * Workspace wire-protocol version this client speaks, sent as
 * `protocol_version` in the auth frame. The canonical value is
 * `?PROTOCOL_VERSION` in `runtime/apps/beamtalk_runtime/include/beamtalk.hrl`;
 * `workspaceClient.test.ts` reads that header and fails if this drifts.
 */
export const WORKSPACE_PROTOCOL_VERSION = "2.0";

/**
 * Client surface declared in the auth frame (BT-4373). The workspace gates
 * this surface on `protocol_version` only — the extension compiles no code,
 * so it need not be built from the same checkout as the node (unlike the
 * CLI/MCP/LSP, which carry the full toolchain lock, BT-4327).
 */
export const WORKSPACE_CLIENT_SURFACE = "vscode";

// ─── Reconnect constants ─────────────────────────────────────────────────────

const RECONNECT_BASE_MS = 1_000;
const RECONNECT_MAX_MS = 30_000;
const RECONNECT_JITTER_MS = 500;
const REQUEST_TIMEOUT_MS = 30_000;

// ─── WorkspaceClient ─────────────────────────────────────────────────────────

interface PendingRequest {
  resolve(value: unknown): void;
  reject(err: Error): void;
}

/**
 * WorkspaceClient manages a single WebSocket connection from the extension host
 * to a running Beamtalk workspace (ADR 0046).
 *
 * - Authenticates with the workspace cookie on connect.
 * - Automatically re-connects with exponential backoff on disconnect.
 * - Exposes op wrappers for all workspace queries.
 * - Emits typed push events for transcript, actor lifecycle, and class-loaded.
 * - Unit-testable: inject a `wsFactory` to avoid real network I/O.
 */
export class WorkspaceClient {
  private readonly workspaceId: string;
  private readonly port: number;
  private readonly cookie: string;
  private readonly wsFactory: WebSocketFactory;

  private ws: IWebSocket | null = null;
  private sessionId: string | null = null;
  private disposed = false;
  private authFailed = false;
  private reconnectAttempts = 0;
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;
  private connectionState: ConnectionState = "disconnected";

  private msgCounter = 0;
  private pending = new Map<string, PendingRequest>();

  /** Reads the node's current identity (its port-file nonce); see `nodeChangedOnLastConnect`. */
  private readonly nodeIdentity: () => string | undefined;
  /** The identity seen at the last `connected`; `null` before the first one. */
  private connectedIdentity: string | undefined | null = null;
  private nodeChanged = false;

  private connectionHandlers: Array<(state: ConnectionState) => void> = [];
  private authErrorHandlers: Array<(message: string) => void> = [];
  private pushHandlers: Array<(event: PushEvent) => void> = [];

  constructor(
    workspaceId: string,
    port: number,
    cookie: string,
    wsFactory: WebSocketFactory = defaultWsFactory,
    nodeIdentity: () => string | undefined = () => undefined
  ) {
    this.workspaceId = workspaceId;
    this.port = port;
    this.cookie = cookie;
    this.wsFactory = wsFactory;
    this.nodeIdentity = nodeIdentity;
  }

  /**
   * Create a WorkspaceClient by reading port and cookie from the workspace
   * directory (`~/.beamtalk/workspaces/{id}/port` and `cookie`; `BEAMTALK_HOME` overrides `~/.beamtalk`).
   *
   * @param workspaceId  Workspace ID (hash or user-assigned name).
   * @param options.baseDir  Override base directory (default: `$BEAMTALK_HOME/workspaces`, else `~/.beamtalk/workspaces`).
   * @param options.wsFactory  Override WebSocket factory (for testing).
   */
  static async fromFiles(
    workspaceId: string,
    options?: { baseDir?: string; wsFactory?: WebSocketFactory }
  ): Promise<WorkspaceClient> {
    const base = options?.baseDir ?? workspacesBaseDir();
    if (base === undefined) {
      throw new Error("Cannot locate the Beamtalk workspaces directory: no home directory");
    }
    const wsDir = path.join(base, workspaceId);

    const portContent = fs.readFileSync(path.join(wsDir, "port"), "utf8");
    const parsed = parsePortFileContent(portContent);
    if (!parsed.ok) {
      throw new Error(
        `Invalid port in ${path.join(wsDir, "port")}: ${JSON.stringify(parsed.firstLine)}`
      );
    }
    const port = parsed.port;

    await assertPortFileFresh(wsDir, port, parsed.nonce);

    const cookie = fs.readFileSync(path.join(wsDir, "cookie"), "utf8").trim();
    if (!cookie) {
      throw new Error(`Empty cookie in ${path.join(wsDir, "cookie")}`);
    }

    // A restarted node rewrites its port file with a fresh nonce, so the nonce
    // tells a transient reconnect to the same node from one to a new node. A
    // crashed node's files linger until its successor rewrites them, so the
    // identity also needs the node pid recorded in node.info to be alive —
    // otherwise it is unknown, which counts as a new node.
    const portFile = path.join(wsDir, "port");
    const nodeIdentity = (): string | undefined => {
      try {
        const reread = parsePortFileContent(fs.readFileSync(portFile, "utf8"));
        if (!reread.ok || reread.nonce === undefined) return undefined;
        const info = JSON.parse(fs.readFileSync(path.join(wsDir, "node.info"), "utf8")) as {
          pid?: unknown;
        };
        if (typeof info.pid === "number" && !pidAlive(info.pid)) return undefined;
        return reread.nonce;
      } catch {
        return undefined;
      }
    };
    return new WorkspaceClient(workspaceId, port, cookie, options?.wsFactory, nodeIdentity);
  }

  /**
   * Initiate the WebSocket connection. Idempotent — safe to call if already
   * connected or connecting.
   */
  connect(): void {
    if (this.disposed || this.ws !== null) {
      return;
    }
    this._doConnect();
  }

  /** Close the connection, cancel pending reconnect, and reject all in-flight requests. */
  dispose(): void {
    this.disposed = true;
    this._cancelReconnect();
    this.ws?.close();
    this.ws = null;
    this._rejectAllPending(new Error("WorkspaceClient disposed"));
    this._setConnectionState("disconnected");
  }

  // ─── Op wrappers ─────────────────────────────────────────────────────────

  /**
   * List variable binding *names* (session locals) for a session.
   *
   * BT-2369 (ADR 0081 Phase 6): the dedicated `bindings` protocol op was
   * removed. We now read session state through the Beamtalk-native `Session`
   * API via `eval`, mirroring how `reload` routes through `ClassName reload`.
   * Session locals only — workspace bindings are a separate `Workspace bindings`
   * layer.
   *
   * Cross-session note: the runtime's `eval` always runs in *this* WebSocket's
   * own session pid — the protocol `session` field does not redirect eval
   * dispatch. To read the user's REPL terminal session we therefore go through
   * the explicit cross-session API `Session withId: <id>` (read-only), guarding
   * the `nil` (unknown id) case. The map's values are placeholders (`""`): a
   * Beamtalk `Dictionary` is encoded as its `printString`, not a JSON object,
   * so per-binding values are fetched on demand via {@link bindingValue}
   * (e.g. from the inspector), which also avoids a torn keys/values read.
   */
  async bindings(sessionId: string): Promise<BindingsMap> {
    const code = `(Session withId: ${btString(assertSessionId(sessionId))}) ifNil: [#()] ifNotNil: [:s | s bindings keys]`;
    const resp = (await this._request({ op: "eval", code })) as {
      value?: unknown;
    };
    const names = Array.isArray(resp.value) ? resp.value : [];
    const map: BindingsMap = {};
    for (const name of names) {
      map[String(name)] = "";
    }
    return map;
  }

  /**
   * Read a single binding's value (session local) from a session, by name.
   *
   * BT-2369 (ADR 0081 Phase 6): reads via the cross-session `Session withId:`
   * API (`eval` runs in this WebSocket's own session, so the protocol `session`
   * field cannot redirect it). Returns `null` when the session id is unknown or
   * the binding is absent. A single eval, so the value is a consistent snapshot.
   */
  async bindingValue(sessionId: string, name: string): Promise<unknown> {
    const code = `(Session withId: ${btString(assertSessionId(sessionId))}) ifNil: [nil] ifNotNil: [:s | s bindings at: ${btSymbol(name)}]`;
    const resp = (await this._request({ op: "eval", code })) as {
      value?: unknown;
    };
    return resp.value ?? null;
  }

  /** List all running actors in the workspace. */
  async actors(): Promise<ActorInfo[]> {
    const resp = (await this._request({ op: "actors" })) as {
      actors?: ActorInfo[];
    };
    return resp.actors ?? [];
  }

  /**
   * List all loaded classes in the workspace.
   *
   * BT-2091: Routes through `list-classes` rather than the deprecated
   * `modules` protocol op (which has been removed). `list-classes` was
   * extended in BT-2091 to include `source_file` and `actor_count` so
   * the editor's class navigation keeps working, and later to include
   * `source_origin` (stdlib/project/dependency, reusing the System
   * Browser's `browse-classes` classifier) so the sidebar can filter by it.
   */
  async classes(): Promise<ClassInfo[]> {
    const resp = (await this._request({ op: "list-classes" })) as {
      class_list?: Array<{
        name: string;
        source_file?: string | null;
        actor_count?: number;
        source_origin?: ClassOrigin | null;
      }>;
    };
    return (resp.class_list ?? []).map((c) => ({
      name: c.name,
      source_file: c.source_file ?? undefined,
      actor_count: c.actor_count,
      source_origin: c.source_origin ?? undefined,
    }));
  }

  /**
   * List every loaded package's declared `type` aliases for the Workspace
   * Explorer sidebar's "Type Aliases" section (ADR 0108 Phase 8, BT-2903).
   *
   * Unlike `classes()` (which still goes through the older `list-classes`
   * REPL op, ADR 0096), there is no equivalent legacy alias-listing op — this
   * is the one sidebar consumer of the new `browse-type-aliases` op directly.
   * A dependency's `internal` alias is already excluded server-side at the
   * seeding boundary (ADR 0108 Implementation), so every row returned here is
   * visible-by-construction; this method does no client-side filtering.
   */
  async typeAliases(): Promise<TypeAliasInfo[]> {
    const resp = (await this._request({ op: "browse-type-aliases" })) as {
      value?: Array<{
        name: string;
        expansion?: string | null;
        doc?: string | null;
        source_file?: string | null;
        internal?: boolean;
        package?: string;
        source_origin?: ClassOrigin;
      }>;
    };
    return (resp.value ?? []).map((a) => ({
      name: a.name,
      expansion: a.expansion ?? undefined,
      doc: a.doc ?? undefined,
      source_file: a.source_file ?? undefined,
      internal: a.internal,
      package: a.package,
      source_origin: a.source_origin,
    }));
  }

  /**
   * Read-only source content for a declared `type` alias (BT-3314's
   * `browse-alias-source` op; wired up client-side in BT-3496). `pkg`
   * disambiguates a same-named alias declared by more than one package
   * (`browse-type-aliases` doesn't dedupe by name — see its doc) — always
   * pass `TypeAliasInfo.package` where available.
   *
   * A `type` declaration produces no BEAM module (aliases erase entirely at
   * compile time), so there is no compiled-module path to recover an
   * absolute file location from at read time — unlike `ClassInfo.source_file`,
   * `TypeAliasInfo.source_file` is a package-relative display path, not one
   * `vscode.Uri.file()` can open directly. This op resolves it server-side
   * instead and returns file content directly. `content` comes back `null`
   * (never an error) whenever the runtime has no path to read from — always
   * the case for a stdlib/dependency-origin alias (no live source tree to
   * resolve against server-side), and possibly a project-origin alias whose
   * recorded `source_file` no longer exists on disk.
   */
  async browseAliasSource(name: string, pkg?: string): Promise<{ content: string | null }> {
    const resp = (await this._request({
      op: "browse-alias-source",
      name,
      ...(pkg ? { package: pkg } : {}),
    })) as {
      value?: { content?: string | null };
    };
    return { content: resp.value?.content ?? null };
  }

  /** Get the internal state of an actor process. */
  async inspect(pid: string): Promise<Record<string, unknown>> {
    const resp = (await this._request({ op: "inspect", actor: pid })) as {
      state?: Record<string, unknown>;
    };
    return resp.state ?? {};
  }

  /** List all methods and state vars for a loaded class. */
  async methods(className: string): Promise<{ methods: MethodInfo[]; stateVars: StateVarInfo[] }> {
    const resp = (await this._request({ op: "methods", class: className })) as {
      methods?: Array<{
        name: string;
        selector: string;
        side: "instance" | "class";
        line?: number | null;
        source_status?: "indexed" | "synthetic" | "unindexed_runtime_fun";
        signature?: string | null;
        doc?: string | null;
      }>;
      state_vars?: Array<{ name: string; line?: number | null }>;
    };
    const methodsRaw = Array.isArray(resp.methods) ? resp.methods : [];
    const stateVarsRaw = Array.isArray(resp.state_vars) ? resp.state_vars : [];
    return {
      // BT-3439: `line` is `null` on the wire (a class predating this field,
      // or ClassBuilder-built) — normalize to `undefined` so callers can use
      // a single `??`/optional-chaining check for "no real line available".
      // BT-3444: `signature`/`doc` are `null` on the wire for every
      // non-`synthetic` row (they're resolved for synthetic rows only) —
      // same normalization.
      methods: methodsRaw.map((m) => ({
        ...m,
        line: m.line ?? undefined,
        signature: m.signature ?? undefined,
        doc: m.doc ?? undefined,
      })),
      stateVars: stateVarsRaw.map((v) => ({ name: v.name, line: v.line ?? undefined })),
    };
  }

  /**
   * List inherited (non-local) instance and class-side methods for a loaded
   * class, each attributed to its defining class (BT-3478). Called only when
   * the sidebar's "Inherited" group is expanded — kept off the eager
   * `methods()` fetch every class item already pays on first expand.
   */
  async inheritedMethods(className: string): Promise<InheritedMethodInfo[]> {
    const resp = (await this._request({ op: "inherited-methods", class: className })) as {
      methods?: Array<{
        name: string;
        selector: string;
        side: "instance" | "class";
        line?: number | null;
        source_status?: "indexed" | "synthetic" | "unindexed_runtime_fun";
        signature?: string | null;
        doc?: string | null;
        defining_class: string;
      }>;
    };
    const methodsRaw = Array.isArray(resp.methods) ? resp.methods : [];
    return methodsRaw.map((m) => ({
      ...m,
      line: m.line ?? undefined,
      signature: m.signature ?? undefined,
      doc: m.doc ?? undefined,
      definingClass: m.defining_class,
    }));
  }

  /** List all active sessions in the workspace. */
  async sessions(): Promise<SessionInfo[]> {
    const resp = (await this._request({ op: "sessions" })) as {
      sessions?: SessionInfo[];
    };
    return resp.sessions ?? [];
  }

  /**
   * Reload a class by name.
   *
   * BT-2091: Routes through `ClassName reload` evaluation rather than the
   * deprecated `reload` protocol op (which has been removed). Recompiles
   * the class's source file and hot-swaps any running actors.
   *
   * @param className  Beamtalk class name (e.g. `"Counter"`).
   */
  async reload(className: string): Promise<{ classes: ClassInfo[]; warnings: string[] }> {
    const resp = (await this._request({
      op: "eval",
      code: `${assertClassName(className)} reload`,
    })) as { warnings?: string[] };
    return {
      classes: [{ name: className }],
      warnings: resp.warnings ?? [],
    };
  }

  /**
   * Subscribe to live log streaming from the workspace.
   *
   * @param level  Minimum log level to receive (default: all levels).
   *   Valid values: "emergency", "alert", "critical", "error", "warning",
   *   "notice", "info", "debug".
   */
  async subscribeLogs(level?: string): Promise<void> {
    const params: Record<string, unknown> = { op: "subscribe-logs" };
    if (level) {
      params.level = level;
    }
    await this._request(params);
  }

  // ─── Events ──────────────────────────────────────────────────────────────

  /**
   * Subscribe to connection state changes (connected / disconnected / reconnecting).
   * Returns a disposal function that removes the handler.
   */
  /** Current connection state. */
  get state(): ConnectionState {
    return this.connectionState;
  }

  /**
   * Whether the latest `connected` reached a different node than the one
   * before it (BT-5189): the node restarted, so actors and sessions from the
   * earlier connection are gone. `false` for the first connect and for a
   * transient reconnect to the same node (same port-file nonce). When the
   * identity cannot be read, a reconnect is assumed to be a new node.
   */
  get nodeChangedOnLastConnect(): boolean {
    return this.nodeChanged;
  }

  onConnectionChange(handler: (state: ConnectionState) => void): () => void {
    this.connectionHandlers.push(handler);
    return () => {
      this.connectionHandlers = this.connectionHandlers.filter((h) => h !== handler);
    };
  }

  /**
   * Subscribe to terminal auth failures (bad cookie, or a protocol mismatch
   * between this extension and the workspace node). The handler receives the
   * server's message. The client does not reconnect after an auth failure.
   * Returns a disposal function that removes the handler.
   */
  onAuthError(handler: (message: string) => void): () => void {
    this.authErrorHandlers.push(handler);
    return () => {
      this.authErrorHandlers = this.authErrorHandlers.filter((h) => h !== handler);
    };
  }

  /**
   * Subscribe to incoming push events from the workspace
   * (transcript output, actor lifecycle, class-loaded).
   * Returns a disposal function that removes the handler.
   */
  onPush(handler: (event: PushEvent) => void): () => void {
    this.pushHandlers.push(handler);
    return () => {
      this.pushHandlers = this.pushHandlers.filter((h) => h !== handler);
    };
  }

  /** The workspace ID this client is connected to. */
  get id(): string {
    return this.workspaceId;
  }

  /** The session ID assigned by the server after successful authentication. */
  get currentSessionId(): string | null {
    return this.sessionId;
  }

  // ─── Private ─────────────────────────────────────────────────────────────

  private _doConnect(): void {
    if (this.disposed || this.ws !== null) {
      return;
    }

    const url = `ws://127.0.0.1:${this.port}/ws`;
    let ws: IWebSocket;
    try {
      ws = this.wsFactory(url, {
        onOpen: () => {
          // Auth challenge arrives as first server message; nothing to do here.
        },
        onClose: (code, reason) => {
          this.ws = null;
          this.sessionId = null;
          this._rejectAllPending(new Error(`WebSocket closed: code=${code} reason=${reason}`));
          if (!this.disposed && !this.authFailed) {
            this._setConnectionState("reconnecting");
            this._scheduleReconnect();
          } else {
            this._setConnectionState("disconnected");
          }
        },
        onMessage: (data) => {
          this._handleMessage(data);
        },
        onError: (_err) => {
          // An error is always followed by onClose; reconnect is handled there.
        },
      });
    } catch (_err) {
      if (!this.disposed) {
        this._setConnectionState("reconnecting");
        this._scheduleReconnect();
      }
      return;
    }

    this.ws = ws;
  }

  private _handleMessage(raw: string): void {
    let msg: Record<string, unknown>;
    try {
      msg = JSON.parse(raw) as Record<string, unknown>;
    } catch {
      return;
    }

    // Auth challenge — send cookie
    if (msg.op === "auth-required") {
      this.ws?.send(
        JSON.stringify({
          type: "auth",
          cookie: this.cookie,
          client: WORKSPACE_CLIENT_SURFACE,
          protocol_version: WORKSPACE_PROTOCOL_VERSION,
        })
      );
      return;
    }

    // Auth success (session-started follows)
    if (msg.type === "auth_ok") {
      return;
    }

    // Auth failure — terminal, do not reconnect (cookie won't change)
    if (msg.type === "auth_error") {
      this.authFailed = true;
      const message = String(msg.message ?? "unknown");
      this._rejectAllPending(new Error(`Auth error: ${message}`));
      for (const h of this.authErrorHandlers) {
        try {
          h(message);
        } catch {
          // Ignore handler errors to prevent one bad handler blocking others.
        }
      }
      this.ws?.close();
      return;
    }

    // Session assigned — now fully connected
    if (msg.op === "session-started") {
      this.sessionId = msg.session as string;
      this.reconnectAttempts = 0;
      const identity = this.nodeIdentity();
      this.nodeChanged =
        this.connectedIdentity !== null &&
        (identity === undefined || identity !== this.connectedIdentity);
      this.connectedIdentity = identity;
      this._setConnectionState("connected");
      return;
    }

    // Push: transcript (simple legacy format)
    if (msg.push === "transcript" && typeof msg.text === "string") {
      this._emitPush({ channel: "transcript", text: msg.text });
      return;
    }

    // Push: typed channel format (actors, classes)
    if (msg.type === "push") {
      this._handlePush(msg);
      return;
    }

    // Op response — correlate by request ID
    if (typeof msg.id === "string") {
      const id = msg.id;
      const req = this.pending.get(id);
      if (req) {
        const status = msg.status as string[] | undefined;
        if (status?.includes("done")) {
          this.pending.delete(id);
          if (status.includes("error")) {
            req.reject(new Error(String(msg.error ?? "Request failed")));
          } else {
            req.resolve(msg);
          }
        }
        // Non-final streaming messages (out) are silently ignored here.
      }
    }
  }

  private _handlePush(msg: Record<string, unknown>): void {
    const channel = msg.channel as string;
    const event = msg.event as string;
    const data = msg.data as Record<string, unknown> | undefined;

    if (channel === "actors" && data) {
      if (event === "spawned") {
        this._emitPush({
          channel: "actors",
          event: "spawned",
          data: {
            pid: String(data.pid ?? ""),
            class: String(data.class ?? ""),
            spawned_at: typeof data.spawned_at === "number" ? data.spawned_at : undefined,
          },
        });
      } else if (event === "stopped") {
        this._emitPush({
          channel: "actors",
          event: "stopped",
          data: {
            pid: String(data.pid ?? ""),
            class: String(data.class ?? ""),
            reason: String(data.reason ?? ""),
          },
        });
      }
    } else if (channel === "classes" && event === "loaded" && data) {
      this._emitPush({
        channel: "classes",
        event: "loaded",
        data: { class: String(data.class ?? "") },
      });
    } else if (channel === "classes" && event === "removed" && data) {
      // BT-2531 (server-side): the runtime already announces this whenever a
      // class's process shuts down (e.g. `removeFromSystem`) — the sidebar
      // just never consumed it, so a removed/unloaded class lingered in the
      // tree until a manual "Refresh Workspace".
      this._emitPush({
        channel: "classes",
        event: "removed",
        data: { class: String(data.class ?? "") },
      });
    } else if (channel === "bindings" && event === "changed" && data) {
      this._emitPush({
        channel: "bindings",
        event: "changed",
        data: { session: String(data.session ?? "") },
      });
    } else if (channel === "logs" && event === "entry" && data) {
      this._emitPush({
        channel: "logs",
        event: "entry",
        data: {
          level: String(data.level ?? "info"),
          time: String(data.time ?? ""),
          msg: String(data.msg ?? ""),
          domain: data.domain != null ? String(data.domain) : undefined,
          class: data.class != null ? String(data.class) : undefined,
          selector: data.selector != null ? String(data.selector) : undefined,
          mfa: data.mfa != null ? String(data.mfa) : undefined,
          pid: data.pid != null ? String(data.pid) : undefined,
        },
      });
    }
  }

  private _request(params: Record<string, unknown>): Promise<unknown> {
    return new Promise((resolve, reject) => {
      if (this.connectionState !== "connected" || !this.ws) {
        reject(new Error("Not connected to workspace"));
        return;
      }
      const id = `wsc-${++this.msgCounter}`;
      const timer = setTimeout(() => {
        this.pending.delete(id);
        reject(
          new Error(`Request timed out after ${REQUEST_TIMEOUT_MS}ms: op=${String(params.op)}`)
        );
      }, REQUEST_TIMEOUT_MS);
      this.pending.set(id, {
        resolve(value: unknown) {
          clearTimeout(timer);
          resolve(value);
        },
        reject(err: Error) {
          clearTimeout(timer);
          reject(err);
        },
      });
      try {
        this.ws.send(JSON.stringify({ id, ...params }));
      } catch (err) {
        clearTimeout(timer);
        this.pending.delete(id);
        reject(err instanceof Error ? err : new Error(String(err)));
      }
    });
  }

  private _scheduleReconnect(): void {
    if (this.disposed || this.reconnectTimer !== null) {
      return;
    }
    const delay = Math.min(
      RECONNECT_BASE_MS * 2 ** this.reconnectAttempts + Math.random() * RECONNECT_JITTER_MS,
      RECONNECT_MAX_MS
    );
    this.reconnectAttempts++;
    this.reconnectTimer = setTimeout(() => {
      this.reconnectTimer = null;
      if (!this.disposed) {
        this._doConnect();
      }
    }, delay);
  }

  private _cancelReconnect(): void {
    if (this.reconnectTimer !== null) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
  }

  private _setConnectionState(state: ConnectionState): void {
    if (this.connectionState === state) {
      return;
    }
    this.connectionState = state;
    for (const h of this.connectionHandlers) {
      try {
        h(state);
      } catch {
        // Ignore handler errors to prevent one bad handler blocking others.
      }
    }
  }

  private _rejectAllPending(err: Error): void {
    for (const req of this.pending.values()) {
      req.reject(err);
    }
    this.pending.clear();
  }

  private _emitPush(event: PushEvent): void {
    for (const h of this.pushHandlers) {
      try {
        h(event);
      } catch {
        // Ignore handler errors to prevent one bad handler blocking others.
      }
    }
  }
}

/** Is a process with this pid alive? (`EPERM` means it exists but is not ours.) */
function pidAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch (err) {
    return (err as NodeJS.ErrnoException).code === "EPERM";
  }
}

/** Result of parsing a workspace `port` file: `PORT\nNONCE`, the nonce line optional. */
export type PortFileParse =
  | { ok: true; port: number; nonce: string | undefined }
  | { ok: false; firstLine: string };

/**
 * Parse the contents of a workspace `port` file.
 *
 * The TypeScript twin of `beamtalk_workspace::parse_port_file`; both are
 * pinned to `tests/conformance/port_file_conformance.json`. Strict (BT-5112):
 * the port is on the first line, ASCII digits only (no sign, no whitespace),
 * in 1..=65535. The nonce line is trimmed; a missing or empty one is `undefined`.
 */
export function parsePortFileContent(content: string): PortFileParse {
  const lines = content.split(/\r?\n/);
  const firstLine = lines[0];
  if (!/^\d+$/.test(firstLine)) {
    return { ok: false, firstLine };
  }
  const port = Number(firstLine);
  if (port < 1 || port > 65535) {
    return { ok: false, firstLine };
  }
  return { ok: true, port, nonce: lines[1]?.trim() || undefined };
}

/**
 * Refuse to hand the workspace cookie to a stale port (BT-5069).
 *
 * Mirrors the CLI's `is_node_running` rule (`node_state.rs`): the port file is
 * `PORT\nNONCE`; when `node.info` records a nonce it must equal the port
 * file's, and the port must match. The CLI relies on its TCP probe for
 * liveness; the extension cannot learn the server's nonce before
 * authenticating, so it additionally requires the recorded BEAM pid to be
 * alive. A killed node whose port was re-bound by another process therefore
 * never receives the cookie. Old files without a nonce/pid stay accepted.
 */
async function assertPortFileFresh(
  wsDir: string,
  port: number,
  fileNonce: string | undefined
): Promise<void> {
  let raw: string;
  try {
    raw = fs.readFileSync(path.join(wsDir, "node.info"), "utf8");
  } catch {
    // Decided (BT-5130): no node.info means an old node or one still starting;
    // there is nothing to cross-check (the CLI trusts its TCP probe here).
    return;
  }
  let info: { port?: unknown; pid?: unknown; nonce?: unknown; start_time?: unknown };
  try {
    info = JSON.parse(raw);
  } catch {
    // Decided (BT-5130): fail closed. A node.info we cannot parse is either
    // mid-write (the caller retries / re-arms on the next node.info change)
    // or corrupt; neither is a reason to hand over the cookie.
    throw new Error(`Stale port file in ${wsDir}: node.info is unreadable (will retry)`);
  }
  if (typeof info.port === "number" && info.port !== port) {
    throw new Error(`Stale port file in ${wsDir}: node.info port ${info.port} != ${port}`);
  }
  if (typeof info.nonce === "string") {
    // node.info records a nonce: the port file must carry the same one.
    if (fileNonce === undefined) {
      throw new Error(`Stale port file in ${wsDir}: port file has no nonce but node.info does`);
    }
    if (info.nonce !== fileNonce) {
      throw new Error(`Stale port file in ${wsDir}: nonce mismatch`);
    }
  }
  if (typeof info.pid === "number") {
    if (!pidAlive(info.pid)) {
      throw new Error(
        `Stale port file in ${wsDir}: node pid ${info.pid} is not running, or its process is not visible from where the extension runs. If the node is running, this can happen when BEAMTALK_HOME is shared across containers or pid namespaces.`
      );
    }
    // Same rule as the CLI's `still_same_process` / `start_time_matches`, on
    // every platform the CLI records a start time on (BT-5189: Linux, macOS,
    // Windows). Read as digits from the raw text: a Windows FILETIME tick count
    // is past 2^53, so `JSON.parse` would round it.
    const expected = rawStartTime(raw, info.start_time);
    if (!startTimeMatches(expected, await procStartTime(info.pid))) {
      throw new Error(
        `Stale port file in ${wsDir}: pid ${info.pid} is alive but its start time differs from node.info (pid reused)`
      );
    }
  }
}

/** `node.info`'s `start_time` as an exact integer, or `undefined` when absent. */
function rawStartTime(raw: string, parsed: unknown): bigint | undefined {
  if (typeof parsed !== "number") return undefined;
  const digits = /"start_time"\s*:\s*(\d+)/.exec(raw)?.[1];
  return digits === undefined ? BigInt(Math.trunc(parsed)) : BigInt(digits);
}

/**
 * What could be learned about a process's start time, in the unit the CLI's
 * `pid_liveness::proc_start_time` records on this platform:
 *
 * - `time` — observed. `resolution` is the granularity it was observed at (in
 *   that unit): 1 where the extension reads the same value the CLI does, larger
 *   where it can only read a coarser one (macOS `ps` reports whole seconds; the
 *   CLI records microseconds).
 * - `gone` — the probe ran and found no such process (or, on Linux, its
 *   `/proc` entry could not be read — as the CLI, a mismatch).
 * - `unknown` — this host cannot observe it (the probe tool is missing or
 *   failed, or the platform records none): liveness is trusted.
 */
export type StartTimeObservation =
  | { kind: "time"; value: bigint; resolution: bigint }
  | { kind: "gone" }
  | { kind: "unknown" };

/**
 * Runs a probe command: resolves to its stdout, or rejects (a spawn error or
 * a non-zero exit). Asynchronous so a slow probe (a PowerShell cold start
 * takes seconds) never blocks the extension host (BT-5189).
 */
export type ProbeRunner = (file: string, args: string[]) => Promise<string>;

const runProbe: ProbeRunner = (file, args) =>
  new Promise((resolve, reject) => {
    execFile(
      file,
      args,
      {
        encoding: "utf8",
        timeout: 10_000,
        windowsHide: true,
        // `TZ=UTC`: `ps -o lstart=` then prints UTC, which has no repeated hour.
        env: { ...process.env, LC_ALL: "C", TZ: "UTC" },
      },
      (err, stdout) => (err ? reject(err) : resolve(stdout))
    );
  });

/** A spawn failure (tool missing) or a kill (timed out) rather than the tool's own non-zero exit. */
function probeUnavailable(err: unknown): boolean {
  const e = err as { code?: unknown; killed?: unknown; signal?: unknown };
  return typeof e.code === "string" || e.killed === true || e.signal != null;
}

const MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

/**
 * Process start time, as the CLI's `pid_liveness::proc_start_time` records it:
 *
 * - Linux: `/proc/<pid>/stat` field 22 (clock ticks since boot).
 * - Windows: `GetProcessTimes` creation time, a FILETIME 100 ns tick count —
 *   read through .NET's `Process.StartTime.ToFileTimeUtc()`, the same value.
 * - macOS: microseconds since the epoch. `ps -o lstart=` gives whole seconds,
 *   so the observation's resolution is one second.
 */
export async function procStartTime(
  pid: number,
  platform: NodeJS.Platform = process.platform,
  run: ProbeRunner = runProbe
): Promise<StartTimeObservation> {
  if (!Number.isSafeInteger(pid) || pid <= 0) return { kind: "gone" };
  switch (platform) {
    case "linux":
      return linuxStartTime(pid);
    case "win32":
      return windowsStartTime(pid, run);
    case "darwin":
      return macosStartTime(pid, run);
    default:
      return { kind: "unknown" };
  }
}

function linuxStartTime(pid: number): StartTimeObservation {
  try {
    const stat = fs.readFileSync(`/proc/${pid}/stat`, "utf8");
    const rest = stat
      .slice(stat.lastIndexOf(")") + 1)
      .trim()
      .split(/\s+/);
    return /^\d+$/.test(rest[19] ?? "")
      ? { kind: "time", value: BigInt(rest[19]), resolution: 1n }
      : { kind: "gone" };
  } catch {
    return { kind: "gone" };
  }
}

async function windowsStartTime(pid: number, run: ProbeRunner): Promise<StartTimeObservation> {
  // `gone` only when the process does not exist; any other failure (access
  // denied reading another user's process) is `unknown`, never a mismatch.
  const script =
    `try { $p = [System.Diagnostics.Process]::GetProcessById(${pid}) } catch { 'gone'; exit 0 }; ` +
    `try { $p.StartTime.ToFileTimeUtc() } catch { 'unknown' }`;
  let out: string;
  try {
    out = (
      await run("powershell.exe", ["-NoProfile", "-NonInteractive", "-Command", script])
    ).trim();
  } catch {
    return { kind: "unknown" };
  }
  if (out === "gone") return { kind: "gone" };
  return /^\d+$/.test(out)
    ? { kind: "time", value: BigInt(out), resolution: 1n }
    : { kind: "unknown" };
}

async function macosStartTime(pid: number, run: ProbeRunner): Promise<StartTimeObservation> {
  let out: string;
  try {
    out = (await run("ps", ["-o", "lstart=", "-p", String(pid)])).trim();
  } catch (err) {
    // `ps` exits non-zero when no process has that pid.
    return probeUnavailable(err) ? { kind: "unknown" } : { kind: "gone" };
  }
  // `lstart` (LC_ALL=C, TZ=UTC): "Wed Nov 25 11:00:00 2026", in UTC.
  const m = /^\w{3}\s+(\w{3})\s+(\d{1,2})\s+(\d{1,2}):(\d{2}):(\d{2})\s+(\d{4})$/.exec(out);
  const month = m ? MONTHS.indexOf(m[1]) : -1;
  if (!m || month < 0) return out === "" ? { kind: "gone" } : { kind: "unknown" };
  const ms = Date.UTC(Number(m[6]), month, Number(m[2]), Number(m[3]), Number(m[4]), Number(m[5]));
  if (!Number.isFinite(ms)) return { kind: "unknown" };
  return { kind: "time", value: BigInt(ms / 1000) * 1_000_000n, resolution: 1_000_000n };
}

/**
 * Twin of `beamtalk_workspace::pid_liveness::start_time_matches`: no recorded
 * start time matches; a recorded one needs an observed, equal one (compared at
 * the observation's resolution). A process that is gone is a mismatch; one
 * whose start time this host cannot observe is trusted on liveness alone.
 */
export function startTimeMatches(
  expected: bigint | undefined,
  actual: StartTimeObservation
): boolean {
  if (expected === undefined) return true;
  switch (actual.kind) {
    case "gone":
      return false;
    case "unknown":
      return true;
    case "time":
      return expected / actual.resolution === actual.value / actual.resolution;
  }
}

// ─── Beamtalk source-literal helpers ────────────────────────────────────────

/**
 * Render a JS string as a Beamtalk double-quoted string literal.
 *
 * Neutralises every construct that would otherwise be live code or break out
 * of the literal: backslash, double quote, `{` / `}` (interpolation, ADR 0138
 * `\{` / `\}` escapes) and line/NUL control characters.
 */
export function btString(value: string): string {
  const escaped = value
    .replace(/\\/g, "\\\\")
    .replace(/"/g, '\\"')
    .replace(/\{/g, "\\{")
    .replace(/\}/g, "\\}")
    .replace(/\r/g, "\\r")
    .replace(/\n/g, "\\n")
    .replace(/\t/g, "\\t")
    .replace(/\0/g, "\\0");
  return `"${escaped}"`;
}

/**
 * Render a binding name as a Beamtalk symbol literal: `#name` for a plain
 * identifier, else the documented quoted form `#'name'`. Quoted symbols have
 * no escape handling, so a name that cannot be represented is rejected.
 */
export function btSymbol(name: string): string {
  if (/^[A-Za-z_][A-Za-z0-9_]*$/.test(name)) {
    return `#${name}`;
  }
  if (/[\\\p{Cc}]/u.test(name)) {
    throw new Error(`Unrepresentable symbol name: ${JSON.stringify(name)}`);
  }
  return `#'${name.replace(/'/g, "''")}'`;
}

const SESSION_ID_RE = /^[A-Za-z0-9_-]+$/;
const CLASS_NAME_RE = /^[A-Z][A-Za-z0-9_]*$/;

/** Validate a session id scraped from terminal output before it is used in source. */
export function assertSessionId(sessionId: string): string {
  if (!SESSION_ID_RE.test(sessionId)) {
    throw new Error(`Invalid session id: ${JSON.stringify(sessionId)}`);
  }
  return sessionId;
}

/** Validate a class name as a Beamtalk identifier before it is used in source. */
export function assertClassName(className: string): string {
  if (!CLASS_NAME_RE.test(className)) {
    throw new Error(`Invalid class name: ${JSON.stringify(className)}`);
  }
  return className;
}

// ─── Default WebSocket factory (production) ──────────────────────────────────

function defaultWsFactory(url: string, callbacks: WebSocketCallbacks): IWebSocket {
  // eslint-disable-next-line @typescript-eslint/no-require-imports
  const WS = require("ws") as typeof WsModule.WebSocket;
  // ws v8: `require("ws")` gives the WebSocket class directly.
  const socket = new WS(url);

  socket.on("open", () => {
    callbacks.onOpen();
  });
  socket.on("close", (code: number, reason: Buffer) => {
    callbacks.onClose(code, reason.toString("utf8"));
  });
  socket.on("message", (data: WsModule.RawData) => {
    let text: string;
    if (typeof data === "string") {
      text = data;
    } else if (Buffer.isBuffer(data)) {
      text = data.toString("utf8");
    } else if (Array.isArray(data)) {
      // ws can deliver Buffer[] when multiple frames arrive together
      text = Buffer.concat(data).toString("utf8");
    } else {
      text = Buffer.from(data as ArrayBuffer).toString("utf8");
    }
    callbacks.onMessage(text);
  });
  socket.on("error", (err: Error) => {
    callbacks.onError(err);
  });

  return {
    get readyState() {
      return socket.readyState;
    },
    send(data: string) {
      socket.send(data);
    },
    close() {
      socket.close();
    },
  };
}

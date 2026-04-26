// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type * as WsModule from "ws";

// ─── Public types ────────────────────────────────────────────────────────────

export type ConnectionState = "connected" | "disconnected" | "reconnecting";

export interface ActorInfo {
  pid: string;
  class: string;
  spawned_at?: number;
}

export interface ActorStoppedInfo {
  pid: string;
  class: string;
  reason: string;
}

export interface ClassInfo {
  name: string;
  source_file?: string;
  actor_count?: number;
}

export interface MethodInfo {
  name: string;
  selector: string;
  side: "instance" | "class";
}

export interface StateVarInfo {
  name: string;
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

  private connectionHandlers: Array<(state: ConnectionState) => void> = [];
  private pushHandlers: Array<(event: PushEvent) => void> = [];

  constructor(
    workspaceId: string,
    port: number,
    cookie: string,
    wsFactory: WebSocketFactory = defaultWsFactory
  ) {
    this.workspaceId = workspaceId;
    this.port = port;
    this.cookie = cookie;
    this.wsFactory = wsFactory;
  }

  /**
   * Create a WorkspaceClient by reading port and cookie from the workspace
   * directory (`~/.beamtalk/workspaces/{id}/port` and `cookie`).
   *
   * @param workspaceId  Workspace ID (hash or user-assigned name).
   * @param options.baseDir  Override base directory (default: `~/.beamtalk/workspaces`).
   * @param options.wsFactory  Override WebSocket factory (for testing).
   */
  static fromFiles(
    workspaceId: string,
    options?: { baseDir?: string; wsFactory?: WebSocketFactory }
  ): WorkspaceClient {
    const base = options?.baseDir ?? path.join(os.homedir(), ".beamtalk", "workspaces");
    const wsDir = path.join(base, workspaceId);

    const portContent = fs.readFileSync(path.join(wsDir, "port"), "utf8");
    const portStr = portContent.trim().split(/\r?\n/)[0];
    if (!/^\d+$/.test(portStr)) {
      throw new Error(`Invalid port in ${path.join(wsDir, "port")}: ${JSON.stringify(portStr)}`);
    }
    const port = Number(portStr);
    if (port < 1 || port > 65535) {
      throw new Error(`Invalid port in ${path.join(wsDir, "port")}: ${JSON.stringify(portStr)}`);
    }

    const cookie = fs.readFileSync(path.join(wsDir, "cookie"), "utf8").trim();
    if (!cookie) {
      throw new Error(`Empty cookie in ${path.join(wsDir, "cookie")}`);
    }

    return new WorkspaceClient(workspaceId, port, cookie, options?.wsFactory);
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
   * List variable bindings for this connection's session.
   * The `sessionId` is sent as a correlation field in the request;
   * the server always returns bindings for this WebSocket's own session.
   */
  async bindings(sessionId: string): Promise<BindingsMap> {
    const resp = (await this._request({ op: "bindings", session: sessionId })) as {
      bindings?: BindingsMap;
    };
    return resp.bindings ?? {};
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
   * the editor's class navigation keeps working.
   */
  async classes(): Promise<ClassInfo[]> {
    const resp = (await this._request({ op: "list-classes" })) as {
      class_list?: Array<{
        name: string;
        source_file?: string | null;
        actor_count?: number;
      }>;
    };
    return (resp.class_list ?? []).map((c) => ({
      name: c.name,
      source_file: c.source_file ?? undefined,
      actor_count: c.actor_count,
    }));
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
      methods?: Array<{ name: string; selector: string; side: "instance" | "class" }>;
      state_vars?: string[];
    };
    const methods = Array.isArray(resp.methods) ? resp.methods : [];
    const stateVarsRaw = Array.isArray(resp.state_vars) ? resp.state_vars : [];
    return {
      methods,
      stateVars: stateVarsRaw.map((name) => ({ name })),
    };
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
      code: `${className} reload`,
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

  /** Unsubscribe from live log streaming. */
  async unsubscribeLogs(): Promise<void> {
    await this._request({ op: "unsubscribe-logs" });
  }

  // ─── Events ──────────────────────────────────────────────────────────────

  /**
   * Subscribe to connection state changes (connected / disconnected / reconnecting).
   * Returns a disposal function that removes the handler.
   */
  onConnectionChange(handler: (state: ConnectionState) => void): () => void {
    this.connectionHandlers.push(handler);
    return () => {
      this.connectionHandlers = this.connectionHandlers.filter((h) => h !== handler);
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
      this.ws?.send(JSON.stringify({ type: "auth", cookie: this.cookie }));
      return;
    }

    // Auth success (session-started follows)
    if (msg.type === "auth_ok") {
      return;
    }

    // Auth failure — terminal, do not reconnect (cookie won't change)
    if (msg.type === "auth_error") {
      this.authFailed = true;
      this._rejectAllPending(new Error(`Auth error: ${String(msg.message ?? "unknown")}`));
      this.ws?.close();
      return;
    }

    // Session assigned — now fully connected
    if (msg.op === "session-started") {
      this.sessionId = msg.session as string;
      this.reconnectAttempts = 0;
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

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { mkdirSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { WebSocketCallbacks, WebSocketFactory } from "../workspaceClient";
import { WORKSPACE_PROTOCOL_VERSION, WorkspaceClient } from "../workspaceClient";

// ─── Mock WebSocket ───────────────────────────────────────────────────────────

/**
 * Simulates the server side of the WebSocket connection.
 * Captures messages sent by the client and allows tests to inject responses.
 */
class MockWebSocket {
  readyState = 1;

  /** JSON-parsed messages sent by the client. */
  sent: Array<Record<string, unknown>> = [];

  private callbacks: WebSocketCallbacks;

  constructor(callbacks: WebSocketCallbacks) {
    this.callbacks = callbacks;
  }

  send(data: string): void {
    this.sent.push(JSON.parse(data) as Record<string, unknown>);
  }

  close(): void {
    this.callbacks.onClose(1000, "closed");
  }

  /** Simulate a JSON message arriving from the server. */
  receive(msg: Record<string, unknown>): void {
    this.callbacks.onMessage(JSON.stringify(msg));
  }

  /** Simulate the server closing the connection unexpectedly. */
  drop(code = 1006, reason = "connection lost"): void {
    this.callbacks.onClose(code, reason);
  }
}

// ─── Test helpers ─────────────────────────────────────────────────────────────

/**
 * Build a client + mock WS pair.
 * Calls connect() so the wsFactory fires synchronously and ws is assigned.
 */
function makeClient(): { client: WorkspaceClient; ws: MockWebSocket } {
  let ws!: MockWebSocket;
  const factory: WebSocketFactory = (_url, callbacks) => {
    ws = new MockWebSocket(callbacks);
    return ws;
  };
  const client = new WorkspaceClient("test-ws-id", 9999, "test-cookie", factory);
  client.connect(); // factory fires synchronously here → ws is now assigned
  return { client, ws };
}

/**
 * Build a client that has completed the auth handshake and is fully connected.
 * Returns the client, the mock WS, and the assigned session ID.
 */
function makeConnectedClient(): {
  client: WorkspaceClient;
  ws: MockWebSocket;
  sessionId: string;
} {
  const { client, ws } = makeClient();

  // Server drives the auth flow
  ws.receive({ op: "auth-required" });
  ws.receive({ type: "auth_ok" });
  ws.receive({ op: "session-started", session: "sess-abc" });

  return { client, ws, sessionId: "sess-abc" };
}

/**
 * Send an op request from the client and immediately resolve it with the
 * given server response payload. Returns the pending Promise.
 */
function respondTo(ws: MockWebSocket, serverPayload: Record<string, unknown>): void {
  // The last message sent is the request we want to respond to.
  const req = ws.sent[ws.sent.length - 1];
  const id = req.id as string;
  ws.receive({ id, status: ["done"], ...serverPayload });
}

// ─── Auth flow ────────────────────────────────────────────────────────────────

describe("WorkspaceClient auth flow", () => {
  it("sends cookie, vscode surface and protocol version in response to auth-required", () => {
    const { ws } = makeClient();
    ws.receive({ op: "auth-required" });
    const authMsg = ws.sent.find((m) => m.type === "auth");
    // BT-4373: the workspace gates the `vscode` surface on protocol_version
    // only; a frame without it is rejected as a pre-handshake client.
    expect(authMsg).toEqual({
      type: "auth",
      cookie: "test-cookie",
      client: "vscode",
      protocol_version: WORKSPACE_PROTOCOL_VERSION,
    });
  });

  it("speaks the protocol version defined by the runtime header", () => {
    // The canonical protocol version is `?PROTOCOL_VERSION` in beamtalk.hrl;
    // fail here (not at a user's auth handshake) if the extension drifts.
    const hrl = readFileSync(
      path.resolve(__dirname, "../../../../runtime/apps/beamtalk_runtime/include/beamtalk.hrl"),
      "utf8"
    );
    const match = /-define\(PROTOCOL_VERSION,\s*<<"([^"]+)">>\)\./.exec(hrl);
    expect(match).not.toBeNull();
    expect(WORKSPACE_PROTOCOL_VERSION).toBe(match?.[1]);
  });

  it("notifies auth-error handlers with the server message", () => {
    const { client, ws } = makeClient();
    const messages: string[] = [];
    client.onAuthError((m) => messages.push(m));
    ws.receive({ op: "auth-required" });
    ws.receive({ type: "auth_error", message: "Protocol mismatch: ..." });
    expect(messages).toEqual(["Protocol mismatch: ..."]);
    client.dispose();
  });

  it("transitions to connected after session-started", () => {
    const { client, ws } = makeClient();
    const states: string[] = [];
    client.onConnectionChange((s) => states.push(s));
    client.connect();
    ws.receive({ op: "auth-required" });
    ws.receive({ type: "auth_ok" });
    ws.receive({ op: "session-started", session: "s1" });
    expect(states).toContain("connected");
    expect(client.currentSessionId).toBe("s1");
    client.dispose();
  });

  it("does not reconnect after auth_error", async () => {
    const connectCount = { value: 0 };
    const factory: WebSocketFactory = (_url, callbacks) => {
      connectCount.value++;
      const ws = new MockWebSocket(callbacks);
      ws.receive({ op: "auth-required" });
      ws.receive({ type: "auth_error", message: "bad cookie" });
      return ws;
    };
    const client = new WorkspaceClient("id", 9999, "wrong", factory);
    client.connect();
    // Auth error should not trigger a reconnect attempt
    expect(connectCount.value).toBe(1);
    client.dispose();
  });
});

// ─── Op: classes() ───────────────────────────────────────────────────────────

describe("WorkspaceClient.classes()", () => {
  // BT-2091: classes() now sends the `list-classes` op rather than the
  // deprecated `modules` op. `list-classes` was extended in BT-2091 to
  // include `source_file` and `actor_count` so editor class navigation
  // continues to work.
  it("sends list-classes op and maps response to ClassInfo[]", async () => {
    const { client, ws } = makeConnectedClient();

    const promise = client.classes();
    const req = ws.sent[ws.sent.length - 1];
    expect(req.op).toBe("list-classes");

    respondTo(ws, {
      class_list: [
        {
          name: "Counter",
          source_file: "/p/Counter.bt",
          actor_count: 2,
          source_origin: "project",
        },
        { name: "Stack", source_file: null, actor_count: 0, source_origin: null },
      ],
    });

    const result = await promise;
    expect(result).toEqual([
      {
        name: "Counter",
        source_file: "/p/Counter.bt",
        actor_count: 2,
        source_origin: "project",
      },
      { name: "Stack", source_file: undefined, actor_count: 0, source_origin: undefined },
    ]);
    client.dispose();
  });

  it("returns [] when class_list field is absent", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.classes();
    respondTo(ws, {});
    expect(await promise).toEqual([]);
    client.dispose();
  });
});

// ─── Op: typeAliases() ────────────────────────────────────────────────────────

describe("WorkspaceClient.typeAliases()", () => {
  // ADR 0108 Phase 8 (BT-2903): typeAliases() sends `browse-type-aliases`
  // directly — unlike classes(), there is no older op to route through since
  // aliases erase entirely at compile time and have no `list-classes`-style
  // legacy surface.
  it("sends browse-type-aliases op and maps response to TypeAliasInfo[]", async () => {
    const { client, ws } = makeConnectedClient();

    const promise = client.typeAliases();
    const req = ws.sent[ws.sent.length - 1];
    expect(req.op).toBe("browse-type-aliases");

    respondTo(ws, {
      value: [
        {
          name: "RestartStrategy",
          expansion: "#temporary | #transient | #permanent",
          doc: "Restart strategy for a supervised child.",
          source_file: "src/restart_strategy.bt",
          internal: false,
        },
        {
          name: "InternalHelper",
          expansion: "Integer",
          doc: null,
          source_file: "src/helper.bt",
          internal: true,
        },
      ],
    });

    const result = await promise;
    expect(result).toEqual([
      {
        name: "RestartStrategy",
        expansion: "#temporary | #transient | #permanent",
        doc: "Restart strategy for a supervised child.",
        source_file: "src/restart_strategy.bt",
        internal: false,
      },
      {
        name: "InternalHelper",
        expansion: "Integer",
        doc: undefined,
        source_file: "src/helper.bt",
        internal: true,
      },
    ]);
    client.dispose();
  });

  it("returns [] when value field is absent", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.typeAliases();
    respondTo(ws, {});
    expect(await promise).toEqual([]);
    client.dispose();
  });

  // BT-3496: `package`/`source_origin` (alias_row/3's extension of AliasRow)
  // must reach the TS client — without them there's no way to apply the
  // stdlib-fallback exemption to type aliases the way `_hasNavigableSource`
  // already does for classes/methods/state-vars.
  it("maps package/source_origin fields (BT-3496)", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.typeAliases();
    respondTo(ws, {
      value: [
        {
          name: "Timeout",
          expansion: "Integer | #infinity",
          source_file: "src/timeout.bt",
          internal: false,
          package: "my_app",
          source_origin: "project",
        },
        {
          name: "RestartStrategy",
          expansion: "#temporary | #transient | #permanent",
          source_file: "restart_strategy.bt",
          internal: false,
          package: "beamtalk_stdlib",
          source_origin: "stdlib",
        },
      ],
    });

    const result = await promise;
    expect(result).toEqual([
      {
        name: "Timeout",
        expansion: "Integer | #infinity",
        doc: undefined,
        source_file: "src/timeout.bt",
        internal: false,
        package: "my_app",
        source_origin: "project",
      },
      {
        name: "RestartStrategy",
        expansion: "#temporary | #transient | #permanent",
        doc: undefined,
        source_file: "restart_strategy.bt",
        internal: false,
        package: "beamtalk_stdlib",
        source_origin: "stdlib",
      },
    ]);
    client.dispose();
  });
});

// ─── Op: browseAliasSource() ─────────────────────────────────────────────────

describe("WorkspaceClient.browseAliasSource()", () => {
  // BT-3314/BT-3496: unlike ClassInfo.source_file, TypeAliasInfo.source_file
  // is a package-relative display path the client can't open directly — this
  // op resolves it server-side and returns content, keyed by name+package
  // (browse-type-aliases doesn't dedupe by name across packages).
  it("sends browse-alias-source op with name and package, returns content", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.browseAliasSource("Timeout", "my_app");

    const req = ws.sent[ws.sent.length - 1];
    expect(req.op).toBe("browse-alias-source");
    expect(req.name).toBe("Timeout");
    expect(req.package).toBe("my_app");

    respondTo(ws, {
      value: {
        name: "Timeout",
        package: "my_app",
        source_file: "src/timeout.bt",
        source_origin: "project",
        editable: false,
        content: "type Timeout = Integer | #infinity\n",
      },
    });

    expect(await promise).toEqual({ content: "type Timeout = Integer | #infinity\n" });
    client.dispose();
  });

  it("omits the package param entirely when none is given", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.browseAliasSource("Timeout");

    const req = ws.sent[ws.sent.length - 1];
    expect(req.op).toBe("browse-alias-source");
    expect("package" in req).toBe(false);

    respondTo(ws, { value: { content: null } });
    await promise;
    client.dispose();
  });

  // The runtime degrades a stdlib/dependency-origin alias to `content: null`
  // rather than an error — no live source tree exists to resolve against
  // server-side for either origin (see alias_disk_content's doc).
  it("returns content: null for a stdlib/dependency-origin alias", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.browseAliasSource("RestartStrategy", "beamtalk_stdlib");
    respondTo(ws, {
      value: {
        name: "RestartStrategy",
        package: "beamtalk_stdlib",
        source_file: "restart_strategy.bt",
        source_origin: "stdlib",
        editable: false,
        content: null,
      },
    });
    expect(await promise).toEqual({ content: null });
    client.dispose();
  });

  it("returns content: null when the value field is absent", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.browseAliasSource("Missing");
    respondTo(ws, {});
    expect(await promise).toEqual({ content: null });
    client.dispose();
  });
});

// ─── Op: actors() ────────────────────────────────────────────────────────────

describe("WorkspaceClient.actors()", () => {
  it("sends actors op and returns actor list", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.actors();
    respondTo(ws, {
      actors: [
        { pid: "<0.1.0>", class: "Counter", spawned_at: 1000 },
        { pid: "<0.2.0>", class: "Worker" },
      ],
    });
    const result = await promise;
    expect(result).toHaveLength(2);
    expect(result[0]).toEqual({
      pid: "<0.1.0>",
      class: "Counter",
      spawned_at: 1000,
    });
    client.dispose();
  });

  it("returns [] when actors field is absent", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.actors();
    respondTo(ws, {});
    expect(await promise).toEqual([]);
    client.dispose();
  });
});

// ─── Op: bindings() ────────────────────────────────────────────────────────────

describe("WorkspaceClient.bindings()", () => {
  // BT-2369 (ADR 0081 Phase 6): the `bindings` op was removed. bindings() reads
  // the target session's local binding *names* through the cross-session
  // `Session withId:` API (eval runs in this WS's own session, so the protocol
  // `session` field cannot redirect it). Values are placeholders.
  it("evaluates the binding keys for the target session via Session withId:", async () => {
    const { client, ws } = makeConnectedClient();

    const promise = client.bindings("sess-xyz");

    const req = ws.sent[ws.sent.length - 1];
    expect(req.op).toBe("eval");
    expect(req.code).toBe(
      '(Session withId: "sess-xyz") ifNil: [#()] ifNotNil: [:s | s bindings keys]'
    );

    respondTo(ws, { value: ["x", "y"] });

    const result = await promise;
    expect(Object.keys(result).sort()).toEqual(["x", "y"]);
    client.dispose();
  });

  it("returns an empty map when the session has no locals or is unknown", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.bindings("sess-empty");
    respondTo(ws, { value: [] });
    const result = await promise;
    expect(result).toEqual({});
    client.dispose();
  });

  it("rejects a session id that is not [A-Za-z0-9_-]+ (BT-5069)", async () => {
    const { client, ws } = makeConnectedClient();
    const sent = ws.sent.length;
    await expect(client.bindings('a"b\\c{1 + 1}')).rejects.toThrow("Invalid session id");
    expect(ws.sent.length).toBe(sent);
    client.dispose();
  });
});

describe("WorkspaceClient.bindingValue()", () => {
  // BT-2369: reads a single binding's value via the cross-session Session API.
  it("evaluates `s bindings at: #name` for the target session", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.bindingValue("sess-xyz", "counter");
    const req = ws.sent[ws.sent.length - 1];
    expect(req.op).toBe("eval");
    expect(req.code).toBe(
      '(Session withId: "sess-xyz") ifNil: [nil] ifNotNil: [:s | s bindings at: #counter]'
    );
    respondTo(ws, { value: 42 });
    expect(await promise).toBe(42);
    client.dispose();
  });

  it("quotes binding names that are not plain identifiers", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.bindingValue("sess-xyz", "has space");
    const req = ws.sent[ws.sent.length - 1];
    expect(req.code).toBe(
      `(Session withId: "sess-xyz") ifNil: [nil] ifNotNil: [:s | s bindings at: #'has space']`
    );
    respondTo(ws, { value: null });
    await promise;
    client.dispose();
  });

  it("returns null when the value is absent", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.bindingValue("sess-xyz", "missing");
    respondTo(ws, {});
    expect(await promise).toBeNull();
    client.dispose();
  });
});

// ─── Op: reload() ────────────────────────────────────────────────────────────

describe("WorkspaceClient.reload()", () => {
  // BT-2091: reload() now evaluates `<className> reload` rather than calling
  // the deprecated `reload` op. The class name (not source path) is the input.
  it("sends `<class> reload` eval and returns the reloaded class", async () => {
    const { client, ws } = makeConnectedClient();

    const promise = client.reload("Counter");
    const req = ws.sent[ws.sent.length - 1];
    expect(req.op).toBe("eval");
    expect(req.code).toBe("Counter reload");

    respondTo(ws, { value: "Counter", warnings: [] });

    const result = await promise;
    expect(result.classes).toEqual([{ name: "Counter" }]);
    expect(result.warnings).toEqual([]);
    client.dispose();
  });

  it("includes warnings when server reports class collisions", async () => {
    const { client, ws } = makeConnectedClient();

    const promise = client.reload("Counter");
    respondTo(ws, {
      value: "Counter",
      warnings: ["Class 'Counter' redefined (was counter@v1, now counter@v2)"],
    });

    const result = await promise;
    expect(result.warnings).toEqual(["Class 'Counter' redefined (was counter@v1, now counter@v2)"]);
    client.dispose();
  });

  it("returns empty warnings when server omits the field", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.reload("Stack");
    respondTo(ws, { value: "Stack" });
    const result = await promise;
    expect(result.warnings).toEqual([]);
    client.dispose();
  });

  it("rejects when server returns an error response", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.reload("Bad");
    const req = ws.sent[ws.sent.length - 1];
    ws.receive({
      id: req.id,
      status: ["done", "error"],
      error: "class not found: Bad",
    });
    await expect(promise).rejects.toThrow("class not found");
    client.dispose();
  });

  it("rejects immediately when not connected", async () => {
    // Build a client without connecting (factory never fires, state stays disconnected)
    const factory: WebSocketFactory = (_url, _cb) => {
      throw new Error("factory should not be called");
    };
    const client = new WorkspaceClient("id", 9999, "cookie", factory);
    // Don't call connect() — client stays in disconnected state
    await expect(client.reload("Counter")).rejects.toThrow("Not connected");
    client.dispose();
  });
});

// ─── Op: methods() ───────────────────────────────────────────────────────────

describe("WorkspaceClient.methods()", () => {
  it("sends methods op with class name and returns method list", async () => {
    const { client, ws } = makeConnectedClient();

    const promise = client.methods("Counter");
    const req = ws.sent[ws.sent.length - 1];
    expect(req.op).toBe("methods");
    expect(req.class).toBe("Counter");

    respondTo(ws, {
      methods: [
        { name: "increment", selector: "increment", side: "instance", line: 14 },
        { name: "new", selector: "new", side: "class", line: null },
      ],
      state_vars: [{ name: "count", line: 5 }],
    });

    const result = await promise;
    expect(result.methods).toHaveLength(2);
    expect(result.methods[0]).toEqual({
      name: "increment",
      selector: "increment",
      side: "instance",
      line: 14,
    });
    expect(result.stateVars).toEqual([{ name: "count", line: 5 }]);
    client.dispose();
  });

  it("normalizes a missing/null line to undefined (BT-3439)", async () => {
    // A class predating the line-tracking feature, or ClassBuilder-built —
    // the server sends `line: null` (or omits the key); callers must see
    // `undefined`, not `null`, for a single `??`/optional-chaining check.
    const { client, ws } = makeConnectedClient();

    const promise = client.methods("LegacyWidget");
    respondTo(ws, {
      methods: [{ name: "render", selector: "render", side: "instance", line: null }],
      state_vars: [{ name: "width" }],
    });

    const result = await promise;
    expect(result.methods[0].line).toBeUndefined();
    expect(result.stateVars[0].line).toBeUndefined();
    client.dispose();
  });
});

// ─── Push events ─────────────────────────────────────────────────────────────

describe("WorkspaceClient push events", () => {
  it("emits class loaded push event", () => {
    const { client, ws } = makeConnectedClient();
    const events: Array<{ channel: string; event: string }> = [];
    client.onPush((e) => events.push({ channel: e.channel, event: "event" in e ? e.event : "" }));

    ws.receive({
      type: "push",
      channel: "classes",
      event: "loaded",
      data: { class: "Counter" },
    });

    expect(events).toHaveLength(1);
    expect(events[0].channel).toBe("classes");
    client.dispose();
  });

  it("emits actor spawned push event", () => {
    const { client, ws } = makeConnectedClient();
    const spawned: string[] = [];
    client.onPush((e) => {
      if (e.channel === "actors" && e.event === "spawned") {
        spawned.push(e.data.pid);
      }
    });

    ws.receive({
      type: "push",
      channel: "actors",
      event: "spawned",
      data: { pid: "<0.42.0>", class: "Counter" },
    });

    expect(spawned).toEqual(["<0.42.0>"]);
    client.dispose();
  });
});

// ─── Reconnect, timeout and pending rejection (BT-5082) ──────────────────────

describe("WorkspaceClient reconnect, timeout and close handling", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    vi.spyOn(Math, "random").mockReturnValue(0); // no jitter: deterministic delays
  });
  afterEach(() => {
    vi.useRealTimers();
    vi.restoreAllMocks();
  });

  function makeMultiClient() {
    const sockets: MockWebSocket[] = [];
    const factory: WebSocketFactory = (_url, callbacks) => {
      const s = new MockWebSocket(callbacks);
      sockets.push(s);
      return s;
    };
    const client = new WorkspaceClient("test-ws-id", 9999, "test-cookie", factory);
    const states: string[] = [];
    client.onConnectionChange((s) => states.push(s));
    client.connect();
    const handshake = (ws: MockWebSocket) => {
      ws.receive({ op: "auth-required" });
      ws.receive({ type: "auth_ok" });
      ws.receive({ op: "session-started", session: "sess-abc" });
    };
    return { client, sockets, states, handshake };
  }

  it("reconnects with exponentially growing delays, capped at 30s", async () => {
    const { client, sockets, handshake } = makeMultiClient();
    handshake(sockets[0]);

    sockets[0].drop();
    const expected = [1000, 2000, 4000, 8000, 16000, 30000, 30000];
    for (let i = 0; i < expected.length; i++) {
      const before = sockets.length;
      await vi.advanceTimersByTimeAsync(expected[i] - 1);
      expect(sockets.length).toBe(before); // not yet
      await vi.advanceTimersByTimeAsync(1);
      expect(sockets.length).toBe(before + 1); // fired exactly at the delay
      sockets[sockets.length - 1].drop(); // attempt fails, back off further
    }
    client.dispose();
  });

  it("resets the backoff after a successful reconnect", async () => {
    const { client, sockets, states, handshake } = makeMultiClient();
    handshake(sockets[0]);
    sockets[0].drop();
    await vi.advanceTimersByTimeAsync(1000);
    sockets[1].drop();
    await vi.advanceTimersByTimeAsync(2000);
    handshake(sockets[2]);
    expect(states.at(-1)).toBe("connected");

    sockets[2].drop();
    const before = sockets.length;
    await vi.advanceTimersByTimeAsync(1000); // back to the base delay
    expect(sockets.length).toBe(before + 1);
    client.dispose();
  });

  // BT-5189: actor panels are closed only when a reconnect reached a
  // restarted node — told apart by the port file's nonce, which a restart
  // rewrites — never on a transient drop to the same node.
  it("reports a node change only when the reconnect reached a restarted node", async () => {
    const base = mkdtempSync(path.join(tmpdir(), "bt-ws-"));
    const dir = path.join(base, "ws1");
    mkdirSync(dir);
    writeFileSync(path.join(dir, "port"), "4000\nnonce-a");
    writeFileSync(path.join(dir, "cookie"), "secret");
    const nodeInfo = (pid: number) =>
      writeFileSync(
        path.join(dir, "node.info"),
        JSON.stringify({ port: 4000, pid, nonce: "nonce-a" })
      );
    nodeInfo(process.pid);
    const sockets: MockWebSocket[] = [];
    const client = await WorkspaceClient.fromFiles("ws1", {
      baseDir: base,
      wsFactory: (_url, callbacks) => {
        const s = new MockWebSocket(callbacks);
        sockets.push(s);
        return s;
      },
    });
    const handshake = (ws: MockWebSocket) => {
      ws.receive({ op: "auth-required" });
      ws.receive({ type: "auth_ok" });
      ws.receive({ op: "session-started", session: "sess-abc" });
    };
    client.connect();
    handshake(sockets[0]);
    expect(client.nodeChangedOnLastConnect).toBe(false);

    // Transient drop, same node (same nonce): not a change.
    sockets[0].drop();
    await vi.advanceTimersByTimeAsync(1000);
    handshake(sockets[1]);
    expect(client.state).toBe("connected");
    expect(client.nodeChangedOnLastConnect).toBe(false);

    // The node restarted on the same port: a fresh nonce.
    writeFileSync(path.join(dir, "port"), "4000\nnonce-b");
    sockets[1].drop();
    await vi.advanceTimersByTimeAsync(1000);
    handshake(sockets[2]);
    expect(client.nodeChangedOnLastConnect).toBe(true);

    // The node crashed and a successor took the port before rewriting the
    // files: the old nonce lingers, but the recorded node pid is dead.
    sockets[2].drop();
    nodeInfo(2147483646);
    await vi.advanceTimersByTimeAsync(1000);
    handshake(sockets[3]);
    expect(client.nodeChangedOnLastConnect).toBe(true);
    client.dispose();
  });

  it("assumes a new node on reconnect when the node's identity cannot be read", async () => {
    const { client, sockets, handshake } = makeMultiClient();
    handshake(sockets[0]);
    expect(client.nodeChangedOnLastConnect).toBe(false);
    sockets[0].drop();
    await vi.advanceTimersByTimeAsync(1000);
    handshake(sockets[1]);
    expect(client.nodeChangedOnLastConnect).toBe(true);
    client.dispose();
  });

  it("stops reconnecting once disposed", async () => {
    const { client, sockets, handshake } = makeMultiClient();
    handshake(sockets[0]);
    sockets[0].drop();
    client.dispose();
    await vi.advanceTimersByTimeAsync(60_000);
    expect(sockets).toHaveLength(1);
  });

  it("rejects a request that gets no response within the timeout", async () => {
    const { client, sockets, handshake } = makeMultiClient();
    handshake(sockets[0]);
    const p = client.subscribeLogs("info");
    const assertion = expect(p).rejects.toThrow(/timed out after 30000ms: op=subscribe-logs/);
    await vi.advanceTimersByTimeAsync(30_000);
    await assertion;
    client.dispose();
  });

  it("rejects every in-flight request when the connection drops", async () => {
    const { client, sockets, handshake } = makeMultiClient();
    handshake(sockets[0]);
    const a = client.subscribeLogs("info");
    const b = client.actors();
    const rejected = Promise.allSettled([a, b]);
    sockets[0].drop();
    const results = await rejected;
    expect(results.map((r) => r.status)).toEqual(["rejected", "rejected"]);
    client.dispose();
  });

  it("rejects in-flight requests on dispose", async () => {
    const { client, sockets, handshake } = makeMultiClient();
    handshake(sockets[0]);
    const p = client.actors();
    const assertion = expect(p).rejects.toThrow(/WebSocket closed|WorkspaceClient disposed/);
    client.dispose();
    await assertion;
  });
});

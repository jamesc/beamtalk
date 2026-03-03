// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { describe, expect, it } from "vitest";
import type { WebSocketCallbacks, WebSocketFactory } from "../workspaceClient";
import { WorkspaceClient } from "../workspaceClient";

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
function makeConnectedClient(): { client: WorkspaceClient; ws: MockWebSocket; sessionId: string } {
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
  it("sends cookie in response to auth-required challenge", () => {
    const { ws } = makeClient();
    ws.receive({ op: "auth-required" });
    const authMsg = ws.sent.find((m) => m.type === "auth");
    expect(authMsg).toEqual({ type: "auth", cookie: "test-cookie" });
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
  it("sends modules op and maps response to ClassInfo[]", async () => {
    const { client, ws } = makeConnectedClient();

    const promise = client.classes();
    respondTo(ws, {
      modules: [
        { name: "Counter", source_file: "/src/counter.bt", actor_count: 2 },
        { name: "Stack", source_file: "/src/stack.bt", actor_count: 0 },
      ],
    });

    const result = await promise;
    expect(result).toEqual([
      { name: "Counter", source_file: "/src/counter.bt", actor_count: 2 },
      { name: "Stack", source_file: "/src/stack.bt", actor_count: 0 },
    ]);
    const req = ws.sent.find((m) => m.op === "modules");
    expect(req).toBeDefined();
    client.dispose();
  });

  it("returns [] when modules field is absent", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.classes();
    respondTo(ws, {});
    expect(await promise).toEqual([]);
    client.dispose();
  });

  it("includes source_file: undefined when omitted by server", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.classes();
    respondTo(ws, { modules: [{ name: "Foo" }] });
    const result = await promise;
    expect(result[0].name).toBe("Foo");
    expect(result[0].source_file).toBeUndefined();
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
    expect(result[0]).toEqual({ pid: "<0.1.0>", class: "Counter", spawned_at: 1000 });
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

// ─── Op: reload() ────────────────────────────────────────────────────────────

describe("WorkspaceClient.reload()", () => {
  it("sends reload op with path and returns reloaded classes", async () => {
    const { client, ws } = makeConnectedClient();

    const promise = client.reload("/workspace/src/counter.bt");
    const req = ws.sent[ws.sent.length - 1];
    expect(req.op).toBe("reload");
    expect(req.path).toBe("/workspace/src/counter.bt");

    respondTo(ws, {
      classes: [{ name: "Counter", source_file: "/workspace/src/counter.bt", actor_count: 1 }],
      warnings: [],
    });

    const result = await promise;
    expect(result.classes).toEqual([
      { name: "Counter", source_file: "/workspace/src/counter.bt", actor_count: 1 },
    ]);
    expect(result.warnings).toEqual([]);
    client.dispose();
  });

  it("includes warnings when server reports class collisions", async () => {
    const { client, ws } = makeConnectedClient();

    const promise = client.reload("/workspace/src/counter.bt");
    respondTo(ws, {
      classes: [{ name: "Counter", source_file: "/workspace/src/counter.bt" }],
      warnings: ["Class 'Counter' redefined (was counter@v1, now counter@v2)"],
    });

    const result = await promise;
    expect(result.warnings).toEqual(["Class 'Counter' redefined (was counter@v1, now counter@v2)"]);
    client.dispose();
  });

  it("returns empty warnings when server omits the field", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.reload("/workspace/src/stack.bt");
    respondTo(ws, { classes: [{ name: "Stack" }] });
    const result = await promise;
    expect(result.warnings).toEqual([]);
    client.dispose();
  });

  it("rejects when server returns an error response", async () => {
    const { client, ws } = makeConnectedClient();
    const promise = client.reload("/workspace/src/bad.bt");
    const req = ws.sent[ws.sent.length - 1];
    ws.receive({
      id: req.id,
      status: ["done", "error"],
      error: "file not found: /workspace/src/bad.bt",
    });
    await expect(promise).rejects.toThrow("file not found");
    client.dispose();
  });

  it("rejects immediately when not connected", async () => {
    // Build a client without connecting (factory never fires, state stays disconnected)
    const factory: WebSocketFactory = (_url, _cb) => {
      throw new Error("factory should not be called");
    };
    const client = new WorkspaceClient("id", 9999, "cookie", factory);
    // Don't call connect() — client stays in disconnected state
    await expect(client.reload("/some/file.bt")).rejects.toThrow("Not connected");
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
        { name: "increment", selector: "increment", side: "instance" },
        { name: "new", selector: "new", side: "class" },
      ],
    });

    const result = await promise;
    expect(result).toHaveLength(2);
    expect(result[0]).toEqual({ name: "increment", selector: "increment", side: "instance" });
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

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

const { panels } = vi.hoisted(() => ({
  panels: [] as Array<{
    disposed: boolean;
    posted: unknown[];
    send: (msg: unknown) => void;
  }>,
}));

vi.mock("vscode", () => ({
  ViewColumn: { Two: 2 },
  window: {
    createWebviewPanel: () => {
      const state = {
        disposed: false,
        posted: [] as unknown[],
        send: (_msg: unknown) => {},
      };
      panels.push(state);
      let onDispose: () => void = () => {};
      return {
        webview: {
          html: "",
          onDidReceiveMessage: (cb: (msg: unknown) => void) => {
            state.send = cb;
            return { dispose() {} };
          },
          postMessage: (m: unknown) => {
            state.posted.push(m);
            return Promise.resolve(true);
          },
        },
        onDidDispose: (cb: () => void) => {
          onDispose = cb;
          return { dispose() {} };
        },
        reveal: () => {},
        dispose: () => {
          state.disposed = true;
          onDispose();
        },
      };
    },
  },
}));

import { InspectorPanel } from "../inspectorPanel";
import type { WorkspaceClient } from "../workspaceClient";

function fakeClient(value: unknown): WorkspaceClient {
  return {
    bindingValue: vi.fn().mockResolvedValue(value),
    inspect: vi.fn().mockResolvedValue({}),
    state: "connected",
    onConnectionChange: () => () => {},
  } as unknown as WorkspaceClient;
}

/** A client that is still connecting when handed to `setClient` (BT-5130). */
function connectingClient(value: unknown) {
  const handlers: Array<(s: string) => void> = [];
  const bindingValue = vi.fn().mockImplementation(async () => {
    if (client.state !== "connected") throw new Error("Not connected to workspace");
    return value;
  });
  const client = {
    bindingValue,
    inspect: vi.fn(),
    state: "reconnecting",
    onConnectionChange: (h: (s: string) => void) => {
      handlers.push(h);
      return () => handlers.splice(handlers.indexOf(h), 1);
    },
  } as unknown as WorkspaceClient & { state: string };
  return {
    client,
    bindingValue,
    connect: () => {
      client.state = "connected";
      for (const h of [...handlers]) h("connected");
    },
  };
}

const flush = async () => {
  for (let i = 0; i < 5; i++) await Promise.resolve();
};

describe("InspectorPanel.setClient (BT-5082)", () => {
  beforeEach(() => {
    InspectorPanel.disposeAll();
    panels.length = 0;
  });

  it("closes actor panels when the node goes away or changes", () => {
    const a = fakeClient(1);
    InspectorPanel.show(a, { kind: "actor", info: { pid: "<0.1.0>", class: "Foo" } });
    expect(panels[0].disposed).toBe(false);

    InspectorPanel.setClient(fakeClient(2));

    expect(panels[0].disposed).toBe(true);
  });

  it("keeps binding panels but refetches them through the new client", async () => {
    const a = fakeClient("old");
    InspectorPanel.show(a, { kind: "binding", name: "x", value: "", sessionId: "s1" });
    await flush();

    const b = fakeClient("new");
    InspectorPanel.setClient(b);
    await flush();

    expect(panels[0].disposed).toBe(false);
    expect(b.bindingValue).toHaveBeenCalledWith("s1", "x");
    expect(a.bindingValue).toHaveBeenCalledTimes(1);
  });

  it("is a no-op for panels already on that client", async () => {
    const a = fakeClient("v");
    InspectorPanel.show(a, { kind: "actor", info: { pid: "<0.2.0>", class: "Foo" } });
    InspectorPanel.setClient(a);
    expect(panels[0].disposed).toBe(false);
  });
});

describe("InspectorPanel reconnect (BT-5130)", () => {
  beforeEach(() => {
    InspectorPanel.disposeAll();
    panels.length = 0;
  });

  it("does not fetch until the new client connects, then refetches", async () => {
    InspectorPanel.show(fakeClient("old"), {
      kind: "binding",
      name: "x",
      value: "",
      sessionId: "s1",
    });
    await flush();
    const next = connectingClient("fresh");
    InspectorPanel.setClient(next.client);
    await flush();
    expect(next.bindingValue).not.toHaveBeenCalled();
    expect(JSON.stringify(panels[0].posted)).not.toContain("Not connected");

    next.connect();
    await flush();
    expect(next.bindingValue).toHaveBeenCalledWith("s1", "x");
    expect(JSON.stringify(panels[0].posted)).toContain("fresh");
  });

  it("shows a session-ended state when the session is gone after a reconnect", async () => {
    InspectorPanel.show(fakeClient("old"), {
      kind: "binding",
      name: "x",
      value: "",
      sessionId: "gone",
    });
    await flush();
    const next = connectingClient("unused");
    next.bindingValue.mockRejectedValue(new Error("no such session"));
    InspectorPanel.setClient(next.client);
    next.connect();
    await flush();
    expect(JSON.stringify(panels[0].posted)).toContain("Session ended");
  });

  it("closes actor panels on closeActorPanels", () => {
    InspectorPanel.show(fakeClient(1), { kind: "actor", info: { pid: "<0.9.0>", class: "Foo" } });
    InspectorPanel.closeActorPanels();
    expect(panels[0].disposed).toBe(true);
  });
});

describe("InspectorPanel error states and waiters (BT-5189)", () => {
  beforeEach(() => {
    InspectorPanel.disposeAll();
    panels.length = 0;
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  const binding = { kind: "binding" as const, name: "x", value: "", sessionId: "s1" };
  const lastPosted = (i = 0) => JSON.stringify(panels[i].posted.at(-1));

  it("shows the real error once the new client has served the binding", async () => {
    InspectorPanel.show(fakeClient("old"), binding);
    await flush();
    const next = connectingClient("fresh");
    InspectorPanel.setClient(next.client);
    next.connect();
    await flush();
    expect(lastPosted()).toContain("fresh");

    // The session is alive on the new client; a later failure is just that.
    next.bindingValue.mockRejectedValue(new Error("boom"));
    panels[0].send({ type: "refresh" });
    await flush();
    expect(lastPosted()).toContain("boom");
    expect(lastPosted()).not.toContain("Session ended");
  });

  it("says what failed alongside session-ended after a client change", async () => {
    InspectorPanel.show(fakeClient("old"), binding);
    await flush();
    const next = connectingClient("unused");
    next.bindingValue.mockRejectedValue(new Error("unknown session s1"));
    InspectorPanel.setClient(next.client);
    next.connect();
    await flush();
    expect(lastPosted()).toContain("Session ended");
    expect(lastPosted()).toContain("unknown session s1");
  });

  it("show() on an existing panel replaces its connect waiter (one refetch, not two)", async () => {
    const next = connectingClient("v");
    InspectorPanel.show(next.client, binding);
    InspectorPanel.show(next.client, binding);
    await flush();
    expect(next.bindingValue).not.toHaveBeenCalled();

    next.connect();
    await flush();
    expect(next.bindingValue).toHaveBeenCalledTimes(1);
    expect(lastPosted()).toContain("v");
  });

  it("stops waiting and says so when the client never connects", async () => {
    vi.useFakeTimers();
    const next = connectingClient("never");
    InspectorPanel.show(next.client, binding);
    expect(lastPosted()).toContain("Loading");

    await vi.advanceTimersByTimeAsync(InspectorPanel.CONNECT_WAIT_MS);
    expect(lastPosted()).toContain("did not connect in time");

    // The waiter is gone: a late connect does not refetch behind the message.
    next.connect();
    await flush();
    expect(next.bindingValue).not.toHaveBeenCalled();
  });

  it("drops its waiter when disposed", async () => {
    const next = connectingClient("v");
    InspectorPanel.show(next.client, binding);
    InspectorPanel.disposeAll();
    next.connect();
    await flush();
    expect(next.bindingValue).not.toHaveBeenCalled();
  });

  it("keeps the last value when the workspace goes away", async () => {
    InspectorPanel.show(fakeClient("kept"), binding);
    await flush();
    expect(lastPosted()).toContain("kept");

    InspectorPanel.setClient(null);
    await flush();
    expect(lastPosted()).toContain("kept");
  });
});

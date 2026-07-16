// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { beforeEach, describe, expect, it, vi } from "vitest";
import type { WebSocketCallbacks, WebSocketFactory } from "../workspaceClient";
import { WorkspaceClient } from "../workspaceClient";
import { WorkspaceTreeDataProvider } from "../workspaceTreeView";

// ─── Minimal `vscode` module mock ──────────────────────────────────────────────
//
// `workspaceTreeView.ts` runs inside the extension host in production, where
// `vscode` is a real module injected by VS Code. Under `vitest` there is no
// extension host, so `vscode` must be mocked before the module under test is
// imported. Only the runtime VALUES the tree view actually constructs/calls
// need a fake — everything else it imports from `vscode` (`CancellationToken`,
// `DocumentSymbol`, `Uri`, `SymbolKind`, …) is used purely in type positions,
// erased at compile time, and never touched at runtime by the code paths these
// tests exercise.
vi.mock("vscode", () => {
  class ThemeIcon {
    constructor(public id: string) {}
  }
  class MarkdownString {
    constructor(public value?: string) {}
  }
  class TreeItem {
    label: string;
    collapsibleState: number;
    description?: string;
    iconPath?: unknown;
    contextValue?: string;
    tooltip?: unknown;
    command?: unknown;
    constructor(label: string, collapsibleState?: number) {
      this.label = label;
      this.collapsibleState = collapsibleState ?? 0;
    }
  }
  class EventEmitter<T> {
    private listeners: Array<(value: T) => void> = [];
    event = (listener: (value: T) => void) => {
      this.listeners.push(listener);
      return { dispose: () => {} };
    };
    fire(value: T): void {
      for (const listener of this.listeners) listener(value);
    }
    dispose(): void {
      this.listeners = [];
    }
  }
  return {
    TreeItemCollapsibleState: { None: 0, Collapsed: 1, Expanded: 2 },
    ThemeIcon,
    MarkdownString,
    TreeItem,
    EventEmitter,
    // Referenced only in type positions or inside functions this suite never
    // calls (document-symbol / hover navigation helpers elsewhere in the
    // file) — present as harmless stand-ins so a stray runtime reference
    // doesn't throw `undefined is not a function`.
    commands: { executeCommand: vi.fn() },
    window: {},
    workspace: {},
  };
});

// ─── Mock WebSocket (mirrors workspaceClient.test.ts) ──────────────────────────

class MockWebSocket {
  readyState = 1;
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

  receive(msg: Record<string, unknown>): void {
    this.callbacks.onMessage(JSON.stringify(msg));
  }
}

/** Build a client fully connected to a mock WS, plus the WS itself. */
function makeConnectedClient(): {
  client: InstanceType<typeof WorkspaceClient>;
  ws: MockWebSocket;
} {
  let ws!: MockWebSocket;
  const factory: WebSocketFactory = (_url, callbacks) => {
    ws = new MockWebSocket(callbacks);
    return ws;
  };
  const client = new WorkspaceClient("test-ws-id", 9999, "test-cookie", factory);
  client.connect();
  ws.receive({ op: "auth-required" });
  ws.receive({ type: "auth_ok" });
  ws.receive({ op: "session-started", session: "sess-abc" });
  return { client, ws };
}

/** Respond to the most recent request matching `op` (order-independent). */
function respondToOp(ws: MockWebSocket, op: string, serverPayload: Record<string, unknown>): void {
  const req = [...ws.sent].reverse().find((m) => m.op === op);
  if (!req) throw new Error(`no pending request for op ${op}`);
  const id = req.id as string;
  ws.receive({ id, status: ["done"], ...serverPayload });
}

/**
 * Answer all four of `_fetchInitialData`'s in-flight requests (bindings via
 * `eval`, actors, classes, and the new `browse-type-aliases`) and flush the
 * microtask queue enough times for the `Promise.allSettled` chain (plus each
 * op wrapper's own `.then`-style response mapping) to fully resolve.
 */
async function respondToInitialFetch(
  ws: MockWebSocket,
  typeAliasesValue: unknown[] = []
): Promise<void> {
  respondToOp(ws, "eval", { value: [] });
  respondToOp(ws, "actors", {});
  respondToOp(ws, "list-classes", { class_list: [] });
  respondToOp(ws, "browse-type-aliases", { value: typeAliasesValue });
  for (let i = 0; i < 5; i++) {
    await Promise.resolve();
  }
}

// ─── Tests ──────────────────────────────────────────────────────────────────

describe("WorkspaceTreeDataProvider — Type Aliases section (ADR 0108 Phase 8, BT-2903)", () => {
  let provider: InstanceType<typeof WorkspaceTreeDataProvider>;

  beforeEach(() => {
    provider = new WorkspaceTreeDataProvider();
  });

  it("the connected root includes a Type Aliases section sibling to Classes", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws);

    const [root] = await provider.getChildren();
    const rootChildren = await provider.getChildren(root);
    const kinds = rootChildren.map((n) => n.kind);

    expect(kinds).toContain("classes-section");
    expect(kinds).toContain("type-aliases-section");

    client.dispose();
  });

  it("renders a 'Type Aliases (N)' section label with the alias count", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws, [
      {
        name: "RestartStrategy",
        expansion: "#temporary | #transient | #permanent",
        doc: "Restart strategy for a supervised child.",
        source_file: "src/restart_strategy.bt",
        internal: false,
      },
      {
        name: "TimeoutMs",
        expansion: "Integer",
        doc: null,
        source_file: "src/timeout.bt",
        internal: false,
      },
    ]);

    const sectionItem = provider.getTreeItem({ kind: "type-aliases-section" });
    expect(sectionItem.label).toBe("Type Aliases");
    expect(sectionItem.description).toBe("(2)");

    const children = await provider.getChildren({ kind: "type-aliases-section" });
    expect(children).toHaveLength(2);
    expect(children.map((c) => (c as { info: { name: string } }).info.name)).toEqual([
      "RestartStrategy",
      "TimeoutMs",
    ]);

    client.dispose();
  });

  it("an alias item renders its expansion inline and is a leaf node", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws, [
      {
        name: "RestartStrategy",
        expansion: "#temporary | #transient | #permanent",
        doc: "Restart strategy for a supervised child.",
        source_file: "src/restart_strategy.bt",
        internal: false,
      },
    ]);

    const [aliasNode] = await provider.getChildren({ kind: "type-aliases-section" });
    const item = provider.getTreeItem(aliasNode);

    expect(item.label).toBe("RestartStrategy");
    expect(item.description).toBe("= #temporary | #transient | #permanent");
    // Leaf: TreeItemCollapsibleState.None === 0 (see the vscode mock above).
    expect(item.collapsibleState).toBe(0);

    // No children — an alias has no methods/state to expand into.
    const grandchildren = await provider.getChildren(aliasNode);
    expect(grandchildren).toEqual([]);

    client.dispose();
  });

  it("section is empty (with a '(none)' description) when no aliases are declared", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws, []);

    const sectionItem = provider.getTreeItem({ kind: "type-aliases-section" });
    expect(sectionItem.description).toBe("(none)");

    const children = await provider.getChildren({ kind: "type-aliases-section" });
    expect(children).toEqual([]);

    client.dispose();
  });

  it("resets the Type Aliases section on disconnect", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws, [
      { name: "RestartStrategy", expansion: "Integer", internal: false },
    ]);

    expect(await provider.getChildren({ kind: "type-aliases-section" })).toHaveLength(1);

    provider.setClient(null);

    expect(await provider.getChildren({ kind: "type-aliases-section" })).toEqual([]);
  });
});

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { beforeEach, describe, expect, it, vi } from "vitest";
import { buildVscodeModule } from "./vscodeMock";

// ─── `vscode` module mock (shared with resolveTreeItem.test.ts) ───────────────
//
// `workspaceTreeView.ts` runs inside the extension host in production, where
// `vscode` is a real module injected by VS Code. Under `vitest` there is no
// extension host, so `vscode` must be mocked before the module under test is
// imported. `executeCommand`/`openTextDocument` are referenced only in type
// positions or inside functions this suite never calls (document-symbol /
// hover navigation helpers elsewhere in the file) — present as harmless
// stand-ins so a stray runtime reference doesn't throw `undefined is not a
// function`.
//
// NOTE: `./vscodeMock` must be imported before `../workspaceClient` /
// `../workspaceTreeView` below — those transitively import the real `vscode`
// module, and the mock factory needs `buildVscodeModule` already resolved by
// the time that import is evaluated.
const { executeCommandMock, openTextDocumentMock } = vi.hoisted(() => ({
  executeCommandMock: vi.fn(),
  openTextDocumentMock: vi.fn(),
}));

vi.mock("vscode", () => buildVscodeModule({ executeCommandMock, openTextDocumentMock }));

import type { WebSocketCallbacks, WebSocketFactory } from "../workspaceClient";
import { WorkspaceClient } from "../workspaceClient";
import type {
  InheritedMethodGroupNode,
  MethodGroupNode,
  MethodItemNode,
} from "../workspaceTreeView";
import { WorkspaceTreeDataProvider } from "../workspaceTreeView";

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

  it("an alias item with a source file wires up Go to Definition", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws, [
      { name: "Timeout", expansion: "Integer | #infinity", source_file: "src/timeout.bt" },
    ]);

    const [aliasNode] = await provider.getChildren({ kind: "type-aliases-section" });
    const item = provider.getTreeItem(aliasNode);

    expect(item.contextValue).toBe("type-alias-item");
    expect(item.command).toEqual({
      command: "beamtalk.navigateToTypeAlias",
      title: "Go to Definition",
      arguments: [aliasNode],
    });

    client.dispose();
  });

  it("an alias item with no source file does not wire up Go to Definition", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws, [{ name: "LogLevel", expansion: "#info | #warning" }]);

    const [aliasNode] = await provider.getChildren({ kind: "type-aliases-section" });
    const item = provider.getTreeItem(aliasNode);

    expect(item.contextValue).toBe("type-alias-item-no-source");
    expect(item.command).toBeUndefined();

    client.dispose();
  });

  // BT-3496: `browse-alias-source` only ever resolves real content for a
  // project-origin alias (the runtime has no live source tree to resolve a
  // stdlib/dependency-origin alias's `source_file` against server-side, and
  // always returns `content: null` for either) — so the navigability badge
  // must be keyed off `source_origin`, not merely a present `source_file`
  // (every alias row, from every origin, carries *some* `source_file`).
  describe("navigability badge is keyed off source_origin (BT-3496)", () => {
    it("is navigable for a project-origin alias", async () => {
      const { client, ws } = makeConnectedClient();
      provider.setClient(client);
      await respondToInitialFetch(ws, [
        {
          name: "Timeout",
          expansion: "Integer | #infinity",
          source_file: "src/timeout.bt",
          package: "my_app",
          source_origin: "project",
        },
      ]);

      const [aliasNode] = await provider.getChildren({ kind: "type-aliases-section" });
      const item = provider.getTreeItem(aliasNode);

      expect(item.contextValue).toBe("type-alias-item");
      expect(item.command).toEqual({
        command: "beamtalk.navigateToTypeAlias",
        title: "Go to Definition",
        arguments: [aliasNode],
      });
      client.dispose();
    });

    it("is NOT navigable for a stdlib-origin alias, even though source_file is present", async () => {
      const { client, ws } = makeConnectedClient();
      provider.setClient(client);
      await respondToInitialFetch(ws, [
        {
          name: "RestartStrategy",
          expansion: "#temporary | #transient | #permanent",
          source_file: "restart_strategy.bt",
          package: "beamtalk_stdlib",
          source_origin: "stdlib",
        },
      ]);

      const [aliasNode] = await provider.getChildren({ kind: "type-aliases-section" });
      const item = provider.getTreeItem(aliasNode);

      expect(item.contextValue).toBe("type-alias-item-no-source");
      expect(item.command).toBeUndefined();
      client.dispose();
    });

    it("is NOT navigable for a dependency-origin alias, even though source_file is present", async () => {
      const { client, ws } = makeConnectedClient();
      provider.setClient(client);
      await respondToInitialFetch(ws, [
        {
          name: "JsonValue",
          expansion: "String | Integer | Float | Boolean | Nil",
          source_file: "src/json_value.bt",
          package: "beamtalk_json",
          source_origin: "dependency",
        },
      ]);

      const [aliasNode] = await provider.getChildren({ kind: "type-aliases-section" });
      const item = provider.getTreeItem(aliasNode);

      expect(item.contextValue).toBe("type-alias-item-no-source");
      expect(item.command).toBeUndefined();
      client.dispose();
    });

    it("falls back to the source_file-presence check when source_origin is absent (older server)", async () => {
      const { client, ws } = makeConnectedClient();
      provider.setClient(client);
      await respondToInitialFetch(ws, [
        { name: "Timeout", expansion: "Integer | #infinity", source_file: "src/timeout.bt" },
      ]);

      const [aliasNode] = await provider.getChildren({ kind: "type-aliases-section" });
      const item = provider.getTreeItem(aliasNode);

      expect(item.contextValue).toBe("type-alias-item");
      client.dispose();
    });
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

// ─── Classes section — stdlib/project/dependency filter ───────────────────────

/** Like `respondToInitialFetch`, but with a caller-supplied `list-classes` payload. */
async function respondToInitialFetchWithClasses(
  ws: MockWebSocket,
  classList: unknown[]
): Promise<void> {
  respondToOp(ws, "eval", { value: [] });
  respondToOp(ws, "actors", {});
  respondToOp(ws, "list-classes", { class_list: classList });
  respondToOp(ws, "browse-type-aliases", { value: [] });
  for (let i = 0; i < 5; i++) {
    await Promise.resolve();
  }
}

describe("WorkspaceTreeDataProvider — Classes section origin filter", () => {
  let provider: InstanceType<typeof WorkspaceTreeDataProvider>;

  const THREE_ORIGIN_CLASSES = [
    { name: "MyApp", source_file: "/proj/MyApp.bt", actor_count: 0, source_origin: "project" },
    {
      name: "Json",
      source_file: "/deps/json/Json.bt",
      actor_count: 0,
      source_origin: "dependency",
    },
    { name: "Array", source_file: null, actor_count: 0, source_origin: "stdlib" },
  ];

  beforeEach(() => {
    provider = new WorkspaceTreeDataProvider();
  });

  it("defaults to showing every origin (no filtering)", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, THREE_ORIGIN_CLASSES);

    expect([...provider.classFilter].sort()).toEqual(["dependency", "project", "stdlib"]);

    const children = await provider.getChildren({ kind: "classes-section" });
    expect(children).toHaveLength(3);

    const sectionItem = provider.getTreeItem({ kind: "classes-section" });
    expect(sectionItem.description).toBe("(3 loaded)");
    expect(sectionItem.contextValue).toBe("classes-section");

    client.dispose();
  });

  it("narrows the Classes section to the selected origins", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, THREE_ORIGIN_CLASSES);

    provider.setClassOriginFilter(new Set(["project"]));

    const children = await provider.getChildren({ kind: "classes-section" });
    expect(children.map((c) => (c as { info: { name: string } }).info.name)).toEqual(["MyApp"]);

    const sectionItem = provider.getTreeItem({ kind: "classes-section" });
    expect(sectionItem.description).toBe("(1 of 3)");
    expect(sectionItem.contextValue).toBe("classes-section-filtered");

    client.dispose();
  });

  it("fires a classes-section change when the filter changes", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, THREE_ORIGIN_CLASSES);

    const fired: unknown[] = [];
    provider.onDidChangeTreeData((node) => fired.push(node));

    provider.setClassOriginFilter(new Set(["stdlib"]));

    expect(fired).toEqual([{ kind: "classes-section" }]);

    client.dispose();
  });

  it("always shows a class with no source_origin, regardless of the filter", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      ...THREE_ORIGIN_CLASSES,
      { name: "Legacy", source_file: "/proj/Legacy.bt", actor_count: 0, source_origin: null },
    ]);

    provider.setClassOriginFilter(new Set(["stdlib"]));

    const children = await provider.getChildren({ kind: "classes-section" });
    const names = children.map((c) => (c as { info: { name: string } }).info.name);
    expect(names).toEqual(expect.arrayContaining(["Array", "Legacy"]));
    expect(names).not.toContain("MyApp");
    expect(names).not.toContain("Json");

    client.dispose();
  });

  it("treats an empty selection as 'show everything' rather than an empty tree", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, THREE_ORIGIN_CLASSES);

    provider.setClassOriginFilter(new Set(["project"]));
    expect(await provider.getChildren({ kind: "classes-section" })).toHaveLength(1);

    provider.setClassOriginFilter(new Set());
    expect(await provider.getChildren({ kind: "classes-section" })).toHaveLength(3);
    expect([...provider.classFilter].sort()).toEqual(["dependency", "project", "stdlib"]);

    client.dispose();
  });
});

// ─── Stale-write races, "classes/removed", and refresh-signal hygiene ─────────

function methodGroup(
  children: Awaited<ReturnType<WorkspaceTreeDataProvider["getChildren"]>>,
  side: "instance" | "class"
): MethodGroupNode["methods"] {
  const group = children.find((c) => c.kind === "method-group" && c.side === side) as
    | MethodGroupNode
    | undefined;
  if (!group) throw new Error(`no ${side} method-group in ${JSON.stringify(children)}`);
  return group.methods;
}

describe("WorkspaceTreeDataProvider — stale-write races (methods/inspect caches)", () => {
  let provider: InstanceType<typeof WorkspaceTreeDataProvider>;

  beforeEach(() => {
    provider = new WorkspaceTreeDataProvider();
  });

  it("discards a class-item methods() fetch that resolves after a classes/loaded push invalidated it", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
    ]);

    const [classItem] = await provider.getChildren({ kind: "classes-section" });

    // Start the first (soon-to-be-stale) fetch — don't await yet.
    const firstFetch = provider.getChildren(classItem);
    await Promise.resolve(); // let the `methods` request actually get sent

    // A reload arrives while that fetch is still in flight: invalidates the
    // cache for "Foo" and kicks off its own re-fetch of the class list.
    ws.receive({
      type: "push",
      channel: "classes",
      event: "loaded",
      data: { class: "Foo" },
    });
    respondToOp(ws, "list-classes", {
      class_list: [{ name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 }],
    });

    // The original request now resolves — with data that is stale relative
    // to the invalidation that already happened.
    respondToOp(ws, "methods", {
      methods: [{ name: "stale", selector: "stale", side: "instance" }],
      state_vars: [],
    });

    // The stale result must be discarded, not written into the cache.
    expect(await firstFetch).toEqual([]);

    // A fresh fetch (cache is still empty) gets the real data.
    const secondFetch = provider.getChildren(classItem);
    respondToOp(ws, "methods", {
      methods: [{ name: "fresh", selector: "fresh", side: "instance" }],
      state_vars: [],
    });
    const fresh = await secondFetch;
    expect(methodGroup(fresh, "instance").map((m) => m.method.selector)).toEqual(["fresh"]);

    client.dispose();
  });

  it("discards an actor-item inspect() fetch that resolves after a stopped/spawned push invalidated it", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws);

    // Introduce the actor via a "spawned" push (mirrors real usage — actors
    // aren't part of the initial fetch fixture here).
    ws.receive({
      type: "push",
      channel: "actors",
      event: "spawned",
      data: { pid: "<0.1.0>", class: "Foo" },
    });
    const [actorItem] = await provider.getChildren({ kind: "actors-section" });

    const firstFetch = provider.getChildren(actorItem);
    await Promise.resolve();

    // The actor stops (e.g. crashed) while inspect() is still in flight.
    ws.receive({
      type: "push",
      channel: "actors",
      event: "stopped",
      data: { pid: "<0.1.0>", class: "Foo", reason: "normal" },
    });

    respondToOp(ws, "inspect", { state: { stale: 1 } });
    expect(await firstFetch).toEqual([]);

    client.dispose();
  });

  it("a wholesale refresh() discards any per-item fetch already in flight", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
    ]);

    const [classItem] = await provider.getChildren({ kind: "classes-section" });
    const firstFetch = provider.getChildren(classItem);
    await Promise.resolve();

    const refreshDone = provider.refresh();
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
    ]);
    await refreshDone;

    respondToOp(ws, "methods", {
      methods: [{ name: "stale", selector: "stale", side: "instance" }],
      state_vars: [],
    });
    expect(await firstFetch).toEqual([]);

    client.dispose();
  });
});

describe("WorkspaceTreeDataProvider — classes/removed push (BT-2531 wiring)", () => {
  let provider: InstanceType<typeof WorkspaceTreeDataProvider>;

  beforeEach(() => {
    provider = new WorkspaceTreeDataProvider();
  });

  it("removes the class from the tree immediately, no re-fetch needed", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
      { name: "Bar", source_file: "/proj/Bar.bt", actor_count: 0 },
    ]);

    expect(await provider.getChildren({ kind: "classes-section" })).toHaveLength(2);

    const sentBefore = ws.sent.length;
    ws.receive({
      type: "push",
      channel: "classes",
      event: "removed",
      data: { class: "Foo" },
    });

    const children = await provider.getChildren({ kind: "classes-section" });
    expect(children.map((c) => (c as { info: { name: string } }).info.name)).toEqual(["Bar"]);
    // No re-fetch: the event alone is enough to update the list.
    expect(ws.sent.length).toBe(sentBefore);

    client.dispose();
  });

  it("fires a classes-section change", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
    ]);

    const fired: unknown[] = [];
    provider.onDidChangeTreeData((node) => fired.push(node));

    ws.receive({
      type: "push",
      channel: "classes",
      event: "removed",
      data: { class: "Foo" },
    });

    expect(fired).toEqual([{ kind: "classes-section" }]);

    client.dispose();
  });

  it("clears the removed class's cached methods so a stale entry can't resurface", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
    ]);

    const [classItem] = await provider.getChildren({ kind: "classes-section" });
    const fetch1 = provider.getChildren(classItem);
    respondToOp(ws, "methods", {
      methods: [{ name: "m", selector: "m", side: "instance" }],
      state_vars: [],
    });
    await fetch1;

    ws.receive({
      type: "push",
      channel: "classes",
      event: "removed",
      data: { class: "Foo" },
    });

    // If the cache weren't cleared, this would resolve instantly from the
    // stale cache instead of sending a new request.
    const sentBefore = ws.sent.length;
    const fetch2 = provider.getChildren(classItem);
    expect(ws.sent.length).toBeGreaterThan(sentBefore);
    respondToOp(ws, "methods", { methods: [], state_vars: [] });
    await fetch2;

    client.dispose();
  });
});

describe("WorkspaceTreeDataProvider — refresh-signal hygiene", () => {
  let provider: InstanceType<typeof WorkspaceTreeDataProvider>;

  beforeEach(() => {
    provider = new WorkspaceTreeDataProvider();
  });

  it("an unexpected disconnect (reconnecting) refreshes only the root node, not the whole tree", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws);

    const fired: unknown[] = [];
    provider.onDidChangeTreeData((node) => fired.push(node));

    // Simulate an unexpected drop (not client.dispose()) — the client
    // schedules a reconnect and reports "reconnecting".
    ws.close();

    expect(fired).toEqual([{ kind: "connected-root" }]);

    client.dispose();
  });

  it("does not double-fire a full reset when setClient(null) follows a dispose() that already reset", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws);

    const fired: unknown[] = [];
    provider.onDidChangeTreeData((node) => fired.push(node));

    // Mirrors connectWorkspace/disconnectWorkspace: dispose() first (which
    // synchronously drives this client's still-registered onConnectionChange
    // to "disconnected", already resetting), then setClient(null).
    client.dispose();
    provider.setClient(null);

    expect(fired.filter((n) => n === undefined)).toHaveLength(1);
  });

  it("still fully resets when setClient(null) is called without a prior dispose()", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetch(ws);

    const fired: unknown[] = [];
    provider.onDidChangeTreeData((node) => fired.push(node));

    provider.setClient(null);

    expect(fired.filter((n) => n === undefined)).toHaveLength(1);
    client.dispose();
  });
});

// ─── Inherited methods (BT-3478) ────────────────────────────────────────────

function inheritedGroup(
  children: Awaited<ReturnType<WorkspaceTreeDataProvider["getChildren"]>>,
  side: "instance" | "class"
): InheritedMethodGroupNode {
  const group = children.find((c) => c.kind === "inherited-method-group" && c.side === side) as
    | InheritedMethodGroupNode
    | undefined;
  if (!group) throw new Error(`no ${side} inherited-method-group in ${JSON.stringify(children)}`);
  return group;
}

describe("WorkspaceTreeDataProvider — inherited methods (BT-3478)", () => {
  let provider: InstanceType<typeof WorkspaceTreeDataProvider>;

  beforeEach(() => {
    provider = new WorkspaceTreeDataProvider();
  });

  it("a class item's children include two flat, collapsed 'Inherited' groups without fetching them eagerly", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
    ]);
    const [classItem] = await provider.getChildren({ kind: "classes-section" });

    const fetch = provider.getChildren(classItem);
    respondToOp(ws, "methods", { methods: [], state_vars: [] });
    const children = await fetch;

    expect(children.map((c) => c.kind)).toEqual([
      "state-group",
      "method-group",
      "method-group",
      "inherited-method-group",
      "inherited-method-group",
    ]);
    // Same tree depth as the local groups — no nested per-superclass level,
    // and never auto-expanded (the count isn't known until fetched).
    const instanceGroupItem = provider.getTreeItem(inheritedGroup(children, "instance"));
    expect(instanceGroupItem.label).toBe("Inherited Instance Methods");
    expect(instanceGroupItem.collapsibleState).toBe(1); // vscode.TreeItemCollapsibleState.Collapsed
    // The eager class-item fetch above only ever requested "methods" — the
    // whole point of the separate op.
    expect(ws.sent.some((m) => m.op === "inherited-methods")).toBe(false);

    client.dispose();
  });

  it("expanding one side's group lazily fetches both sides once, split and attributed by defining class", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
      { name: "Actor", source_file: "/stdlib/Actor.bt", actor_count: 0 },
    ]);
    const [classItem] = await provider.getChildren({ kind: "classes-section" });
    const fetch = provider.getChildren(classItem);
    respondToOp(ws, "methods", { methods: [], state_vars: [] });
    const children = await fetch;

    const instanceFetch = provider.getChildren(inheritedGroup(children, "instance"));
    respondToOp(ws, "inherited-methods", {
      methods: [
        {
          name: "spawn",
          selector: "spawn",
          side: "instance",
          defining_class: "Actor",
        },
        {
          name: "supervisionSpec",
          selector: "supervisionSpec",
          side: "class",
          defining_class: "Actor",
        },
      ],
    });
    const instanceItems = (await instanceFetch) as MethodItemNode[];

    expect(instanceItems.map((n) => n.method.selector)).toEqual(["spawn"]);
    expect(instanceItems[0].definingClass).toBe("Actor");
    // Defining class's own loaded ClassInfo is used (real source_file), not
    // the receiving class Foo's — so "Go to Definition" opens Actor.bt.
    expect(instanceItems[0].classInfo).toEqual(
      expect.objectContaining({ name: "Actor", source_file: "/stdlib/Actor.bt" })
    );

    // The class-side group reuses the same cached fetch — no second request.
    const classItems = (await provider.getChildren(
      inheritedGroup(children, "class")
    )) as MethodItemNode[];
    expect(classItems.map((n) => n.method.selector)).toEqual(["supervisionSpec"]);
    expect(ws.sent.filter((m) => m.op === "inherited-methods")).toHaveLength(1);

    client.dispose();
  });

  it("falls back to a source-less stub ClassInfo when the defining class isn't loaded", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
    ]);
    const [classItem] = await provider.getChildren({ kind: "classes-section" });
    const fetch = provider.getChildren(classItem);
    respondToOp(ws, "methods", { methods: [], state_vars: [] });
    const children = await fetch;

    const instanceFetch = provider.getChildren(inheritedGroup(children, "instance"));
    respondToOp(ws, "inherited-methods", {
      methods: [
        { name: "class", selector: "class", side: "instance", defining_class: "ProtoObject" },
      ],
    });
    const [item] = (await instanceFetch) as MethodItemNode[];

    expect(item.classInfo).toEqual({ name: "ProtoObject" });
    const treeItem = provider.getTreeItem(item);
    expect(treeItem.contextValue).toBe("method-item-no-source");

    client.dispose();
  });

  it("wires up Go to Definition for a stdlib method with no source_file, direct or inherited", async () => {
    // The runtime never reports a real `source_file` for compiled-in stdlib
    // classes, but `beamtalk.navigateToMethod` falls back to the LSP's
    // `beamtalk-stdlib://` virtual URI scheme whenever `source_origin` is
    // "stdlib" — so a row for a stdlib method must stay clickable even
    // without a `source_file`, both for a method declared directly on the
    // browsed class and for one inherited from a stdlib superclass.
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Array", source_file: null, actor_count: 0, source_origin: "stdlib" },
      { name: "Object", source_file: null, actor_count: 0, source_origin: "stdlib" },
    ]);
    const [classItem] = await provider.getChildren({ kind: "classes-section" });
    const fetch = provider.getChildren(classItem);
    respondToOp(ws, "methods", {
      methods: [{ name: "size", selector: "size", side: "instance" }],
      state_vars: [],
    });
    const children = await fetch;

    const methodGroup = children.find(
      (c) => c.kind === "method-group" && c.side === "instance"
    ) as MethodGroupNode;
    const [directItem] = methodGroup.methods;
    const directTreeItem = provider.getTreeItem(directItem);
    expect(directTreeItem.contextValue).toBe("method-item");
    expect(directTreeItem.command).toEqual(
      expect.objectContaining({ command: "beamtalk.navigateToMethod" })
    );

    const instanceFetch = provider.getChildren(inheritedGroup(children, "instance"));
    respondToOp(ws, "inherited-methods", {
      methods: [{ name: "==", selector: "==", side: "instance", defining_class: "Object" }],
    });
    const [inheritedItem] = (await instanceFetch) as MethodItemNode[];
    const inheritedTreeItem = provider.getTreeItem(inheritedItem);
    expect(inheritedTreeItem.contextValue).toBe("method-item");
    expect(inheritedTreeItem.command).toEqual(
      expect.objectContaining({ command: "beamtalk.navigateToMethod" })
    );

    client.dispose();
  });

  it("discards an inherited-methods fetch that resolves after a classes/loaded push invalidated it", async () => {
    const { client, ws } = makeConnectedClient();
    provider.setClient(client);
    await respondToInitialFetchWithClasses(ws, [
      { name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 },
    ]);
    const [classItem] = await provider.getChildren({ kind: "classes-section" });
    const classFetch = provider.getChildren(classItem);
    respondToOp(ws, "methods", { methods: [], state_vars: [] });
    const children = await classFetch;

    const firstFetch = provider.getChildren(inheritedGroup(children, "instance"));
    await Promise.resolve();

    ws.receive({
      type: "push",
      channel: "classes",
      event: "loaded",
      data: { class: "Foo" },
    });
    respondToOp(ws, "list-classes", {
      class_list: [{ name: "Foo", source_file: "/proj/Foo.bt", actor_count: 0 }],
    });

    respondToOp(ws, "inherited-methods", {
      methods: [{ name: "stale", selector: "stale", side: "instance", defining_class: "Actor" }],
    });
    expect(await firstFetch).toEqual([]);

    client.dispose();
  });
});

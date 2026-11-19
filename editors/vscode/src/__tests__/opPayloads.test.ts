// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/**
 * The TypeScript half of the per-op wire payload fixture (BT-5049).
 *
 * `runtime/apps/beamtalk_workspace/test/fixtures/repl_op_payloads.json` holds
 * one example reply per REPL op. The Erlang encoders are checked against it in
 * `beamtalk_repl_op_table_tests`, the Rust client types in
 * `crates/beamtalk-repl-protocol/tests/op_payloads.rs`; this file feeds the same
 * replies to `WorkspaceClient` and checks what the extension's types make of
 * them. A server-side shape change that `WorkspaceClient` would misread fails
 * here.
 *
 * Run it with `npm test -- opPayloads` from `editors/vscode`.
 */

import { readFileSync } from "node:fs";
import * as path from "node:path";
import { describe, expect, it } from "vitest";
import type { WebSocketCallbacks, WebSocketFactory } from "../workspaceClient";
import { WorkspaceClient } from "../workspaceClient";

// ─── Fixture ──────────────────────────────────────────────────────────────────

interface Fixture {
  payloads: Record<string, { op: string; response: Record<string, unknown> }>;
  unpinned: Record<string, string>;
}

const FIXTURE: Fixture = JSON.parse(
  readFileSync(
    path.resolve(
      __dirname,
      "../../../../runtime/apps/beamtalk_workspace/test/fixtures/repl_op_payloads.json"
    ),
    "utf8"
  )
);

const OP_TABLE: Array<{ op: string; params: string[]; optional: string[] }> = JSON.parse(
  readFileSync(
    path.resolve(
      __dirname,
      "../../../../runtime/apps/beamtalk_workspace/test/fixtures/repl_op_table.json"
    ),
    "utf8"
  )
);

function payload(id: string): Record<string, unknown> {
  const entry = FIXTURE.payloads[id];
  if (!entry) {
    throw new Error(`no payload ${id} in repl_op_payloads.json`);
  }
  return entry.response;
}

// ─── Mock server ──────────────────────────────────────────────────────────────

class MockWebSocket {
  readyState = 1;
  sent: Array<Record<string, unknown>> = [];

  constructor(private callbacks: WebSocketCallbacks) {}

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

function connectedClient(): { client: WorkspaceClient; ws: MockWebSocket } {
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

/**
 * Run `call` against a connected client whose server answers the request it
 * sends with fixture payload `id` (its `id` swapped for the request's, as the
 * real server echoes it). Returns the request the client sent and the result.
 */
async function roundTrip<T>(
  id: string,
  call: (client: WorkspaceClient) => Promise<T>
): Promise<{ request: Record<string, unknown>; result: T }> {
  const { client, ws } = connectedClient();
  const pending = call(client);
  const request = ws.sent[ws.sent.length - 1];
  ws.receive({ ...payload(id), id: request.id });
  const result = await pending;
  client.dispose();
  return { request, result };
}

// ─── The fixture itself ───────────────────────────────────────────────────────

describe("repl_op_payloads.json", () => {
  it("pins or explains every op in the op table, never both", () => {
    const pinned = new Set(Object.values(FIXTURE.payloads).map((p) => p.op));
    const explained = new Set(Object.keys(FIXTURE.unpinned));
    const tableOps = OP_TABLE.map((e) => e.op).sort();
    expect([...pinned].filter((op) => explained.has(op))).toEqual([]);
    expect([...pinned, ...explained].sort()).toEqual(tableOps);
  });

  it("gives every payload the envelope and a done status", () => {
    for (const [id, { response }] of Object.entries(FIXTURE.payloads)) {
      expect(response.id, id).toBe("msg-1");
      expect(response.session, id).toBe("sess-1");
      expect(response.status as string[], id).toContain("done");
    }
  });
});

// ─── What WorkspaceClient makes of each reply ────────────────────────────────

describe("WorkspaceClient against the op payload fixture", () => {
  it("sends only ops and params the table declares", async () => {
    // The Rust scan `editor_clients_op_table.rs` checks this statically; the
    // requests below are the ones this file's replies answer.
    const calls: Array<[string, (c: WorkspaceClient) => Promise<unknown>]> = [
      ["list-classes", (c) => c.classes()],
      ["actors", (c) => c.actors()],
      ["methods", (c) => c.methods("Counter")],
      ["inherited-methods", (c) => c.inheritedMethods("Counter")],
      ["sessions", (c) => c.sessions()],
      ["browse-type-aliases", (c) => c.typeAliases()],
      ["browse-alias-source", (c) => c.browseAliasSource("Count", "proj")],
      ["inspect", (c) => c.inspect("<0.123.0>")],
    ];
    for (const [id, call] of calls) {
      const { request } = await roundTrip(id, call);
      const row = OP_TABLE.find((e) => e.op === request.op);
      expect(row, `op ${String(request.op)} is in the op table`).toBeDefined();
      const allowed = new Set([
        "op",
        "id",
        "session",
        ...(row?.params ?? []),
        ...(row?.optional ?? []),
      ]);
      for (const key of Object.keys(request)) {
        expect(allowed.has(key), `${String(request.op)} sends undeclared param ${key}`).toBe(true);
      }
      for (const required of row?.params ?? []) {
        expect(required in request, `${String(request.op)} sends ${required}`).toBe(true);
      }
    }
  });

  it("list-classes: ClassInfo carries source_file, actor_count and source_origin", async () => {
    const { result } = await roundTrip("list-classes", (c) => c.classes());
    expect(result).toEqual([
      {
        name: "Counter",
        source_file: "/work/proj/src/counter.bt",
        actor_count: 2,
        source_origin: "project",
      },
      // `null` source_file on the wire reads as undefined.
      { name: "Object", source_file: undefined, actor_count: 0, source_origin: "stdlib" },
    ]);
  });

  it("actors: ActorInfo covers a user actor and a system actor with a null start time", async () => {
    const { result } = await roundTrip("actors", (c) => c.actors());
    expect(result).toHaveLength(2);
    expect(result[0]).toMatchObject({
      pid: "<0.123.0>",
      class: "Counter",
      module: "bt@counter",
      spawned_at: 1234567890,
    });
    expect(result[0].system).toBeUndefined();
    expect(result[1]).toMatchObject({
      pid: "<0.5.0>",
      class: "TranscriptStream",
      spawned_at: null,
      system: true,
    });
  });

  it("methods: MethodInfo and StateVarInfo normalise null to undefined", async () => {
    const { result } = await roundTrip("methods", (c) => c.methods("Counter"));
    expect(result.methods).toHaveLength(2);
    expect(result.methods[0]).toMatchObject({
      name: "increment",
      selector: "increment",
      side: "instance",
      line: 12,
      source_status: "indexed",
    });
    expect(result.methods[0].signature).toBeUndefined();
    expect(result.methods[0].doc).toBeUndefined();
    expect(result.methods[1]).toMatchObject({
      side: "class",
      source_status: "synthetic",
      signature: "new -> Counter",
      doc: "Make one.",
    });
    expect(result.methods[1].line).toBeUndefined();
    expect(result.stateVars).toEqual([
      { name: "count", line: 5 },
      { name: "step", line: undefined },
    ]);
  });

  it("inherited-methods: each row names its defining class", async () => {
    const { result } = await roundTrip("inherited-methods", (c) => c.inheritedMethods("Counter"));
    expect(result).toHaveLength(1);
    expect(result[0]).toMatchObject({
      name: "printString",
      side: "instance",
      definingClass: "Object",
    });
  });

  it("sessions: the id is a session id, not a pid string", async () => {
    const { result } = await roundTrip("sessions", (c) => c.sessions());
    expect(result).toEqual([{ id: "sess-1" }]);
  });

  it("inspect: state is the map of printed slot values", async () => {
    const { result } = await roundTrip("inspect", (c) => c.inspect("<0.123.0>"));
    expect(result).toEqual({ count: 3, name: '"c"' });
  });

  it("browse-type-aliases: AliasRow reads into TypeAliasInfo", async () => {
    const { result } = await roundTrip("browse-type-aliases", (c) => c.typeAliases());
    expect(result).toEqual([
      {
        name: "Count",
        expansion: "Integer",
        doc: undefined,
        source_file: "src/types.bt",
        internal: false,
        package: "proj",
        source_origin: "project",
      },
    ]);
  });

  it("browse-alias-source: content is the source text", async () => {
    const { result } = await roundTrip("browse-alias-source", (c) =>
      c.browseAliasSource("Count", "proj")
    );
    expect(result).toEqual({ content: "type Count = Integer\n" });
  });

  it("close and subscribe-logs replies resolve (value: ok)", async () => {
    // `subscribe-logs` is answered with the same `encode_status` reply `close` pins.
    const { request } = await roundTrip("close", (c) => c.subscribeLogs("info"));
    expect(request.op).toBe("subscribe-logs");
    expect(request.level).toBe("info");
  });

  it("an error reply rejects with the server's message", async () => {
    for (const id of ["eval/error", "get-traces/error"]) {
      const { client, ws } = connectedClient();
      const pending = client.inspect("<0.1.0>");
      const request = ws.sent[ws.sent.length - 1];
      ws.receive({ ...payload(id), id: request.id });
      await expect(pending).rejects.toThrow(String(payload(id).error));
      client.dispose();
    }
  });

  it("eval warnings reach reload() as plain strings", async () => {
    const { result } = await roundTrip("eval/output_and_warnings", (c) => c.reload("Counter"));
    expect(result.warnings).toEqual(["Unused variable: x"]);
  });
});

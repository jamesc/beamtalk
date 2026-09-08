// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { describe, expect, it, vi } from "vitest";
import { buildVscodeModule } from "./vscodeMock";

const { executeCommandMock, openTextDocumentMock } = vi.hoisted(() => ({
  executeCommandMock: vi.fn(),
  openTextDocumentMock: vi.fn(),
}));

vi.mock("vscode", () => buildVscodeModule({ executeCommandMock, openTextDocumentMock }));

import { resolveDeclarationOffsetSync } from "../symbolLookup";

// Reproduces task_queue_registry.bt's `state: queues :: Dictionary(String, TaskQueue)`.
const DEFAULTLESS_TYPED_BT = `\
typed Actor subclass: TaskQueueRegistry
  /// Internal mapping of queue names to TaskQueue actors.
  state: queues :: Dictionary(String, TaskQueue)
`;

// Reproduces event_store.bt's saveSnapshot:state:eventId: with :: typed params.
const EVENT_STORE_BT = `\
Actor subclass: EventStore
  /// store saveSnapshot: "wf-1" state: #{#replayCursor => 50} eventId: 51
  saveSnapshot: workflowId :: String state: state :: ReplaySnapshot eventId: eventId :: Integer -> Nil =>
    42
`;

describe("resolveDeclarationOffsetSync — unified DeclarationRef resolver", () => {
  it("resolves a method with :: typed params, not the doc-comment mention", () => {
    const offset = resolveDeclarationOffsetSync(EVENT_STORE_BT, {
      kind: "method",
      side: "instance",
      selector: "saveSnapshot:state:eventId:",
    });
    expect(offset).not.toBe(-1);
    const line = EVENT_STORE_BT.slice(0, offset).split("\n").length;
    expect(line).toBe(3);
  });

  it("resolves a defaultless typed field", () => {
    const offset = resolveDeclarationOffsetSync(DEFAULTLESS_TYPED_BT, {
      kind: "field",
      name: "queues",
    });
    expect(offset).not.toBe(-1);
    expect(DEFAULTLESS_TYPED_BT.slice(offset, offset + 6)).toBe("queues");
  });

  it("resolves a class declaration, skipping a doc-comment mention", () => {
    const src = [
      "/// registry := TaskQueueRegistry spawn",
      "Actor subclass: TaskQueueRegistry",
    ].join("\n");
    const offset = resolveDeclarationOffsetSync(src, { kind: "class", name: "TaskQueueRegistry" });
    expect(offset).not.toBe(-1);
    const line = src.slice(0, offset).split("\n").length;
    expect(line).toBe(2);
  });

  it("resolves a type alias declaration (ADR 0108 Phase 8, BT-2903)", () => {
    const src = [
      "/// See the Timeout alias below for retry/backoff options.",
      "type Timeout = Integer | #infinity",
    ].join("\n");
    const offset = resolveDeclarationOffsetSync(src, { kind: "type-alias", name: "Timeout" });
    expect(offset).not.toBe(-1);
    const line = src.slice(0, offset).split("\n").length;
    expect(line).toBe(2);
  });

  it("prefers a valid declared line over the regex tier", () => {
    const src = "Object subclass: Foo\n  state: count = 0\n  state: count = 1\n";
    // Line 3 (1-based) is the real one here even though line 2 also matches "count".
    const offset = resolveDeclarationOffsetSync(src, { kind: "field", name: "count" }, 3);
    const line = src.slice(0, offset).split("\n").length;
    expect(line).toBe(3);
  });

  it("falls back to the regex tier when the declared line is stale", () => {
    const src = "Object subclass: Foo\n// shifted\n  state: count = 0\n";
    // Declared line 2 used to be the declaration before a line was inserted above it.
    const offset = resolveDeclarationOffsetSync(src, { kind: "field", name: "count" }, 2);
    expect(offset).not.toBe(-1);
    const line = src.slice(0, offset).split("\n").length;
    expect(line).toBe(3);
  });

  it("returns -1 when nothing matches, for every kind", () => {
    expect(
      resolveDeclarationOffsetSync("Object subclass: Foo", {
        kind: "method",
        side: "instance",
        selector: "nonexistent:",
      })
    ).toBe(-1);
    expect(
      resolveDeclarationOffsetSync("Object subclass: Foo", { kind: "field", name: "nonexistent" })
    ).toBe(-1);
    expect(
      resolveDeclarationOffsetSync("Object subclass: Foo", { kind: "class", name: "Nonexistent" })
    ).toBe(-1);
    expect(
      resolveDeclarationOffsetSync("type Timeout = Integer", {
        kind: "type-alias",
        name: "Nonexistent",
      })
    ).toBe(-1);
  });
});

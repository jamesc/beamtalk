// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { describe, expect, it, vi } from "vitest";

vi.mock("vscode", () => ({}));

import { scanSessionChunk } from "../repl";

describe("scanSessionChunk (BT-5130)", () => {
  it("accepts an id followed by a newline", () => {
    expect(scanSessionChunk("", "[beamtalk] session: abc-123\n").sessionId).toBe("abc-123");
  });

  it("does not accept a prefix id when the marker is split across chunks", () => {
    const first = scanSessionChunk("", "[beamtalk] session: abc");
    expect(first.sessionId).toBeUndefined();
    const second = scanSessionChunk(first.tail, "-123\n");
    expect(second.sessionId).toBe("abc-123");
  });

  it("does not accept an id at the end of the buffer", () => {
    expect(scanSessionChunk("", "[beamtalk] session: abc").sessionId).toBeUndefined();
  });
});

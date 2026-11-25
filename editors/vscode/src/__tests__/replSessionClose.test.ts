// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { beforeEach, describe, expect, it, vi } from "vitest";

vi.mock("vscode", () => ({ commands: { executeCommand: vi.fn() } }));

import { state } from "../extensionState";
import { captureSessionId, closeReplSession } from "../repl";

describe("closeReplSession (BT-5199)", () => {
  beforeEach(() => {
    state.replSessionId = undefined;
  });

  it("closes the REPL's session on the node once, then forgets it", async () => {
    state.replSessionId = "sess-repl";
    const client = { state: "connected", closeSession: vi.fn().mockResolvedValue(undefined) };

    closeReplSession(client, vi.fn());
    closeReplSession(client, vi.fn());

    expect(client.closeSession).toHaveBeenCalledTimes(1);
    expect(client.closeSession).toHaveBeenCalledWith("sess-repl");
    expect(state.replSessionId).toBeUndefined();
  });

  it("sends nothing without a recorded session or a connected client", () => {
    const client = { state: "reconnecting", closeSession: vi.fn() };
    closeReplSession(client, vi.fn());
    state.replSessionId = "sess-repl";
    closeReplSession(client, vi.fn());
    closeReplSession(undefined, vi.fn());
    expect(client.closeSession).not.toHaveBeenCalled();
  });

  it("warns when the close fails", async () => {
    state.replSessionId = "sess-repl";
    const warn = vi.fn();
    const client = {
      state: "connected",
      closeSession: vi.fn().mockRejectedValue(new Error("Not connected to workspace")),
    };
    closeReplSession(client, warn);
    await Promise.resolve();
    await Promise.resolve();
    expect(warn).toHaveBeenCalledWith(expect.stringContaining("sess-repl"));
  });
});

describe("captureSessionId closes the session only when the REPL exited (BT-5199)", () => {
  type Execution = Parameters<typeof captureSessionId>[0];
  type Terminal = Parameters<typeof captureSessionId>[1];

  function execution(fail: boolean): Execution {
    return {
      read: async function* () {
        yield "[beamtalk] session: sess-live\n";
        if (fail) throw new Error("shell integration disconnected");
      },
    } as unknown as Execution;
  }

  function setup() {
    const terminal = {} as Terminal;
    const client = { state: "connected", closeSession: vi.fn().mockResolvedValue(undefined) };
    state.replTerminal = terminal;
    state.workspaceWsClient = client as unknown as typeof state.workspaceWsClient;
    return { terminal, client };
  }

  it("closes the REPL's session when its execution ran to the end", async () => {
    const { terminal, client } = setup();
    await captureSessionId(execution(false), terminal);
    expect(client.closeSession).toHaveBeenCalledWith("sess-live");
  });

  it("leaves a possibly running REPL alone when reading its output fails", async () => {
    const { terminal, client } = setup();
    await captureSessionId(execution(true), terminal);
    expect(client.closeSession).not.toHaveBeenCalled();
  });
});

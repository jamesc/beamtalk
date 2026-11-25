// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { describe, expect, it, vi } from "vitest";
import { applyLogLevel, type LogLevelWorkspace, registerLogLevelSync } from "../logLevelSync";

function fakeWorkspace(initial: string) {
  let level = initial;
  let listener: ((e: { affectsConfiguration(s: string): boolean }) => void) | undefined;
  const workspace: LogLevelWorkspace = {
    onDidChangeConfiguration: (l) => {
      listener = l;
      return { dispose: () => {} };
    },
    getConfiguration: () => ({ get: <T>() => level as unknown as T }),
  };
  return {
    workspace,
    change(next: string, section = "beamtalk.logs.level") {
      level = next;
      listener?.({ affectsConfiguration: (s) => s === section });
    },
  };
}

describe("registerLogLevelSync (BT-5082)", () => {
  it("re-subscribes at the new level when the setting changes in the Settings UI", () => {
    const ws = fakeWorkspace("warning");
    const subscribeLogs = vi.fn().mockResolvedValue(undefined);
    const bar = vi.fn();
    registerLogLevelSync(
      ws.workspace,
      () => ({ subscribeLogs, unsubscribeLogs: vi.fn() }),
      bar,
      vi.fn()
    );

    ws.change("debug");

    expect(subscribeLogs).toHaveBeenCalledWith("debug");
    expect(bar).toHaveBeenCalledWith("debug");
  });

  it("ignores unrelated settings and a missing client", () => {
    const ws = fakeWorkspace("warning");
    const subscribeLogs = vi.fn().mockResolvedValue(undefined);
    let client:
      | { subscribeLogs: typeof subscribeLogs; unsubscribeLogs: typeof subscribeLogs }
      | undefined = {
      subscribeLogs,
      unsubscribeLogs: vi.fn(),
    };
    registerLogLevelSync(ws.workspace, () => client, vi.fn(), vi.fn());

    ws.change("info", "beamtalk.other");
    expect(subscribeLogs).not.toHaveBeenCalled();

    client = undefined;
    expect(() => ws.change("info")).not.toThrow();
  });

  it("warns instead of throwing when the subscription fails", async () => {
    const ws = fakeWorkspace("warning");
    const warn = vi.fn();
    registerLogLevelSync(
      ws.workspace,
      () => ({
        subscribeLogs: () => Promise.reject(new Error("boom")),
        unsubscribeLogs: vi.fn(),
      }),
      vi.fn(),
      warn
    );
    ws.change("error");
    await Promise.resolve();
    await Promise.resolve();
    expect(warn).toHaveBeenCalledWith(expect.stringContaining("boom"));
  });
});

describe("log level off (BT-5199)", () => {
  it("unsubscribes instead of subscribing when the level is set to off", () => {
    const ws = fakeWorkspace("warning");
    const subscribeLogs = vi.fn().mockResolvedValue(undefined);
    const unsubscribeLogs = vi.fn().mockResolvedValue(undefined);
    registerLogLevelSync(
      ws.workspace,
      () => ({ subscribeLogs, unsubscribeLogs }),
      vi.fn(),
      vi.fn()
    );

    ws.change("off");
    expect(unsubscribeLogs).toHaveBeenCalledTimes(1);
    expect(subscribeLogs).not.toHaveBeenCalled();

    ws.change("info");
    expect(subscribeLogs).toHaveBeenCalledWith("info");
  });

  it("applyLogLevel picks the op from the level", async () => {
    const client = {
      subscribeLogs: vi.fn().mockResolvedValue(undefined),
      unsubscribeLogs: vi.fn().mockResolvedValue(undefined),
    };
    await applyLogLevel(client, "off");
    await applyLogLevel(client, "error");
    expect(client.unsubscribeLogs).toHaveBeenCalledTimes(1);
    expect(client.subscribeLogs).toHaveBeenCalledWith("error");
  });
});

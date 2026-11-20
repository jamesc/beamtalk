// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { describe, expect, it, vi } from "vitest";
import { type LogLevelWorkspace, registerLogLevelSync } from "../logLevelSync";

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
    registerLogLevelSync(ws.workspace, () => ({ subscribeLogs }), bar, vi.fn());

    ws.change("debug");

    expect(subscribeLogs).toHaveBeenCalledWith("debug");
    expect(bar).toHaveBeenCalledWith("debug");
  });

  it("ignores unrelated settings and a missing client", () => {
    const ws = fakeWorkspace("warning");
    const subscribeLogs = vi.fn().mockResolvedValue(undefined);
    let client: { subscribeLogs: typeof subscribeLogs } | undefined = { subscribeLogs };
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
      () => ({ subscribeLogs: () => Promise.reject(new Error("boom")) }),
      vi.fn(),
      warn
    );
    ws.change("error");
    await Promise.resolve();
    await Promise.resolve();
    expect(warn).toHaveBeenCalledWith(expect.stringContaining("boom"));
  });
});

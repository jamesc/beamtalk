// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { describe, expect, it } from "vitest";
import {
  assertClassName,
  assertSessionId,
  btString,
  btSymbol,
  type ProbeRunner,
  procStartTime,
  startTimeMatches,
  WorkspaceClient,
} from "../workspaceClient";

describe("btString (BT-5069)", () => {
  it("escapes interpolation braces", () => {
    expect(btString("{1 + 1}")).toBe('"\\{1 + 1\\}"');
  });
  it("escapes quotes, backslashes and newlines", () => {
    expect(btString('a"b\\c\nd')).toBe('"a\\"b\\\\c\\nd"');
  });
  it("leaves no unescaped brace", () => {
    expect(btString("x{Smalltalk}y}{").replace(/\\[{}]/g, "")).not.toMatch(/[{}]/);
  });
});

describe("btSymbol", () => {
  it("uses #name for identifiers and #'…' otherwise", () => {
    expect(btSymbol("foo_1")).toBe("#foo_1");
    expect(btSymbol("a b")).toBe("#'a b'");
  });
  it("doubles embedded single quotes like the lexer's quoted symbol", () => {
    expect(btSymbol("a'b")).toBe("#'a''b'");
  });
  it("rejects names a quoted symbol cannot hold", () => {
    expect(() => btSymbol("a\\b")).toThrow();
  });
});

describe("validators", () => {
  it("accepts ids and rejects injection", () => {
    expect(assertSessionId("ab-12_Z")).toBe("ab-12_Z");
    expect(() => assertSessionId("a{1+1}")).toThrow();
    expect(assertClassName("Counter")).toBe("Counter");
    expect(() => assertClassName("Counter. 1 foo")).toThrow();
  });
});

describe("fromFiles stale port file (BT-5069)", () => {
  function setup(nodeInfo: object | null, portFile = "4000\nnonce-a"): string {
    const base = fs.mkdtempSync(path.join(os.tmpdir(), "bt-ws-"));
    const dir = path.join(base, "ws1");
    fs.mkdirSync(dir);
    fs.writeFileSync(path.join(dir, "port"), portFile);
    fs.writeFileSync(path.join(dir, "cookie"), "secret");
    if (nodeInfo) fs.writeFileSync(path.join(dir, "node.info"), JSON.stringify(nodeInfo));
    return base;
  }
  const live = { port: 4000, pid: process.pid, nonce: "nonce-a" };

  it("accepts a fresh port file", () => {
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: setup(live) })).not.toThrow();
  });
  it("rejects a dead pid", () => {
    const base = setup({ ...live, pid: 2147483646 });
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).toThrow(/Stale/);
  });
  it("explains an invisible pid in terms a user can act on", () => {
    const base = setup({ ...live, pid: 2147483646 });
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).toThrow(
      /not visible from where the extension runs.*BEAMTALK_HOME is shared across containers/
    );
  });
  it("rejects a nonce mismatch", () => {
    const base = setup({ ...live, nonce: "other" });
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).toThrow(/nonce/);
  });
  it("rejects a pid whose start time differs (pid reuse)", () => {
    if (process.platform !== "linux") return;
    const base = setup({ ...live, start_time: 1 });
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).toThrow(/start time/);
  });
  it("accepts the matching start time", () => {
    const st = procStartTime(process.pid);
    const base = setup({
      ...live,
      ...(st.kind === "time" ? { start_time: Number(st.value) } : {}),
    });
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).not.toThrow();
  });
  it("rejects a port file without a nonce when node.info has one", () => {
    const base = setup(live, "4000");
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).toThrow(/no nonce/);
  });
  it("fails closed on an unparseable node.info", () => {
    const base = setup(null);
    fs.writeFileSync(path.join(base, "ws1", "node.info"), "{trunc");
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).toThrow(/unreadable/);
  });
  it("rejects a port mismatch", () => {
    const base = setup({ ...live, port: 4001 });
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).toThrow(/Stale/);
  });
});

// BT-5189: the CLI records a start time on Windows and macOS too
// (`pid_liveness::proc_start_time`), so the extension must compare it there —
// it used to skip the check off Linux and hand the cookie to a reused pid.
describe("procStartTime on Windows and macOS (BT-5189)", () => {
  const runs =
    (out: string): ProbeRunner =>
    () =>
      out;
  const fails =
    (err: object): ProbeRunner =>
    () => {
      throw err;
    };

  it("reads the Windows FILETIME creation time exactly, past 2^53", () => {
    expect(procStartTime(1234, "win32", runs("133800000000000000123\r\n"))).toEqual({
      kind: "time",
      value: 133800000000000000123n,
      resolution: 1n,
    });
  });
  it("rejects a reused pid on Windows", () => {
    const observed = procStartTime(1234, "win32", runs("133800000000000000999"));
    expect(startTimeMatches(133800000000000000123n, observed)).toBe(false);
    expect(startTimeMatches(133800000000000000999n, observed)).toBe(true);
  });
  it("treats a pid with no process on Windows as gone (a mismatch)", () => {
    const observed = procStartTime(1234, "win32", runs("gone"));
    expect(observed).toEqual({ kind: "gone" });
    expect(startTimeMatches(1n, observed)).toBe(false);
  });
  it("trusts liveness when PowerShell cannot answer", () => {
    expect(procStartTime(1234, "win32", fails({ code: "ENOENT" }))).toEqual({ kind: "unknown" });
    expect(procStartTime(1234, "win32", runs("unknown"))).toEqual({ kind: "unknown" });
    expect(startTimeMatches(1n, { kind: "unknown" })).toBe(true);
  });
  it("compares macOS start times at ps's one-second resolution", () => {
    const observed = procStartTime(1234, "darwin", runs("Wed Nov 25 11:00:07 2026\n"));
    const seconds = BigInt(new Date(2026, 10, 25, 11, 0, 7).getTime() / 1000);
    expect(observed).toEqual({
      kind: "time",
      value: seconds * 1_000_000n,
      resolution: 1_000_000n,
    });
    // The CLI records microseconds: same second matches, another one does not.
    expect(startTimeMatches(seconds * 1_000_000n + 123_456n, observed)).toBe(true);
    expect(startTimeMatches((seconds - 1n) * 1_000_000n + 999_999n, observed)).toBe(false);
  });
  it("treats a pid ps does not know on macOS as gone, a missing ps as unknown", () => {
    expect(procStartTime(1234, "darwin", fails({ status: 1, signal: null }))).toEqual({
      kind: "gone",
    });
    expect(procStartTime(1234, "darwin", fails({ code: "ENOENT" }))).toEqual({ kind: "unknown" });
  });
  it("needs no recorded start time to match", () => {
    expect(startTimeMatches(undefined, { kind: "gone" })).toBe(true);
  });
});

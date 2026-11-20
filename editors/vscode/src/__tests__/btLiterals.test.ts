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
  it("rejects names a quoted symbol cannot hold", () => {
    expect(() => btSymbol("a'b")).toThrow();
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
  it("rejects a nonce mismatch", () => {
    const base = setup({ ...live, nonce: "other" });
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).toThrow(/nonce/);
  });
  it("rejects a port mismatch", () => {
    const base = setup({ ...live, port: 4001 });
    expect(() => WorkspaceClient.fromFiles("ws1", { baseDir: base })).toThrow(/Stale/);
  });
});

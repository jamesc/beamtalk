// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { readFileSync } from "node:fs";
import * as path from "node:path";
import { describe, expect, it } from "vitest";
import { parsePortFileContent } from "../workspaceClient";

// Shared with `beamtalk_workspace::parse_port_file` (Rust): both read
// tests/conformance/port_file_conformance.json.
const fixture = JSON.parse(
  readFileSync(
    path.join(__dirname, "../../../../tests/conformance/port_file_conformance.json"),
    "utf8"
  )
) as {
  cases: Array<{ name: string; content: string; port: number | null; nonce: string | null }>;
};

describe("parsePortFileContent (shared port-file conformance fixture)", () => {
  it("has cases", () => {
    expect(fixture.cases.length).toBeGreaterThan(0);
  });

  for (const c of fixture.cases) {
    it(c.name, () => {
      const parsed = parsePortFileContent(c.content);
      if (c.port === null) {
        expect(parsed.ok).toBe(false);
      } else {
        expect(parsed).toEqual({ ok: true, port: c.port, nonce: c.nonce ?? undefined });
      }
    });
  }
});

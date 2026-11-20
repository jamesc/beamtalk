// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/**
 * TypeScript half of the shared workspace-id fixture (BT-5069). The Rust half is
 * `test_workspace_id_fixture_shared_with_vscode_extension` in `beamtalk-workspace`.
 */
import { readFileSync } from "node:fs";
import * as path from "node:path";
import { describe, expect, it } from "vitest";
import { hashWorkspacePath, toRustCanonicalWindows } from "../workspaceId";

interface Case {
  platform: "posix" | "win32";
  input: string;
  canonical: string;
  id: string;
}

const CASES: Case[] = JSON.parse(
  readFileSync(
    path.resolve(
      __dirname,
      "../../../../crates/beamtalk-workspace/tests/fixtures/workspace_id_cases.json"
    ),
    "utf8"
  )
);

describe("workspace id fixture", () => {
  for (const c of CASES) {
    it(`${c.input} -> ${c.id}`, () => {
      const canonical = c.platform === "win32" ? toRustCanonicalWindows(c.input) : c.input;
      expect(canonical).toBe(c.canonical);
      expect(hashWorkspacePath(canonical)).toBe(c.id);
    });
  }
});

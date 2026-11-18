// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { readFileSync } from "node:fs";
import * as path from "node:path";
import { describe, expect, it } from "vitest";
import { beamtalkRootDir, workspacesBaseDir } from "../beamtalkRoot";

interface Row {
  name: string;
  env: Record<string, string>;
  root: string | null;
  workspaces: string | null;
}

// Shared Rust/Erlang/TypeScript corpus: the resolver rule lives in one place.
const fixture: Row[] = JSON.parse(
  readFileSync(
    path.resolve(
      __dirname,
      "../../../../runtime/apps/beamtalk_runtime/test/fixtures/beamtalk_root_dir_conformance.json"
    ),
    "utf8"
  )
);

// path.join normalises separators on Windows; compare in POSIX form.
const posix = (p: string | undefined) => p?.split(path.sep).join("/");

describe("beamtalkRootDir (conformance fixture)", () => {
  it("loads a non-empty corpus", () => {
    expect(fixture.length).toBeGreaterThan(0);
  });

  for (const row of fixture) {
    it(row.name, () => {
      // No fallback home: the fixture's env fully determines the result.
      expect(posix(beamtalkRootDir(row.env, () => undefined))).toBe(row.root ?? undefined);
      expect(posix(workspacesBaseDir(row.env, () => undefined))).toBe(row.workspaces ?? undefined);
    });
  }

  it("falls back to the OS home directory when HOME and USERPROFILE are unset", () => {
    expect(posix(beamtalkRootDir({}, () => "/os/home"))).toBe("/os/home/.beamtalk");
  });
});

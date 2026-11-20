// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as crypto from "node:crypto";
import * as fs from "node:fs";

/**
 * Rewrite a Windows path into the form Rust's `std::fs::canonicalize` returns
 * (the string the workspace id is hashed from): the `\\?\` extended prefix, an
 * upper-case drive letter, and `\\?\UNC\server\share` for UNC shares. Pure, so
 * it is pinned by the shared fixture on every platform.
 */
export function toRustCanonicalWindows(p: string): string {
  if (p.startsWith("\\\\?\\")) {
    return p;
  }
  if (p.startsWith("\\\\")) {
    return `\\\\?\\UNC\\${p.slice(2)}`;
  }
  const withDrive = p.replace(/^[a-z]:/, (d) => d.toUpperCase());
  return `\\\\?\\${withDrive}`;
}

/** SHA-256 of the canonical path string, first 12 hex chars (`hash_workspace_path_string`). */
export function hashWorkspacePath(canonical: string): string {
  return crypto.createHash("sha256").update(canonical).digest("hex").slice(0, 12);
}

/**
 * Compute the workspace ID for a project root directory.
 *
 * Replicates `beamtalk_workspace::generate_workspace_id`. Uses the native
 * realpath so the on-disk case of Windows paths matches Rust's `canonicalize`.
 * Pinned against Rust by `crates/beamtalk-workspace/tests/fixtures/workspace_id_cases.json`.
 */
export function computeWorkspaceId(projectRoot: string): string | null {
  try {
    let canonical = fs.realpathSync.native(projectRoot);
    if (process.platform === "win32") {
      canonical = toRustCanonicalWindows(canonical);
    }
    return hashWorkspacePath(canonical);
  } catch {
    return null;
  }
}

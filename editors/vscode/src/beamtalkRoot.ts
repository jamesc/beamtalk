// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as os from "node:os";
import * as path from "node:path";

/**
 * Resolve the Beamtalk root directory (`~/.beamtalk`, overridable with
 * `BEAMTALK_HOME`) with the same rule as `beamtalk_env::beamtalk_root_dir`
 * (Rust) and `beamtalk_platform:beamtalk_root_dir/0` (Erlang). The three are
 * pinned to `beamtalk_root_dir_conformance.json`.
 *
 * - A non-empty `BEAMTALK_HOME` wins and is used verbatim.
 * - An empty `BEAMTALK_HOME` is treated as unset.
 * - Otherwise `<home>/.beamtalk`, where home is `HOME`, else `USERPROFILE`
 *   (native Windows), else `os.homedir()`.
 * - No home at all resolves to `undefined` (no cache-dir fallback).
 */
export function beamtalkRootDir(
  env: Record<string, string | undefined> = process.env,
  fallbackHome: () => string | undefined = () => os.homedir() || undefined
): string | undefined {
  const override = env.BEAMTALK_HOME;
  if (override) {
    return override;
  }
  const home = env.HOME || env.USERPROFILE || fallbackHome();
  return home ? path.join(home, ".beamtalk") : undefined;
}

/** Directory holding per-workspace directories, or `undefined` without a home. */
export function workspacesBaseDir(
  env: Record<string, string | undefined> = process.env,
  fallbackHome?: () => string | undefined
): string | undefined {
  const root = beamtalkRootDir(env, fallbackHome);
  return root === undefined ? undefined : path.join(root, "workspaces");
}

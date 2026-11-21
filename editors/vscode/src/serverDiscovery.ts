// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { execFile } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { expandWorkspaceFolder, findProjectRoot } from "./projectRoot";

type ResolvedServerPath = {
  serverPath: string;
  warning?: string;
} | null;

function withPlatformExecutable(pathLike: string): string {
  if (process.platform !== "win32") {
    return pathLike;
  }

  return pathLike.endsWith(".exe") ? pathLike : `${pathLike}.exe`;
}

function fileExists(pathLike: string): boolean {
  return fs.existsSync(pathLike) || fs.existsSync(withPlatformExecutable(pathLike));
}

/**
 * Locate `beamtalk-lsp` by running `beamtalk --print-sysroot` asynchronously.
 *
 * Returns the absolute path to `{sysroot}/bin/beamtalk-lsp[.exe]`, or null
 * if `beamtalk` is not on PATH or the binary is absent from the sysroot.
 *
 * @param beamtalkBin - Path or command name for the `beamtalk` CLI binary.
 *   Defaults to bare `"beamtalk"` (PATH lookup). Override with
 *   `beamtalk.binary.path` to use a dev build such as `./target/debug/beamtalk`.
 */
function findLspViaSysroot(beamtalkBin = "beamtalk"): Promise<string | null> {
  return new Promise((resolve) => {
    execFile(
      beamtalkBin,
      ["--print-sysroot"],
      { encoding: "utf8", timeout: 5000, windowsHide: true },
      (error, stdout) => {
        if (error || !stdout?.trim()) {
          resolve(null);
          return;
        }
        const sysroot = stdout.trim();
        const ext = process.platform === "win32" ? ".exe" : "";
        const lspPath = path.join(sysroot, "bin", `beamtalk-lsp${ext}`);
        resolve(fs.existsSync(lspPath) ? lspPath : null);
      }
    );
  });
}

/**
 * Check if `beamtalk-lsp` is available on PATH, asynchronously.
 */
function lspIsOnPath(): Promise<boolean> {
  return new Promise((resolve) => {
    execFile(
      withPlatformExecutable("beamtalk-lsp"),
      ["--help"],
      { timeout: 3000, windowsHide: true },
      (error) => resolve(!error)
    );
  });
}

/**
 * Resolve the path to `beamtalk-lsp`.
 *
 * Resolution order:
 * 1. User-configured override (`beamtalk.server.path`)
 * 2. Sysroot discovery via `beamtalk --print-sysroot` → `{sysroot}/bin/beamtalk-lsp`
 *    Uses `beamtalk.binary.path` if configured, otherwise bare `beamtalk`.
 * 3. Bare `beamtalk-lsp` command name (requires PATH)
 *
 * Returns null if Beamtalk is not installed and no override is configured.
 */
export async function resolveServerPath(
  context: vscode.ExtensionContext
): Promise<ResolvedServerPath> {
  const config = vscode.workspace.getConfiguration("beamtalk");
  const projectRoot = findProjectRoot();
  const expandVars = (s: string) => expandWorkspaceFolder(s, projectRoot);
  const override = expandVars(config.get<string>("server.path", "").trim());
  const beamtalkBin = expandVars(config.get<string>("binary.path", "").trim()) || "beamtalk";
  let warning: string | undefined;

  // 1. Explicit user override
  if (override) {
    const isPathLike =
      override.includes("/") ||
      override.includes("\\") ||
      override.startsWith(".") ||
      path.isAbsolute(override);

    if (!isPathLike) {
      return { serverPath: override };
    }

    const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
    const candidates = [
      path.isAbsolute(override) ? override : undefined,
      workspaceRoot ? path.resolve(workspaceRoot, override) : undefined,
      path.resolve(context.extensionPath, override),
      path.resolve(override),
    ].filter((candidate): candidate is string => Boolean(candidate));

    const resolved = candidates.find((candidate) => fileExists(candidate));
    if (resolved) {
      return { serverPath: withPlatformExecutable(resolved) };
    }

    warning =
      `Configured beamtalk.server.path does not exist: ${override}. ` +
      "Falling back to automatic discovery (sysroot, then PATH).";
    // Path-like override was invalid: warn and fall through.
  }

  // 2. Sysroot discovery via `beamtalk --print-sysroot` (or configured binary).
  //    In dev mode, `beamtalk.binary.path` should point to `./target/debug/beamtalk`.
  const sysrootLsp = await findLspViaSysroot(beamtalkBin);
  if (sysrootLsp) {
    return { serverPath: sysrootLsp, warning };
  }

  // 3. Fall back to bare PATH lookup — only if beamtalk-lsp is available
  if (await lspIsOnPath()) {
    return { serverPath: "beamtalk-lsp", warning };
  }

  // Beamtalk not found
  return null;
}

/**
 * Resolve `beamtalk.stdlibSourceDir` for `initializationOptions`.
 *
 * Expands a leading `${workspaceFolder}` (the server itself only resolves
 * bare relative paths against its discovered project roots, so a literal
 * `${workspaceFolder}` placeholder must be substituted client-side). Empty
 * when unset, which tells the server to fall back to sysroot auto-discovery.
 */
export function resolveStdlibSourceDir(): string {
  const config = vscode.workspace.getConfiguration("beamtalk");
  const configured = config.get<string>("stdlibSourceDir", "").trim();
  if (!configured) {
    return "";
  }
  return expandWorkspaceFolder(configured, findProjectRoot());
}

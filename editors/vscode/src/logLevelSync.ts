// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/** The slice of `vscode.workspace` the log-level sync needs (keeps this module testable). */
export interface LogLevelWorkspace {
  onDidChangeConfiguration(listener: (e: { affectsConfiguration(s: string): boolean }) => void): {
    dispose(): void;
  };
  getConfiguration(section: string): { get<T>(key: string, fallback: T): T };
}

/** The slice of `WorkspaceClient` the log-level sync needs. */
export interface LogSubscriber {
  subscribeLogs(level: string): Promise<unknown>;
  /** Connection state, when known; a non-connected client subscribes on `connected`. */
  readonly state?: string;
}

/**
 * Push a new log level to the workspace's server-side log filter. Shared by the
 * QuickPick command and the Settings-UI handler so both re-subscribe (BT-5082:
 * the Settings path used to update only the status bar).
 */
export function resubscribeLogs(
  client: LogSubscriber | undefined,
  level: string,
  warn: (msg: string) => void
): void {
  if (!client) return;
  if (client.state !== undefined && client.state !== "connected") return; // re-subscribed on connect
  client.subscribeLogs(level).catch((err) => {
    warn(`Failed to update log subscription: ${err instanceof Error ? err.message : String(err)}`);
  });
}

/**
 * Keep the status bar and the server-side log subscription in step with the
 * `beamtalk.logs.level` setting, however it was changed.
 */
export function registerLogLevelSync(
  workspace: LogLevelWorkspace,
  getClient: () => LogSubscriber | undefined,
  updateStatusBar: (level: string) => void,
  warn: (msg: string) => void
): { dispose(): void } {
  return workspace.onDidChangeConfiguration((e) => {
    if (!e.affectsConfiguration("beamtalk.logs.level")) return;
    const level = workspace.getConfiguration("beamtalk").get<string>("logs.level", "warning");
    updateStatusBar(level);
    resubscribeLogs(getClient(), level, warn);
  });
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { InspectorPanel } from "./inspectorPanel";
import { TranscriptViewProvider } from "./transcriptView";
import type { ClassOrigin } from "./workspaceClient";
import {
  type ActorItemNode,
  type BindingItemNode,
  type ClassItemNode,
  ALL_CLASS_ORIGINS,
  WorkspaceTreeDataProvider,
} from "./workspaceTreeView";
import { openStdlibDocumentForClass } from "./virtualDocuments";
import { state } from "./extensionState";

/** `context.workspaceState` key persisting the "Classes" section's origin filter. */
const CLASS_FILTER_STATE_KEY = "beamtalk.classOriginFilter";

/** QuickPick label for each class origin (BT-2552 badges), in menu display order. */
const CLASS_ORIGIN_LABELS: Record<ClassOrigin, string> = {
  project: "Project",
  dependency: "Dependencies",
  stdlib: "Standard Library",
};

/**
 * The Workspace Explorer tree, Transcript view and their commands (BT-1023,
 * BT-1024): refresh, class filter, inspect and reload.
 */
export function registerWorkspaceExplorer(context: vscode.ExtensionContext): void {
  // ─── Workspace Explorer (BT-1023) ──────────────────────────────────────────

  state.workspaceTreeProvider = new WorkspaceTreeDataProvider();
  context.subscriptions.push(state.workspaceTreeProvider);
  // Lets sidebar hover tooltips (resolveTreeItem) read source for stdlib
  // classes too, via the same beamtalk-stdlib:// virtual URI fallback
  // navigation already uses — without it, a stdlib-defined class or a method/
  // state var inherited from one never has a real source_file to read a doc
  // comment from, and the hover always falls back to hardcoded metadata.
  state.workspaceTreeProvider.setStdlibDocumentOpener(openStdlibDocumentForClass);

  // Restore the "Classes" filter from the last session in this workspace, if any.
  const persistedClassFilter = context.workspaceState.get<ClassOrigin[]>(CLASS_FILTER_STATE_KEY);
  const isPersistedFilterActive =
    !!persistedClassFilter &&
    persistedClassFilter.length > 0 &&
    persistedClassFilter.length < ALL_CLASS_ORIGINS.length;
  if (isPersistedFilterActive) {
    state.workspaceTreeProvider.setClassOriginFilter(new Set(persistedClassFilter));
  }
  void vscode.commands.executeCommand(
    "setContext",
    "beamtalk.classFilterActive",
    isPersistedFilterActive
  );

  context.subscriptions.push(
    vscode.window.createTreeView("beamtalk.workspaceExplorer", {
      treeDataProvider: state.workspaceTreeProvider,
    })
  );

  // ─── Transcript View (BT-1024) ─────────────────────────────────────────────

  state.transcriptViewProvider = new TranscriptViewProvider({
    fullReload: vscode.env.remoteName === "wsl",
  });
  context.subscriptions.push(state.transcriptViewProvider);

  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      TranscriptViewProvider.viewType,
      state.transcriptViewProvider
    )
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.clearTranscript", () => {
      state.transcriptViewProvider?.clear();
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.refreshWorkspace", () => {
      void state.workspaceTreeProvider?.refresh();
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.filterClasses", async () => {
      if (!state.workspaceTreeProvider) return;
      const current = state.workspaceTreeProvider.classFilter;
      const picks: Array<vscode.QuickPickItem & { origin: ClassOrigin }> = ALL_CLASS_ORIGINS.map(
        (origin) => ({
          label: CLASS_ORIGIN_LABELS[origin],
          origin,
          picked: current.has(origin),
        })
      );
      const selected = await vscode.window.showQuickPick(picks, {
        canPickMany: true,
        title: "Filter Classes",
        placeHolder: "Show classes from… (nothing checked shows everything)",
      });
      // undefined means the picker was dismissed (Escape) — leave the filter as-is.
      if (selected === undefined) return;
      const origins = new Set<ClassOrigin>(selected.map((pick) => pick.origin));
      state.workspaceTreeProvider.setClassOriginFilter(origins);
      await context.workspaceState.update(
        CLASS_FILTER_STATE_KEY,
        origins.size > 0 ? [...origins] : undefined
      );
      void vscode.commands.executeCommand(
        "setContext",
        "beamtalk.classFilterActive",
        origins.size > 0 && origins.size < ALL_CLASS_ORIGINS.length
      );
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.clearClassFilter", async () => {
      state.workspaceTreeProvider?.setClassOriginFilter(new Set());
      await context.workspaceState.update(CLASS_FILTER_STATE_KEY, undefined);
      void vscode.commands.executeCommand("setContext", "beamtalk.classFilterActive", false);
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.inspectBinding", (node?: BindingItemNode) => {
      if (!node || node.kind !== "binding-item") {
        void vscode.window.showInformationMessage("Select a binding in Workspace Explorer first.");
        return;
      }
      const sessionId =
        state.workspaceTreeProvider?.currentSessionId ??
        state.workspaceWsClient?.currentSessionId ??
        null;
      InspectorPanel.show(state.workspaceWsClient ?? null, {
        kind: "binding",
        name: node.name,
        value: node.value,
        sessionId,
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.inspectActor", (node?: ActorItemNode) => {
      if (!node || node.kind !== "actor-item") {
        void vscode.window.showInformationMessage("Select an actor in Workspace Explorer first.");
        return;
      }
      InspectorPanel.show(state.workspaceWsClient ?? null, {
        kind: "actor",
        info: node.info,
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.reloadClass", async (node?: ClassItemNode) => {
      if (!state.workspaceWsClient) {
        await vscode.window.showInformationMessage("Not connected to a Beamtalk workspace.");
        return;
      }
      const className = node?.info.name;
      if (!className) {
        await vscode.window.showInformationMessage("No class selected.");
        return;
      }
      try {
        // BT-2091: Reloads via `ClassName reload` (the deprecated `reload`
        // protocol op has been removed). The runtime locates the source
        // file via the class's recorded `source_file` attribute.
        const result = await state.workspaceWsClient.reload(className);
        const names = result.classes.map((c) => c.name).filter(Boolean);
        const baseMessage =
          names.length > 0 ? `Reloaded: ${names.join(", ")}` : "Reload completed.";
        const warningText = result.warnings.length > 0 ? ` (${result.warnings.join("; ")})` : "";
        await vscode.window.showInformationMessage(`${baseMessage}${warningText}`);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        await vscode.window.showErrorMessage(`Reload failed: ${msg}`);
      }
    })
  );
}

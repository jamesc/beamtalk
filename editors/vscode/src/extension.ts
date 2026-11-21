// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { resolveClassDocument } from "./documentResolution";
import { state } from "./extensionState";
import { InspectorPanel } from "./inspectorPanel";
import { registerLogLevelSync } from "./logLevelSync";
import { resolveDeclarationOffset } from "./symbolLookup";
import { TranscriptViewProvider } from "./transcriptView";
import type { ClassOrigin } from "./workspaceClient";
import {
  type ActorItemNode,
  type BindingItemNode,
  type ClassItemNode,
  type MethodItemNode,
  type StateVarItemNode,
  type TypeAliasItemNode,
  ALL_CLASS_ORIGINS,
  WorkspaceTreeDataProvider,
} from "./workspaceTreeView";
import {
  AliasContentProvider,
  StdlibContentProvider,
  openAliasSourceDocument,
  openStdlibDocumentForClass,
} from "./virtualDocuments";
import { BeamtalkTaskProvider } from "./taskProvider";
import { startClient } from "./languageClient";
import { setupAutoConnect } from "./workspaceConnection";
import { setReplRunning, setupTerminalMonitoring, startReplCommand } from "./repl";

const LOG_LEVELS = [
  "debug",
  "info",
  "notice",
  "warning",
  "error",
  "critical",
  "alert",
  "emergency",
] as const;

type LogLevel = (typeof LOG_LEVELS)[number];

/** `context.workspaceState` key persisting the "Classes" section's origin filter. */
const CLASS_FILTER_STATE_KEY = "beamtalk.classOriginFilter";

/** QuickPick label for each class origin (BT-2552 badges), in menu display order. */
const CLASS_ORIGIN_LABELS: Record<ClassOrigin, string> = {
  project: "Project",
  dependency: "Dependencies",
  stdlib: "Standard Library",
};

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  // Reset context state on each activation — VS Code persists setContext values
  // across extension host restarts, so we must re-initialise on every activation.
  setReplRunning(false);

  const stdlibProvider = new StdlibContentProvider();
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("beamtalk-stdlib", stdlibProvider)
  );

  const aliasProvider = new AliasContentProvider();
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("beamtalk-alias", aliasProvider)
  );

  context.subscriptions.push(
    vscode.tasks.registerTaskProvider("beamtalk", new BeamtalkTaskProvider())
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.restartServer", async () => {
      state.outputChannel?.info("Restarting language server…");
      if (state.client) {
        await state.client.stop();
        state.client = undefined;
      }
      await startClient(context);
    })
  );

  // ─── Beamtalk Logs output channel + log level picker (BT-1433) ────────────

  state.logOutputChannel ??= vscode.window.createOutputChannel("Beamtalk Logs");

  state.logLevelStatusBarItem?.dispose();
  state.logLevelStatusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    50
  );
  state.logLevelStatusBarItem.command = "beamtalk.setLogLevel";
  state.logLevelStatusBarItem.tooltip = "Beamtalk log level — click to change";
  context.subscriptions.push(state.logLevelStatusBarItem);

  const updateLogLevelStatusBar = (level: string): void => {
    if (state.logLevelStatusBarItem) {
      state.logLevelStatusBarItem.text = `$(filter) ${level}`;
      state.logLevelStatusBarItem.show();
    }
  };

  {
    const config = vscode.workspace.getConfiguration("beamtalk");
    updateLogLevelStatusBar(config.get<string>("logs.level", "warning"));
  }

  // Keep the status bar and the server-side log filter in sync when the
  // setting is changed via Settings UI (BT-5082).
  context.subscriptions.push(
    registerLogLevelSync(
      vscode.workspace,
      () => state.workspaceWsClient,
      updateLogLevelStatusBar,
      (msg) => state.outputChannel?.warn(msg)
    )
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.setLogLevel", async () => {
      const config = vscode.workspace.getConfiguration("beamtalk");
      const current = config.get<string>("logs.level", "warning");
      const picked = await vscode.window.showQuickPick(
        LOG_LEVELS.map((l) => ({
          label: l,
          description: l === current ? "current" : undefined,
        })),
        { title: "Beamtalk: Set Log Level", placeHolder: "Select minimum log level" }
      );
      if (!picked) return;
      const newLevel = picked.label as LogLevel;
      await config.update("logs.level", newLevel, vscode.ConfigurationTarget.Global);
      // The onDidChangeConfiguration handler above updates the status bar and re-subscribes.
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.showLogs", () => {
      state.logOutputChannel?.show();
    })
  );

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
    vscode.commands.registerCommand("beamtalk.openClassSource", async (node?: ClassItemNode) => {
      const result = node
        ? await resolveClassDocument(node.info, openStdlibDocumentForClass)
        : ({ kind: "no-source" } as const);
      let document: vscode.TextDocument;
      if (result.kind === "opened") {
        document = result.document;
      } else if (result.kind === "source-file-error") {
        await vscode.window.showErrorMessage(
          `Could not open source: ${result.sourceFile}: ${result.error}`
        );
        return;
      } else {
        await vscode.window.showInformationMessage("Source not available for this class.");
        return;
      }

      const className = node?.info.name;
      if (!className) return;

      // Same three-tier resolver as navigateToMethod/navigateToStateVar/hover
      // (regex first — cheap, no LSP round trip needed for the common case —
      // then the document-symbol-provider fallback for whatever shape defeats it).
      const declOffset = await resolveDeclarationOffset(document, {
        kind: "class",
        name: className,
      });
      const position = declOffset === -1 ? undefined : document.positionAt(declOffset);

      await vscode.window.showTextDocument(document, {
        selection: position ? new vscode.Range(position, position) : undefined,
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand(
      "beamtalk.navigateToTypeAlias",
      async (node?: TypeAliasItemNode) => {
        const aliasName = node?.info.name;
        if (!aliasName) return;

        // BT-3496: `TypeAliasInfo.source_file` is a package-relative display
        // path, never one `vscode.Uri.file()` can open directly — go through
        // the `beamtalk-alias://` virtual URI (`browse-alias-source` op)
        // instead, same as `openStdlibDocumentForClass` does for stdlib
        // classes via `beamtalk-stdlib://`.
        const document = await openAliasSourceDocument(node.info);
        if (!document) {
          await vscode.window.showInformationMessage("Source not available for this type alias.");
          return;
        }

        // Same resolver as openClassSource — no xref line is tracked for
        // aliases (ADR 0108 Phase 8: `TypeAliasInfo` carries no `line`), and
        // no document-symbol fallback exists yet either (the Rust
        // document-symbols provider doesn't emit alias symbols), so this is
        // regex-only in practice today; the resolver still tries the
        // document-symbol tier for forward compatibility if that changes.
        const declOffset = await resolveDeclarationOffset(document, {
          kind: "type-alias",
          name: aliasName,
        });
        const position = declOffset === -1 ? undefined : document.positionAt(declOffset);

        await vscode.window.showTextDocument(document, {
          selection: position ? new vscode.Range(position, position) : undefined,
        });
      }
    )
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

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.navigateToMethod", async (node: MethodItemNode) => {
      const { classInfo, method } = node;

      const result = await resolveClassDocument(classInfo, openStdlibDocumentForClass);
      let document: vscode.TextDocument;
      if (result.kind === "opened") {
        document = result.document;
      } else if (result.kind === "source-file-error") {
        await vscode.window.showInformationMessage(
          `Cannot open source for ${classInfo.name}: ${result.sourceFile}`
        );
        return;
      } else {
        await vscode.window.showInformationMessage(
          `No source file recorded for class ${classInfo.name}`
        );
        return;
      }
      const uri = document.uri;

      // BT-3439: prefer the real declaration line from beamtalk_xref's
      // compiled index over guessing via source-text regex/search — the
      // regex (methodHeadPattern) can miss syntax it doesn't understand yet,
      // so only fall back to the regex/text-search path when no real line is
      // available (a class compiled before this field existed, or a
      // ClassBuilder-built class with no compiler to derive one from) or the
      // line is stale (the file was edited since the class was last
      // compiled/reloaded). LSP go-to-definition is used *after*
      // showTextDocument below for precise navigation either way.
      const text = document.getText();
      let declOffset = await resolveDeclarationOffset(
        document,
        { kind: "method", side: method.side, selector: method.selector },
        method.line
      );
      // Last resort: a bare substring search, which can land on an unrelated
      // mention (e.g. a doc-comment usage example) — only reached if the
      // real line, regex, and document-symbol provider all came up empty.
      if (declOffset === -1) declOffset = text.indexOf(method.selector);
      if (declOffset === -1) {
        await vscode.window.showInformationMessage(
          `Method '${method.selector}' not found in source for ${classInfo.name}`
        );
        return;
      }
      const position = document.positionAt(declOffset);

      // Try LSP go-to-definition for a precise location. If LSP is unavailable or
      // returns no results, fall back to the text-search position — don't fail.
      let locations: Array<vscode.Location | vscode.LocationLink> | undefined;
      try {
        locations = await vscode.commands.executeCommand<
          Array<vscode.Location | vscode.LocationLink>
        >("vscode.executeDefinitionProvider", uri, position);
      } catch {
        // LSP not available — fall through to direct navigation.
      }

      if (locations && locations.length > 0) {
        const loc = locations[0];
        const targetUri = "targetUri" in loc ? loc.targetUri : loc.uri;
        const targetRange = "targetRange" in loc ? loc.targetRange : loc.range;
        await vscode.window.showTextDocument(targetUri, { selection: targetRange });
      } else {
        // LSP unavailable or returned no results — navigate to the method declaration.
        await vscode.window.showTextDocument(document, {
          selection: new vscode.Range(position, position),
        });
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand(
      "beamtalk.navigateToStateVar",
      async (node?: StateVarItemNode) => {
        if (!node || node.kind !== "state-item") {
          await vscode.window.showInformationMessage(
            "Select a state variable in Workspace Explorer first."
          );
          return;
        }
        const { classInfo, stateVar } = node;
        const result = await resolveClassDocument(classInfo, openStdlibDocumentForClass);
        let document: vscode.TextDocument;
        if (result.kind === "opened") {
          document = result.document;
        } else if (result.kind === "source-file-error") {
          await vscode.window.showInformationMessage(
            `Cannot open source for ${classInfo.name}: ${result.sourceFile}`
          );
          return;
        } else {
          await vscode.window.showInformationMessage(
            `No source file recorded for class ${classInfo.name}`
          );
          return;
        }
        // BT-3439: prefer the real declaration line from beamtalk_xref's
        // compiled index — see the analogous comment in navigateToMethod.
        const text = document.getText();
        let declOffset = await resolveDeclarationOffset(
          document,
          { kind: "field", name: stateVar.name },
          stateVar.line
        );
        // Last resort: a bare substring search.
        if (declOffset === -1) declOffset = text.indexOf(stateVar.name);
        if (declOffset === -1) {
          await vscode.window.showInformationMessage(
            `State variable '${stateVar.name}' not found in source for ${classInfo.name}`
          );
          return;
        }
        const position = document.positionAt(declOffset);
        await vscode.window.showTextDocument(document, {
          selection: new vscode.Range(position, position),
        });
      }
    )
  );

  // ─── Start / Stop REPL commands (BT-1025) ──────────────────────────────────

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.startRepl", () => startReplCommand(context))
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("beamtalk.stopRepl", () => {
      // Close the REPL terminal — clears the session and bindings,
      // but leaves the workspace connection up so classes/actors remain visible.
      if (state.replTerminal && state.replTerminal.exitStatus === undefined) {
        state.replTerminal.dispose();
      }
    })
  );

  // ─── Auto-connect + terminal monitoring (BT-1025) ──────────────────────────

  setupTerminalMonitoring(context);
  setupAutoConnect(context);

  await startClient(context);
}

export function deactivate(): Thenable<void> | undefined {
  const stop = state.client?.stop();
  if (state.portDebounceTimer !== null) {
    clearTimeout(state.portDebounceTimer);
    state.portDebounceTimer = null;
  }
  // Close all inspector panels before disposing the client so they don't
  // attempt to re-fetch after the connection is gone.
  InspectorPanel.disposeAll();
  // Providers are disposed via context.subscriptions; dispose() is idempotent
  // so no double-dispose risk, but we detach the client first to ensure no
  // late push events fire into disposed providers.
  state.workspaceWsClient?.dispose();
  state.workspaceWsClient = undefined;
  state.replTerminal = undefined;
  state.workspaceTreeProvider?.setClient(null);
  state.workspaceTreeProvider = undefined;
  state.transcriptViewProvider?.setClient(null);
  state.transcriptViewProvider = undefined;
  state.logLevelStatusBarItem?.dispose();
  state.logLevelStatusBarItem = undefined;
  state.logOutputChannel?.dispose();
  state.outputChannel?.dispose();
  state.traceOutputChannel?.dispose();
  state.logOutputChannel = undefined;
  state.outputChannel = undefined;
  state.traceOutputChannel = undefined;
  return stop;
}

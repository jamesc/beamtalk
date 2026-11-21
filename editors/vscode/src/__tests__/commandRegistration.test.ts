// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { readFileSync } from "node:fs";
import * as path from "node:path";
import { beforeEach, describe, expect, it, vi } from "vitest";
import { buildVscodeModule } from "./vscodeMock";

// ─── vscode mock that records command registrations (BT-5092) ────────────────
//
// `activate()` was split into per-feature `register…` functions so the command
// layer can be exercised without an extension host. The mock below captures
// every `registerCommand` handler so a test can invoke it directly.

const h = vi.hoisted(() => {
  const handlers = new Map<string, (...args: unknown[]) => unknown>();
  return {
    handlers,
    executeCommandMock: vi.fn(),
    openTextDocumentMock: vi.fn(),
    showInformationMessage: vi.fn(),
    showErrorMessage: vi.fn(),
    showWarningMessage: vi.fn(),
    showQuickPick: vi.fn(),
    createTreeView: vi.fn(() => ({ dispose: () => {} })),
    createStatusBarItem: vi.fn(),
    createOutputChannel: vi.fn(),
    registerWebviewViewProvider: vi.fn(() => ({ dispose: () => {} })),
    getConfiguration: vi.fn(),
    configUpdate: vi.fn(),
    configValues: {} as Record<string, unknown>,
  };
});

vi.mock("vscode", () => {
  const base = buildVscodeModule({
    executeCommandMock: h.executeCommandMock,
    openTextDocumentMock: h.openTextDocumentMock,
  });
  return {
    ...base,
    StatusBarAlignment: { Left: 1, Right: 2 },
    ConfigurationTarget: { Global: 1 },
    env: { remoteName: undefined },
    commands: {
      executeCommand: h.executeCommandMock,
      registerCommand: (id: string, fn: (...args: unknown[]) => unknown) => {
        h.handlers.set(id, fn);
        return { dispose: () => h.handlers.delete(id) };
      },
    },
    window: {
      showInformationMessage: h.showInformationMessage,
      showErrorMessage: h.showErrorMessage,
      showWarningMessage: h.showWarningMessage,
      showQuickPick: h.showQuickPick,
      createTreeView: h.createTreeView,
      createStatusBarItem: h.createStatusBarItem,
      createOutputChannel: h.createOutputChannel,
      registerWebviewViewProvider: h.registerWebviewViewProvider,
    },
    workspace: {
      openTextDocument: h.openTextDocumentMock,
      getConfiguration: h.getConfiguration,
      onDidChangeConfiguration: () => ({ dispose: () => {} }),
    },
  };
});

vi.mock("vscode-languageclient/node", () => ({
  LanguageClient: class {},
  RevealOutputChannelOn: { Never: 4 },
}));

// `restartServer` re-runs `startClient`; keep it from probing the machine for a
// real `beamtalk-lsp` binary.
vi.mock("../serverDiscovery", () => ({
  resolveServerPath: vi.fn(async () => null),
  resolveStdlibSourceDir: vi.fn(() => ""),
}));

import { InspectorPanel } from "../inspectorPanel";
import { state } from "../extensionState";
import { registerServerCommands } from "../languageClient";
import { registerLogUi } from "../logUi";
import { registerNavigationCommands } from "../navigationCommands";
import { registerReplCommands } from "../repl";
import { registerWorkspaceExplorer } from "../workspaceExplorer";
import type { TranscriptViewProvider } from "../transcriptView";
import { ALL_CLASS_ORIGINS, type WorkspaceTreeDataProvider } from "../workspaceTreeView";

interface FakeContext {
  subscriptions: Array<{ dispose(): unknown }>;
  workspaceState: { get: ReturnType<typeof vi.fn>; update: ReturnType<typeof vi.fn> };
  extensionPath: string;
}

function makeContext(persistedFilter?: string[]): FakeContext {
  return {
    subscriptions: [],
    extensionPath: "/ext",
    workspaceState: {
      get: vi.fn(() => persistedFilter),
      update: vi.fn(async () => {}),
    },
  };
}

function resetState(): void {
  state.client = undefined;
  state.outputChannel = undefined;
  state.traceOutputChannel = undefined;
  state.logOutputChannel = undefined;
  state.logLevelStatusBarItem = undefined;
  state.workspaceTreeProvider = undefined;
  state.transcriptViewProvider = undefined;
  state.workspaceWsClient = undefined;
  state.replTerminal = undefined;
  state.replRunning = false;
  state.portDebounceTimer = null;
}

function call(id: string, ...args: unknown[]): unknown {
  const handler = h.handlers.get(id);
  if (!handler) throw new Error(`command not registered: ${id}`);
  return handler(...args);
}

function registerAll(context: FakeContext): void {
  const ctx = context as never;
  registerServerCommands(ctx);
  registerLogUi(ctx);
  registerWorkspaceExplorer(ctx);
  registerNavigationCommands(ctx);
  registerReplCommands(ctx);
}

beforeEach(() => {
  h.handlers.clear();
  for (const fn of [
    h.executeCommandMock,
    h.showInformationMessage,
    h.showErrorMessage,
    h.showWarningMessage,
    h.showQuickPick,
    h.configUpdate,
  ]) {
    fn.mockReset();
  }
  h.configValues = {};
  h.getConfiguration.mockImplementation(() => ({
    get: (key: string, fallback: unknown) =>
      key in h.configValues ? h.configValues[key] : fallback,
    update: h.configUpdate,
  }));
  h.createStatusBarItem.mockImplementation(() => ({
    text: "",
    command: undefined,
    tooltip: undefined,
    show: vi.fn(),
    dispose: vi.fn(),
  }));
  h.createOutputChannel.mockImplementation(() => ({
    info: vi.fn(),
    warn: vi.fn(),
    error: vi.fn(),
    show: vi.fn(),
    appendLine: vi.fn(),
    dispose: vi.fn(),
  }));
  resetState();
});

describe("command registration", () => {
  it("registers exactly the commands package.json contributes", () => {
    const pkg = JSON.parse(
      readFileSync(path.join(__dirname, "..", "..", "package.json"), "utf8")
    ) as { contributes: { commands: Array<{ command: string }> } };
    const declared = pkg.contributes.commands.map((c) => c.command).sort();

    registerAll(makeContext());

    expect([...h.handlers.keys()].sort()).toEqual(declared);
  });

  it("pushes a disposable for every registration", () => {
    const ctx = makeContext();
    registerAll(ctx);
    expect(ctx.subscriptions.length).toBeGreaterThanOrEqual(h.handlers.size);
  });
});

describe("workspace explorer commands", () => {
  it("restores a persisted partial class filter and sets the context key", () => {
    registerAll(makeContext(["project"]));
    expect([...(state.workspaceTreeProvider?.classFilter ?? [])]).toEqual(["project"]);
    expect(h.executeCommandMock).toHaveBeenCalledWith(
      "setContext",
      "beamtalk.classFilterActive",
      true
    );
  });

  it("filterClasses applies and persists the picked origins", async () => {
    const ctx = makeContext();
    registerAll(ctx);
    h.showQuickPick.mockResolvedValue([{ origin: "stdlib" }]);

    await call("beamtalk.filterClasses");

    expect([...(state.workspaceTreeProvider?.classFilter ?? [])]).toEqual(["stdlib"]);
    expect(ctx.workspaceState.update).toHaveBeenCalledWith("beamtalk.classOriginFilter", [
      "stdlib",
    ]);
    expect(h.executeCommandMock).toHaveBeenLastCalledWith(
      "setContext",
      "beamtalk.classFilterActive",
      true
    );
  });

  it("filterClasses leaves the filter alone when the picker is dismissed", async () => {
    const ctx = makeContext(["project"]);
    registerAll(ctx);
    h.showQuickPick.mockResolvedValue(undefined);

    await call("beamtalk.filterClasses");

    expect([...(state.workspaceTreeProvider?.classFilter ?? [])]).toEqual(["project"]);
    expect(ctx.workspaceState.update).not.toHaveBeenCalled();
  });

  it("clearClassFilter empties the filter, the persisted value and the context key", async () => {
    const ctx = makeContext(["project"]);
    registerAll(ctx);

    await call("beamtalk.clearClassFilter");

    // An empty filter means "show everything": every origin is reported.
    expect(state.workspaceTreeProvider?.classFilter.size).toBe(ALL_CLASS_ORIGINS.length);
    expect(ctx.workspaceState.update).toHaveBeenCalledWith("beamtalk.classOriginFilter", undefined);
    expect(h.executeCommandMock).toHaveBeenLastCalledWith(
      "setContext",
      "beamtalk.classFilterActive",
      false
    );
  });

  it("inspectBinding asks for a binding when invoked without one", () => {
    registerAll(makeContext());
    const show = vi.spyOn(InspectorPanel, "show").mockImplementation(() => {});

    call("beamtalk.inspectBinding", undefined);
    call("beamtalk.inspectBinding", { kind: "actor-item" });

    expect(h.showInformationMessage).toHaveBeenCalledTimes(2);
    expect(show).not.toHaveBeenCalled();
    show.mockRestore();
  });

  it("inspectBinding opens the inspector for a binding node", () => {
    registerAll(makeContext());
    const show = vi.spyOn(InspectorPanel, "show").mockImplementation(() => {});

    call("beamtalk.inspectBinding", { kind: "binding-item", name: "x", value: "42" });

    expect(show).toHaveBeenCalledWith(null, {
      kind: "binding",
      name: "x",
      value: "42",
      sessionId: null,
    });
    show.mockRestore();
  });

  it("inspectActor opens the inspector for an actor node and rejects others", () => {
    registerAll(makeContext());
    const show = vi.spyOn(InspectorPanel, "show").mockImplementation(() => {});

    call("beamtalk.inspectActor", undefined);
    expect(h.showInformationMessage).toHaveBeenCalledTimes(1);

    const info = { pid: "<0.1.0>" };
    call("beamtalk.inspectActor", { kind: "actor-item", info });
    expect(show).toHaveBeenCalledWith(null, { kind: "actor", info });
    show.mockRestore();
  });

  it("reloadClass reports when no workspace is connected", async () => {
    registerAll(makeContext());

    await call("beamtalk.reloadClass", { info: { name: "Counter" } });

    expect(h.showInformationMessage).toHaveBeenCalledWith("Not connected to a Beamtalk workspace.");
  });

  it("reloadClass reloads the selected class and reports the result", async () => {
    registerAll(makeContext());
    const reload = vi.fn(async () => ({ classes: [{ name: "Counter" }], warnings: ["w1"] }));
    state.workspaceWsClient = { reload } as never;

    await call("beamtalk.reloadClass", { info: { name: "Counter" } });

    expect(reload).toHaveBeenCalledWith("Counter");
    expect(h.showInformationMessage).toHaveBeenCalledWith("Reloaded: Counter (w1)");
  });

  it("reloadClass surfaces a failure as an error message", async () => {
    registerAll(makeContext());
    state.workspaceWsClient = {
      reload: vi.fn(async () => {
        throw new Error("boom");
      }),
    } as never;

    await call("beamtalk.reloadClass", { info: { name: "Counter" } });

    expect(h.showErrorMessage).toHaveBeenCalledWith("Reload failed: boom");
  });

  it("refreshWorkspace and clearTranscript delegate to their providers", () => {
    registerAll(makeContext());
    const refresh = vi
      .spyOn(state.workspaceTreeProvider as WorkspaceTreeDataProvider, "refresh")
      .mockResolvedValue();
    const clear = vi
      .spyOn(state.transcriptViewProvider as TranscriptViewProvider, "clear")
      .mockImplementation(() => {});

    call("beamtalk.refreshWorkspace");
    call("beamtalk.clearTranscript");

    expect(refresh).toHaveBeenCalledTimes(1);
    expect(clear).toHaveBeenCalledTimes(1);
  });
});

describe("navigation commands", () => {
  it("navigateToStateVar asks for a state variable when invoked without one", async () => {
    registerAll(makeContext());

    await call("beamtalk.navigateToStateVar", undefined);

    expect(h.showInformationMessage).toHaveBeenCalledWith(
      "Select a state variable in Workspace Explorer first."
    );
  });

  it("openClassSource reports a class with no source", async () => {
    registerAll(makeContext());

    await call("beamtalk.openClassSource", { info: { name: "Counter" } });

    expect(h.showInformationMessage).toHaveBeenCalledWith("Source not available for this class.");
  });

  it("navigateToTypeAlias does nothing without an alias node", async () => {
    registerAll(makeContext());

    await call("beamtalk.navigateToTypeAlias", undefined);

    expect(h.showInformationMessage).not.toHaveBeenCalled();
  });
});

describe("log commands", () => {
  it("creates the log-level status bar item wired to setLogLevel", () => {
    h.configValues["logs.level"] = "debug";
    registerAll(makeContext());

    expect(state.logLevelStatusBarItem?.command).toBe("beamtalk.setLogLevel");
    expect(state.logLevelStatusBarItem?.text).toBe("$(filter) debug");
  });

  it("setLogLevel writes the picked level to the global configuration", async () => {
    registerAll(makeContext());
    h.showQuickPick.mockResolvedValue({ label: "error" });

    await call("beamtalk.setLogLevel");

    expect(h.configUpdate).toHaveBeenCalledWith("logs.level", "error", 1);
  });

  it("setLogLevel does nothing when the picker is dismissed", async () => {
    registerAll(makeContext());
    h.showQuickPick.mockResolvedValue(undefined);

    await call("beamtalk.setLogLevel");

    expect(h.configUpdate).not.toHaveBeenCalled();
  });

  it("showLogs reveals the log output channel", () => {
    registerAll(makeContext());

    call("beamtalk.showLogs");

    expect(state.logOutputChannel?.show).toHaveBeenCalled();
  });
});

describe("server and REPL commands", () => {
  it("restartServer stops the running client before starting a new one", async () => {
    registerAll(makeContext());
    const stop = vi.fn(async () => {});
    state.client = { stop } as never;
    state.outputChannel = { info: vi.fn(), warn: vi.fn(), error: vi.fn() } as never;

    await call("beamtalk.restartServer");

    expect(stop).toHaveBeenCalled();
    expect(state.client).toBeUndefined();
  });

  it("stopRepl disposes a live REPL terminal and ignores an exited one", () => {
    registerAll(makeContext());
    const dispose = vi.fn();
    state.replTerminal = { exitStatus: undefined, dispose } as never;
    call("beamtalk.stopRepl");
    expect(dispose).toHaveBeenCalledTimes(1);

    const disposeExited = vi.fn();
    state.replTerminal = { exitStatus: { code: 0 }, dispose: disposeExited } as never;
    call("beamtalk.stopRepl");
    expect(disposeExited).not.toHaveBeenCalled();
  });
});

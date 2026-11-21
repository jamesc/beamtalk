// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";

/** Provides preconfigured build and test tasks for Beamtalk projects. */
export class BeamtalkTaskProvider implements vscode.TaskProvider {
  provideTasks(): vscode.Task[] {
    const buildTask = new vscode.Task(
      { type: "beamtalk", task: "build" },
      vscode.TaskScope.Workspace,
      "build",
      "beamtalk",
      new vscode.ShellExecution("beamtalk", ["build", "."]),
      "$beamtalk"
    );
    buildTask.group = vscode.TaskGroup.Build;

    const testTask = new vscode.Task(
      { type: "beamtalk", task: "test" },
      vscode.TaskScope.Workspace,
      "test",
      "beamtalk",
      new vscode.ShellExecution("beamtalk", ["test"]),
      "$beamtalk"
    );
    testTask.group = vscode.TaskGroup.Test;

    return [buildTask, testTask];
  }

  resolveTask(): vscode.Task | undefined {
    return undefined;
  }
}

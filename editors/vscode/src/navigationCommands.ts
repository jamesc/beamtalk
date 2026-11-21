// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import { resolveClassDocument } from "./documentResolution";
import { resolveDeclarationOffset } from "./symbolLookup";
import type {
  ClassItemNode,
  MethodItemNode,
  StateVarItemNode,
  TypeAliasItemNode,
} from "./workspaceTreeView";
import { openAliasSourceDocument, openStdlibDocumentForClass } from "./virtualDocuments";

/** Commands that open class, type-alias, method and state-variable source. */
export function registerNavigationCommands(context: vscode.ExtensionContext): void {
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
}

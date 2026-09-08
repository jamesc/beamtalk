// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import * as vscode from "vscode";
import {
  findClassDeclaration,
  findMethodDeclaration,
  findStateVarDeclaration,
  findTypeAliasDeclaration,
  offsetForDeclarationLine,
} from "./textUtils";

/** The kinds of Beamtalk declarations navigation/hover can resolve via document symbols. */
export type SymbolLookupKind = "class" | "method" | "class-method" | "field" | "type-alias";

/**
 * A declaration to resolve — the same shape every sidebar navigation/hover
 * surface needs, so they can all go through one resolver instead of each
 * reimplementing the "line → regex → document symbols" fallback chain
 * slightly differently (the exact drift that caused several real bugs: one
 * call site had a document-symbol fallback the others lacked, one call
 * site's regex understood `::` typed params before another's did, etc.).
 */
export type DeclarationRef =
  | { kind: "method"; side: "instance" | "class"; selector: string }
  | { kind: "field"; name: string }
  | { kind: "class"; name: string }
  | { kind: "type-alias"; name: string };

function declarationRefName(ref: DeclarationRef): string {
  return ref.kind === "method" ? ref.selector : ref.name;
}

function declarationRefLookupKind(ref: DeclarationRef): SymbolLookupKind {
  if (ref.kind === "method") return ref.side === "class" ? "class-method" : "method";
  return ref.kind;
}

/**
 * Find the position of a class, method, or field symbol from LSP document
 * symbols — the AST-based fallback used when a source-text guess (regex or a
 * stale compiled line number) can't locate a declaration, e.g. syntax the
 * regex doesn't understand yet, or a class predating real-line tracking.
 *
 * Shared by the sidebar hover tooltip (`workspaceTreeView.ts`) and the
 * sidebar goto commands (`extension.ts`) so both degrade to the same
 * authoritative lookup instead of each guessing independently.
 */
export function findSymbolPosition(
  symbols: vscode.DocumentSymbol[] | vscode.SymbolInformation[],
  name: string,
  kind: SymbolLookupKind
): vscode.Position | undefined {
  const targetKind =
    kind === "class"
      ? [vscode.SymbolKind.Class]
      : kind === "field"
        ? [vscode.SymbolKind.Field]
        : kind === "type-alias"
          ? [vscode.SymbolKind.Interface]
          : [vscode.SymbolKind.Method, vscode.SymbolKind.Function];

  for (const sym of symbols) {
    // Class symbols are named "ClassName (class)" per ADR 0013 — strip the suffix for matching.
    const symBaseName =
      sym.kind === vscode.SymbolKind.Class ? sym.name.replace(/ \(class\)$/, "") : sym.name;
    if (targetKind.includes(sym.kind) && symBaseName === name) {
      if ("range" in sym) {
        // DocumentSymbol
        return sym.selectionRange.start;
      } else {
        // SymbolInformation
        return sym.location.range.start;
      }
    }
    // Recurse into children (DocumentSymbol only)
    if ("children" in sym && sym.children.length > 0) {
      const found = findSymbolPosition(sym.children, name, kind);
      if (found) return found;
    }
  }
  return undefined;
}

/**
 * Resolve a declaration's position via the LSP's document-symbol provider —
 * the authoritative, AST-based source of truth. Returns undefined if the LSP
 * is unavailable, the document has no symbols yet, or the symbol isn't found.
 */
export async function resolveViaDocumentSymbols(
  uri: vscode.Uri,
  name: string,
  kind: SymbolLookupKind
): Promise<vscode.Position | undefined> {
  try {
    const docSymbols = await vscode.commands.executeCommand<
      vscode.DocumentSymbol[] | vscode.SymbolInformation[]
    >("vscode.executeDocumentSymbolProvider", uri);
    return findSymbolPosition(docSymbols ?? [], name, kind);
  } catch {
    return undefined;
  }
}

/**
 * Resolve a declaration's offset via source-text search only: a real
 * compiled line number from beamtalk_xref (`declaredLine`, validated against
 * the source so a stale line can't silently mismatch — see
 * `offsetForDeclarationLine`'s doc), then a regex guess appropriate to the
 * declaration kind. No LSP round trip, so this is cheap enough to call
 * synchronously on every sidebar hover.
 *
 * Returns -1 if neither tier locates it — callers fall back further to
 * `resolveDeclarationOffset`'s document-symbol tier, or their own
 * last-resort handling.
 */
export function resolveDeclarationOffsetSync(
  text: string,
  ref: DeclarationRef,
  declaredLine?: number
): number {
  if (ref.kind === "class") {
    return findClassDeclaration(text, ref.name);
  }
  if (ref.kind === "type-alias") {
    return findTypeAliasDeclaration(text, ref.name);
  }
  // The full joined selector never appears verbatim in source when it has
  // keyword parts — validate the real-line path against just its first
  // keyword instead (see offsetForDeclarationLine's doc).
  const needle = ref.kind === "method" ? ref.selector.split(":")[0] : ref.name;
  let offset =
    declaredLine !== undefined ? offsetForDeclarationLine(text, declaredLine, needle) : -1;
  if (offset === -1) {
    offset =
      ref.kind === "method"
        ? findMethodDeclaration(text, ref.selector, ref.side)
        : findStateVarDeclaration(text, ref.name);
  }
  return offset;
}

/**
 * Resolve a declaration's offset in `document`, trying (in order): a real
 * compiled line number, a source-text regex guess, and the LSP's
 * document-symbol provider — the full three-tier chain described on
 * `DeclarationRef`. Returns -1 if none of these locate it.
 *
 * Every goto command in `extension.ts` should resolve declarations through
 * this one function. The sidebar hover tooltip (`workspaceTreeView.ts`)
 * instead calls `resolveDeclarationOffsetSync` and `resolveViaDocumentSymbols`
 * separately, because it needs to retry hover at the document-symbol
 * position even when the sync tiers *did* find an offset but hovering there
 * came up empty — a nuance goto doesn't need, since it only wants one
 * best-effort position.
 */
export async function resolveDeclarationOffset(
  document: vscode.TextDocument,
  ref: DeclarationRef,
  declaredLine?: number
): Promise<number> {
  const text = document.getText();
  let offset = resolveDeclarationOffsetSync(text, ref, declaredLine);
  if (offset === -1) {
    const pos = await resolveViaDocumentSymbols(
      document.uri,
      declarationRefName(ref),
      declarationRefLookupKind(ref)
    );
    if (pos) offset = document.offsetAt(pos);
  }
  return offset;
}

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { beforeEach, describe, expect, it, vi } from "vitest";
// Real `vscode.Uri` implementation, used the same way findMethodDeclaration.test.ts
// uses it: to build a `.path` the way a real `vscode.Uri` produces it, since
// `buildVscodeModule`'s `Uri.parse` now itself delegates to this same package.
import { URI } from "vscode-uri";
import { buildVscodeModule } from "./vscodeMock";

const { executeCommandMock, openTextDocumentMock } = vi.hoisted(() => ({
  executeCommandMock: vi.fn(),
  openTextDocumentMock: vi.fn(),
}));

vi.mock("vscode", () => buildVscodeModule({ executeCommandMock, openTextDocumentMock }));

import {
  aliasSourceContentFor,
  resolveAliasDocument,
  resolveClassDocument,
} from "../documentResolution";
import { aliasSourceUriString } from "../textUtils";
import type { TypeAliasInfo } from "../workspaceClient";

const classInfo = { name: "Account", source_file: "/proj/account.bt", actor_count: 0 };
const stdlibClassInfo = { name: "Actor", source_origin: "stdlib" as const };
const fakeDoc = { getText: () => "" } as unknown as ReturnType<typeof openTextDocumentMock>;

describe("resolveClassDocument", () => {
  beforeEach(() => {
    openTextDocumentMock.mockReset();
  });

  it("opens the real source_file when one is recorded", async () => {
    openTextDocumentMock.mockResolvedValue(fakeDoc);
    const result = await resolveClassDocument(classInfo, null);
    expect(result).toEqual({ kind: "opened", document: fakeDoc });
    expect(openTextDocumentMock).toHaveBeenCalledTimes(1);
  });

  it("reports source-file-error, with the underlying error, when opening the real file throws", async () => {
    openTextDocumentMock.mockRejectedValue(new Error("ENOENT"));
    const result = await resolveClassDocument(classInfo, null);
    expect(result).toEqual({
      kind: "source-file-error",
      sourceFile: "/proj/account.bt",
      error: "ENOENT",
    });
  });

  it("falls back to the stdlib opener when no real source_file is recorded", async () => {
    const stdlibOpener = vi.fn().mockResolvedValue(fakeDoc);
    const result = await resolveClassDocument(stdlibClassInfo, stdlibOpener);
    expect(result).toEqual({ kind: "opened", document: fakeDoc });
    expect(stdlibOpener).toHaveBeenCalledWith(stdlibClassInfo);
    expect(openTextDocumentMock).not.toHaveBeenCalled();
  });

  it("reports no-source when there's no real source_file and no stdlib opener", async () => {
    const result = await resolveClassDocument(stdlibClassInfo, null);
    expect(result).toEqual({ kind: "no-source" });
  });

  it("reports no-source when the stdlib opener itself comes up empty or throws", async () => {
    const emptyOpener = vi.fn().mockResolvedValue(undefined);
    expect(await resolveClassDocument(stdlibClassInfo, emptyOpener)).toEqual({
      kind: "no-source",
    });

    const throwingOpener = vi.fn().mockRejectedValue(new Error("LSP unavailable"));
    expect(await resolveClassDocument(stdlibClassInfo, throwingOpener)).toEqual({
      kind: "no-source",
    });
  });

  it('treats an explicit "unknown" source_file the same as a missing one', async () => {
    const stdlibOpener = vi.fn().mockResolvedValue(fakeDoc);
    const result = await resolveClassDocument(
      { name: "Foo", source_file: "unknown" },
      stdlibOpener
    );
    expect(result).toEqual({ kind: "opened", document: fakeDoc });
    expect(openTextDocumentMock).not.toHaveBeenCalled();
  });
});

// ─── resolveAliasDocument / aliasSourceContentFor ─────────────────────────────
//
// BT-3505: `resolveAliasDocument` and `aliasSourceContentFor` are the code
// extracted out of `openAliasSourceDocument`/`AliasContentProvider` in
// extension.ts so this exact path — the one that shipped two real,
// review-missed bugs (a double-decode `URIError`, then a leading-`//`-path
// `UriError`) — runs through a real `vscode.Uri.parse` (via `buildVscodeModule`'s
// `Uri.parse`, itself backed by the real `vscode-uri` package) in tests,
// instead of only the pure `aliasSourceUriString`/`parseAliasSourceUriPath`
// string functions being tested in isolation of any actual URI object.

const projectAlias: TypeAliasInfo = {
  name: "Timeout",
  expansion: "Integer | #infinity",
  source_file: "src/timeout.bt",
  package: "my_app",
  source_origin: "project",
};

describe("resolveAliasDocument", () => {
  beforeEach(() => {
    openTextDocumentMock.mockReset();
  });

  it("opens the document when real content exists for a populated package", async () => {
    openTextDocumentMock.mockResolvedValue(fakeDoc);
    const fetchAliasSource = vi.fn().mockResolvedValue({ content: "type Timeout = ..." });
    const result = await resolveAliasDocument(projectAlias, fetchAliasSource);
    expect(result).toEqual({ kind: "opened", document: fakeDoc });
    expect(fetchAliasSource).toHaveBeenCalledWith("Timeout", "my_app");
  });

  // BT-3505 regression, at the level that actually shipped the bug: an alias
  // row with no `package` (the pre-BT-3496 backward-compat fallback path in
  // `_hasNavigableAliasSource` reaches this with `package` undefined even
  // against a current server). Before the fix this built a
  // `beamtalk-alias:////Timeout.bt` URI, whose `.path` starts with `//` —
  // `vscode.Uri.parse` throws for that, which the outer `try/catch` here
  // swallowed into a silent `{ kind: "no-source" }`, i.e. exactly the "Go to
  // Definition says source not available" symptom reported even though real
  // content existed. Asserting `kind: "opened"` (not just "didn't throw")
  // is what actually distinguishes the fix from the swallowed-throw bug,
  // since both looked identical from outside the try/catch.
  it("opens the document when the package is undefined (BT-3505 regression)", async () => {
    openTextDocumentMock.mockResolvedValue(fakeDoc);
    const fetchAliasSource = vi.fn().mockResolvedValue({ content: "type Timeout = ..." });
    const result = await resolveAliasDocument(
      { name: "Timeout", package: undefined },
      fetchAliasSource
    );
    expect(result).toEqual({ kind: "opened", document: fakeDoc });
    expect(openTextDocumentMock).toHaveBeenCalledTimes(1);
  });

  it("reports no-source when content is null (stdlib/dependency origin, or a stale project file)", async () => {
    const fetchAliasSource = vi.fn().mockResolvedValue({ content: null });
    const result = await resolveAliasDocument(projectAlias, fetchAliasSource);
    expect(result).toEqual({ kind: "no-source" });
    expect(openTextDocumentMock).not.toHaveBeenCalled();
  });

  it("reports no-source when the existence-check fetch throws", async () => {
    const fetchAliasSource = vi.fn().mockRejectedValue(new Error("disconnected"));
    const result = await resolveAliasDocument(projectAlias, fetchAliasSource);
    expect(result).toEqual({ kind: "no-source" });
    expect(openTextDocumentMock).not.toHaveBeenCalled();
  });

  it("reports no-source when openTextDocument itself throws", async () => {
    openTextDocumentMock.mockRejectedValue(new Error("boom"));
    const fetchAliasSource = vi.fn().mockResolvedValue({ content: "type Timeout = ..." });
    const result = await resolveAliasDocument(projectAlias, fetchAliasSource);
    expect(result).toEqual({ kind: "no-source" });
  });

  it("passes a percent-encoded name/package through to openTextDocument without throwing", async () => {
    openTextDocumentMock.mockResolvedValue(fakeDoc);
    const fetchAliasSource = vi.fn().mockResolvedValue({ content: "..." });
    const result = await resolveAliasDocument(
      { name: "50% Done", package: "100% Coverage" },
      fetchAliasSource
    );
    expect(result).toEqual({ kind: "opened", document: fakeDoc });
  });
});

describe("aliasSourceContentFor", () => {
  it("returns a placeholder when there is no live workspace connection", async () => {
    const uri = URI.parse(aliasSourceUriString("Timeout", "my_app"));
    const content = await aliasSourceContentFor(uri, null);
    expect(content).toContain("Not connected to a Beamtalk workspace.");
  });

  it("returns the real content when the fetch succeeds", async () => {
    const uri = URI.parse(aliasSourceUriString("Timeout", "my_app"));
    const fetchAliasSource = vi.fn().mockResolvedValue({ content: "type Timeout = Integer\n" });
    const content = await aliasSourceContentFor(uri, fetchAliasSource);
    expect(content).toBe("type Timeout = Integer\n");
    expect(fetchAliasSource).toHaveBeenCalledWith("Timeout", "my_app");
  });

  it("returns a placeholder mentioning the name when content is null", async () => {
    const uri = URI.parse(aliasSourceUriString("Timeout", "my_app"));
    const fetchAliasSource = vi.fn().mockResolvedValue({ content: null });
    const content = await aliasSourceContentFor(uri, fetchAliasSource);
    expect(content).toContain("Source not available for type alias `Timeout`");
  });

  it("returns the underlying error message when the fetch throws", async () => {
    const uri = URI.parse(aliasSourceUriString("Timeout", "my_app"));
    const fetchAliasSource = vi.fn().mockRejectedValue(new Error("workspace unreachable"));
    const content = await aliasSourceContentFor(uri, fetchAliasSource);
    expect(content).toContain("Failed to load type alias source for `Timeout`");
    expect(content).toContain("workspace unreachable");
  });

  it("decodes an unknown (undefined) package back to undefined, not the bare marker string", async () => {
    const uri = URI.parse(aliasSourceUriString("Timeout", undefined));
    const fetchAliasSource = vi.fn().mockResolvedValue({ content: "..." });
    await aliasSourceContentFor(uri, fetchAliasSource);
    expect(fetchAliasSource).toHaveBeenCalledWith("Timeout", undefined);
  });

  // Double-decode regression (found in BT-3496 review): parseAliasSourceUriPath
  // previously called decodeURIComponent a second time on top of the real
  // vscode.Uri's already-decoded `.path`, throwing `URIError: URI malformed`
  // for any name/package containing a literal `%`.
  it("decodes a literal '%' in the name/package without throwing (double-decode regression)", async () => {
    const uri = URI.parse(aliasSourceUriString("50% Done", "100% Coverage"));
    const fetchAliasSource = vi.fn().mockResolvedValue({ content: "..." });
    const content = await aliasSourceContentFor(uri, fetchAliasSource);
    expect(content).toBe("...");
    expect(fetchAliasSource).toHaveBeenCalledWith("50% Done", "100% Coverage");
  });
});

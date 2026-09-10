// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { beforeEach, describe, expect, it, vi } from "vitest";
import { buildVscodeModule } from "./vscodeMock";

const { executeCommandMock, openTextDocumentMock } = vi.hoisted(() => ({
  executeCommandMock: vi.fn(),
  openTextDocumentMock: vi.fn(),
}));

vi.mock("vscode", () => buildVscodeModule({ executeCommandMock, openTextDocumentMock }));

import { resolveClassDocument } from "../documentResolution";

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

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { describe, expect, it } from "vitest";
import { GenerationTracker } from "../generationTracker";

describe("GenerationTracker", () => {
  it("a token is current until its key is bumped", () => {
    const t = new GenerationTracker();
    const token = t.token("a");
    expect(t.isCurrent("a", token)).toBe(true);
    t.bump("a");
    expect(t.isCurrent("a", token)).toBe(false);
  });

  it("bumping one key does not invalidate a different key's token", () => {
    const t = new GenerationTracker();
    const tokenA = t.token("a");
    const tokenB = t.token("b");
    t.bump("a");
    expect(t.isCurrent("a", tokenA)).toBe(false);
    expect(t.isCurrent("b", tokenB)).toBe(true);
  });

  it("bumpAll invalidates every key's token, including keys never bumped individually", () => {
    const t = new GenerationTracker();
    const tokenA = t.token("a");
    const tokenB = t.token("b");
    t.bumpAll();
    expect(t.isCurrent("a", tokenA)).toBe(false);
    expect(t.isCurrent("b", tokenB)).toBe(false);
  });

  it("a token captured after a bump is current until the next bump", () => {
    const t = new GenerationTracker();
    t.bump("a");
    const token = t.token("a");
    expect(t.isCurrent("a", token)).toBe(true);
    t.bump("a");
    expect(t.isCurrent("a", token)).toBe(false);
  });

  it("a token captured after bumpAll is current until the next invalidation", () => {
    const t = new GenerationTracker();
    t.bumpAll();
    const token = t.token("a");
    expect(t.isCurrent("a", token)).toBe(true);
    t.bump("a");
    expect(t.isCurrent("a", token)).toBe(false);
  });

  it("a never-seen key starts current with an empty token", () => {
    const t = new GenerationTracker();
    expect(t.isCurrent("nonexistent", { epoch: 0, keyGen: 0 })).toBe(true);
  });
});

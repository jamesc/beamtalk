// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { TranscriptViewProvider } from "../transcriptView";
import type { PushEvent, WorkspaceClient } from "../workspaceClient";

function setup(options?: { fullReload?: boolean }) {
  const provider = new TranscriptViewProvider(options);
  let htmlWrites = 0;
  const posted: Array<{ type: string; text: string }> = [];
  let html = "";
  const webview = {
    options: {},
    get html() {
      return html;
    },
    set html(v: string) {
      html = v;
      htmlWrites++;
    },
    postMessage: (m: { type: string; text: string }) => {
      posted.push(m);
      return Promise.resolve(true);
    },
  };
  let visibleHandler: () => void = () => {};
  const view = {
    webview,
    visible: true,
    onDidDispose: () => ({ dispose() {} }),
    onDidChangeVisibility: (h: () => void) => {
      visibleHandler = h;
      return { dispose() {} };
    },
  };
  provider.resolveWebviewView(view as never, {} as never, {} as never);
  let push: (e: PushEvent) => void = () => {};
  provider.setClient({
    onPush: (h: (e: PushEvent) => void) => {
      push = h;
      return () => {};
    },
  } as unknown as WorkspaceClient);
  const emit = (text: string) => push({ channel: "transcript", text } as PushEvent);
  const setVisible = (v: boolean) => {
    view.visible = v;
    visibleHandler();
  };
  return { provider, emit, posted, setVisible, htmlWrites: () => htmlWrites, html: () => html };
}

describe("TranscriptViewProvider (BT-5082)", () => {
  beforeEach(() => vi.useFakeTimers());
  afterEach(() => vi.useRealTimers());

  it("a burst of 1000 lines causes one append message and no page reload", () => {
    const t = setup();
    expect(t.htmlWrites()).toBe(1); // initial render only
    for (let i = 0; i < 1000; i++) t.emit(`line ${i}\n`);
    vi.advanceTimersByTime(100);

    expect(t.htmlWrites()).toBe(1);
    expect(t.posted).toHaveLength(1);
    expect(t.posted[0].type).toBe("append");
    expect(t.posted[0].text.split("\n").length - 1).toBe(1000);
  });

  it("only sends text received since the last flush", () => {
    const t = setup();
    t.emit("a\n");
    vi.advanceTimersByTime(100);
    t.emit("b\n");
    vi.advanceTimersByTime(100);
    expect(t.posted.map((m) => m.text)).toEqual(["a\n", "b\n"]);
  });

  it("replaces the page content once the retained buffer is trimmed", () => {
    const t = setup();
    t.emit("x\n".repeat(10_001));
    vi.advanceTimersByTime(100);
    expect(t.posted).toHaveLength(1);
    expect(t.posted[0].type).toBe("replace");
    expect(t.posted[0].text.split("\n").length - 1).toBeLessThan(10_000);
  });

  it("keeps the full-reload path when fullReload is set (WSL)", () => {
    const t = setup({ fullReload: true });
    t.emit("hello\n");
    vi.advanceTimersByTime(100);
    expect(t.posted).toHaveLength(0);
    expect(t.htmlWrites()).toBe(2);
    expect(t.html()).toContain("hello");
  });

  it("does not lose output printed while the view is hidden", () => {
    const t = setup();
    t.setVisible(false);
    t.emit("while hidden\n");
    vi.advanceTimersByTime(100);
    expect(t.posted).toHaveLength(0);

    t.setVisible(true);
    expect(t.html()).toContain("while hidden");

    // Later output appends normally and is not duplicated.
    t.emit("after\n");
    vi.advanceTimersByTime(100);
    expect(t.posted.map((m) => m.text)).toEqual(["after\n"]);
  });

  it("does not accumulate pending text when no view is attached", () => {
    const provider = new TranscriptViewProvider();
    let push: (e: PushEvent) => void = () => {};
    provider.setClient({
      onPush: (h: (e: PushEvent) => void) => {
        push = h;
        return () => {};
      },
    } as unknown as WorkspaceClient);
    push({ channel: "transcript", text: "x\n" } as PushEvent);
    expect((provider as unknown as { pendingText: string }).pendingText).toBe("");
  });
});

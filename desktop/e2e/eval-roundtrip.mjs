#!/usr/bin/env node
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// BT-2989: drive a real Chromium (via Playwright) against a running
// bt_attach front's own page — the same content a desktop-app workspace
// window loads (ADR 0097 Decision: "a window loads
// http://localhost:<port>") — to confirm an eval genuinely round-trips
// through the connected LiveView UI, not just that the workspace RPC layer
// works. See desktop/e2e/README.md for why this is a standalone script
// rather than a Rust test (needs a real browser) or an ExUnit/PhoenixTest
// case (needs a URL this script's caller spawned outside any Mix-managed
// test server, via the desktop broker's own spawn contract).
//
// Reuses the `playwright` devDependency editors/liveview/assets/package.json
// already declares for editors/liveview's own browser e2e suite
// (workspace_browser_test.exs) — run this script with that directory as the
// working directory (or otherwise on a NODE_PATH that resolves it) so
// `import { chromium } from "playwright"` finds the already-installed
// package instead of needing a second, redundant install.
//
// Usage:
//   node eval-roundtrip.mjs <front-url> <expr-source> <expected-substring>
//
// Exits 0 and prints "OK: ..." on success; exits 1 and prints "FAIL: ..." to
// stderr (with the specific step that failed) otherwise. Every wait below is
// bounded — this script should never hang past its own timeouts, mirroring
// the "surfaces failures rather than hanging" acceptance criterion the
// caller's negative path (a separate, non-browser check in attach-cycle.sh)
// also exercises.

import { chromium } from "playwright";

const [, , url, exprSource, expectedSubstring] = process.argv;

if (!url || !exprSource || !expectedSubstring) {
  console.error(
    "usage: eval-roundtrip.mjs <front-url> <expr-source> <expected-substring>",
  );
  process.exit(2);
}

const NAV_TIMEOUT_MS = 15_000;
const ATTACH_TIMEOUT_MS = 15_000;
const EVAL_TIMEOUT_MS = 10_000;

async function main() {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage();
    await page.goto(url, { timeout: NAV_TIMEOUT_MS, waitUntil: "load" });

    // The connected-only render (`.att-label` text "attached") appears only
    // once the LiveSocket has mounted AND the front's lazy
    // ensure_distributed/0 + workspace attach have both succeeded — real
    // proof the "window loaded" in the sense BT-2989's acceptance criterion
    // means, not merely that Phoenix answered an HTTP GET.
    await page
      .locator(".att-label", { hasText: "attached" })
      .waitFor({ timeout: ATTACH_TIMEOUT_MS });

    // The same CmEditor doc-replace transaction
    // editors/liveview/test/bt_attach_web/workspace_browser_test.exs's
    // `set_cm_source/3` helper uses: dispatch through the real CodeMirror
    // view (`el.cmView`) so the hidden form field mirrors it exactly like
    // real typing would, rather than poking `.value` on a plain textarea
    // (there isn't one — CmEditor is CodeMirror 6, not a <textarea>).
    await page.evaluate((source) => {
      const el = document.querySelector(
        "#workspace-editor-overlay .cm-content",
      );
      if (!el || !el.cmView) {
        throw new Error(
          "CmEditor (cmView) never mounted on #workspace-editor-overlay",
        );
      }
      el.cmView.dispatch({
        changes: { from: 0, to: el.cmView.state.doc.length, insert: source },
      });
    }, exprSource);

    // "Print it": submits the eval form over the live socket; the workspace
    // evaluates and the result flashes in the transient status line.
    await page.click("button[value='print_it']");

    await page
      .locator(".eval-status .val", { hasText: expectedSubstring })
      .waitFor({ timeout: EVAL_TIMEOUT_MS });

    console.log(
      `OK: eval of ${JSON.stringify(exprSource)} round-tripped through the LiveView UI (got ${JSON.stringify(expectedSubstring)})`,
    );
  } finally {
    await browser.close();
  }
}

main().catch((err) => {
  console.error(`FAIL: ${err.message}`);
  process.exit(1);
});

// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Builds the unpacked extension into dist/. The grammar and icon are copied
// from the VS Code extension at build time so there is one source of truth.

import { copyFileSync, mkdirSync, rmSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { build } from "esbuild";

const here = dirname(fileURLToPath(import.meta.url));
const dist = join(here, "dist");
const require = createRequire(import.meta.url);

rmSync(dist, { recursive: true, force: true });
mkdirSync(dist, { recursive: true });

await build({
  entryPoints: {
    background: join(here, "src/background.js"),
    content: join(here, "src/content.js"),
  },
  outdir: dist,
  bundle: true,
  format: "iife",
  target: "chrome105",
  logLevel: "warning",
});

const copies = [
  [join(here, "manifest.json"), "manifest.json"],
  [join(here, "src/content.css"), "content.css"],
  [require.resolve("vscode-oniguruma/release/onig.wasm"), "onig.wasm"],
  [join(here, "../vscode/syntaxes/beamtalk.tmLanguage.json"), "beamtalk.tmLanguage.json"],
  [join(here, "../vscode/images/icon.png"), "icon.png"],
];
for (const [from, to] of copies) copyFileSync(from, join(dist, to));

console.log(`Built unpacked extension in ${dist}`);

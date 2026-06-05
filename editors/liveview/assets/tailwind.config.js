// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// Tailwind config for the Beamtalk LiveView app, run via the standalone
// `tailwind` binary (no npm). The standalone CLI bundles the core plugins, so
// this config intentionally avoids npm-only plugins (e.g. @tailwindcss/forms)
// and the deps/heroicons SVG loader to keep the build dependency-free.
module.exports = {
  content: [
    "./js/**/*.js",
    "../lib/bt_attach_web.ex",
    "../lib/bt_attach_web/**/*.*ex",
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}

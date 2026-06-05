// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// The Beamtalk LiveView client. Bundled by esbuild (see config/config.exs).
//
// `phoenix`, `phoenix_html`, and `phoenix_live_view` are resolved from the Mix
// deps directory via NODE_PATH (set in the esbuild config), so there is no npm
// install step — the JS ships inside the hex packages.
import "phoenix_html"
import { Socket } from "phoenix"
import { LiveSocket } from "phoenix_live_view"

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content")

const liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken },
})

// Connect if there are any LiveViews on the page.
liveSocket.connect()

// Expose liveSocket on window for web console debug logs and latency simulator:
//   >> liveSocket.enableDebug()
//   >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
//   >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket

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

// Per-TAB workspace token (BT-2410 Wave 4). Stored in `sessionStorage`, which is
// scoped to a single browser tab and survives a page reload / socket reconnect
// of *that* tab — but is NOT shared between tabs. So:
//   * two tabs  → two distinct tokens → two isolated workspace sessions;
//   * reload / reconnect of one tab → same token → resume that tab's session.
// The token rides on the LiveSocket `params`, so the server reads it back on
// every (re)connect via `get_connect_params/1` and re-binds to the live session.
function tabToken() {
  const key = "bt_workspace_token"
  let token = window.sessionStorage.getItem(key)
  if (!token) {
    token =
      window.crypto && window.crypto.randomUUID
        ? window.crypto.randomUUID()
        : `tab-${Date.now()}-${Math.random().toString(36).slice(2)}`
    window.sessionStorage.setItem(key, token)
  }
  return token
}

const liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken, workspace_token: tabToken() },
})

// Connect if there are any LiveViews on the page.
liveSocket.connect()

// Expose liveSocket on window for web console debug logs and latency simulator:
//   >> liveSocket.enableDebug()
//   >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
//   >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket

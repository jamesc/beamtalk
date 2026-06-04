// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
// BT-2394 Attach spike — plain (non-bundled) LiveView client.
// phoenix.min.js and phoenix_live_view.min.js are loaded first as classic
// scripts, exposing the `Phoenix` and `LiveView` globals.
(function () {
  var csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content");
  var liveSocket = new LiveView.LiveSocket("/live", Phoenix.Socket, {
    params: { _csrf_token: csrfToken },
  });
  liveSocket.connect();
  window.liveSocket = liveSocket;
})();

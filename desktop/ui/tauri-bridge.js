// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// The picker's two IPC calls, `invoke` and `listen` (BT-5194).
//
// `tauri.conf.json` no longer sets `withGlobalTauri`: that setting is
// app-wide in Tauri 2, so it also injected the `window.__TAURI__` API into
// every workspace window, which shows workspace and user content. Tauri
// itself injects `window.__TAURI_INTERNALS__` into every webview whatever
// the setting, so the capability ACL (`capabilities/default.json`, scoped to
// the picker window) is the real boundary; dropping the global API keeps the
// workspace windows from getting a convenience surface on top of it.
//
// These two functions do exactly what `@tauri-apps/api`'s `core.invoke` and
// `event.listen` do (the bundle `withGlobalTauri` used to inject), over the
// same `__TAURI_INTERNALS__` calls.
(function installPickerIpc(global) {
  const internals = global.__TAURI_INTERNALS__;

  function invoke(cmd, args = {}) {
    return internals.invoke(cmd, args);
  }

  // Resolves to an unlisten function, like `@tauri-apps/api`'s `listen`.
  function listen(event, handler) {
    return invoke("plugin:event|listen", {
      event,
      target: { kind: "Any" },
      handler: internals.transformCallback(handler),
    }).then((eventId) => async () => {
      global.__TAURI_EVENT_PLUGIN_INTERNALS__?.unregisterListener(
        event,
        eventId,
      );
      await invoke("plugin:event|unlisten", { event, eventId });
    });
  }

  global.pickerIpc = Object.freeze({ invoke, listen });
})(globalThis);

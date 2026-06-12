import Config

# The browser e2e suite (Playwright, see test/bt_attach_web/workspace_browser_test.exs)
# drives a REAL Chromium over HTTP, so the endpoint must actually be listening —
# but only then. A bare `mix test` keeps its no-server speed; the port is bound
# only when the browser suite is selected (PHX_PLAYWRIGHT=1). The two suites never
# run in the same `mix test` invocation, so there is no port-binding overlap.
browser_e2e? = System.get_env("PHX_PLAYWRIGHT") not in [nil, ""]

config :bt_attach, BtAttachWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "oTKE0c+KLWtXcW8/EDCknzsXsZfcDNgW8SklcUazRhVvrWwnkSWsIQpCPwYmLSD1",
  server: browser_e2e?

# PhoenixTest + its Playwright driver: run the same test API against a real
# browser so the connected-only JS hooks (CodeEditor, KeyboardShortcuts,
# SelectionTracker, TweaksPanel) actually execute — they are invisible to
# LiveViewTest's server-side floki render. The Node Playwright CLI lives under
# assets/node_modules (installed by the e2e CI job via `npm --prefix assets install`).
config :phoenix_test,
  otp_app: :bt_attach,
  endpoint: BtAttachWeb.Endpoint,
  playwright: [
    browser: :chromium,
    headless: System.get_env("PLAYWRIGHT_HEADLESS", "true") != "false",
    # The 4s default is too tight for a cold Chromium spawn on slower dev
    # machines (e.g. WSL2); CI launches well under either value. The pool must
    # be declared explicitly: the global key only merges into browser_pools
    # entries that are present in config (the implicit default pool keeps the
    # 4s default — phoenix_test_playwright 0.14.0 Config.global/1).
    browser_launch_timeout: 30_000,
    browser_pools: [[id: :default_pool, browser_launch_timeout: 30_000]]
  ]

# Print only warnings and errors during test
config :logger, level: :warning

# A short session-resume grace window for the workspace-gated reconnect tests
# (BT-2410 Wave 4): long enough to bridge a same-tab reconnect within a test, but
# short enough that the teardown-on-disconnect assertion does not stall waiting
# for a reap. Production uses the 30s default in `BtAttach.SessionRegistry`.
config :bt_attach, :session_reap_after_ms, 300

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Enable helpful, but potentially expensive runtime checks
config :phoenix_live_view,
  enable_expensive_runtime_checks: true

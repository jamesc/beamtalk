import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :bt_attach, BtAttachWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "oTKE0c+KLWtXcW8/EDCknzsXsZfcDNgW8SklcUazRhVvrWwnkSWsIQpCPwYmLSD1",
  server: false

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

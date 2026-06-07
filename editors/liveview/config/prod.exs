import Config

# Do not print debug messages in production
config :logger, level: :info

# Mark the session (and OIDC handshake) cookies `Secure` in production — they
# gate an RCE-bearing tool and must only travel over HTTPS (ADR 0091, BT-2419).
# Dev/test default to false (compile_env default) so plain-HTTP local runs and
# the test conn still carry the cookie.
config :bt_attach, :secure_session, true

# Runtime production configuration, including reading
# of environment variables, is done on config/runtime.exs.

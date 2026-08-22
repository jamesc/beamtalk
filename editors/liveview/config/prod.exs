import Config

# Do not print debug messages in production
config :logger, level: :info

# The session cookies' `Secure` flag is resolved at RUNTIME in
# config/runtime.exs from the deployment posture (PHX_HOST set → TLS →
# Secure), because the same release binary also serves the desktop/local
# mode on plain http://localhost, where WKWebView drops Secure cookies
# (BT-3233). Do not reintroduce a compile-time `:secure_session` here.

# Runtime production configuration, including reading
# of environment variables, is done on config/runtime.exs.

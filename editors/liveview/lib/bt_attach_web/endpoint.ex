defmodule BtAttachWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :bt_attach

  # The authenticated session cookie (ADR 0091 Decision 1 / BT-2419). It gates an
  # RCE-bearing tool, so it is `HttpOnly` (cookie store default), `SameSite=Strict`
  # — not Lax — and `Secure` on TLS deployments. The one flow that genuinely needs
  # a cross-site top-level redirect to carry state (browser → IdP → /oidc/callback)
  # does NOT rely on this cookie: the OIDC handshake state lives in a separate
  # `SameSite=Lax`, `path=/oidc`, short-lived encrypted cookie scoped to that
  # handler (`BtAttachWeb.OidcHandshake`), so the main session stays Strict.
  @session_options_base [
    store: :cookie,
    key: "_bt_attach_key",
    signing_salt: "f7iEj/hj",
    same_site: "Strict"
  ]

  # `:secure` must be resolved at RUNTIME (config/runtime.exs keys it off the
  # deployment posture), not compile time: the same release binary serves both
  # TLS remote deployments and the desktop/local-trial mode on plain
  # http://localhost. WKWebView (the Tauri desktop webview) silently drops
  # `Secure` cookies over plain http — it has no localhost exception like
  # Chrome/Firefox — so a compile-time `Secure` flag left every desktop
  # LiveView join session-less: refused as "stale", full-page reload, an
  # infinite reload loop (BT-3233).
  def session_options do
    Keyword.put(
      @session_options_base,
      :secure,
      Application.get_env(:bt_attach, :secure_session, false)
    )
  end

  # :peer_data is required by BtAttachWeb.Auth.dev_connect_ok?/1, which gates
  # dev-auth LiveView sockets to loopback. Without it, get_connect_info(socket,
  # :peer_data) is always nil on every connected mount, so dev-auth always
  # halts+redirects — an infinite full-page reload loop (BT-3228).
  socket "/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [:peer_data, session: {__MODULE__, :session_options, []}]],
    longpoll: [connect_info: [:peer_data, session: {__MODULE__, :session_options, []}]]

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phx.digest
  # when deploying your static files in production.
  plug Plug.Static,
    at: "/",
    from: :bt_attach,
    gzip: false,
    only: BtAttachWeb.static_paths()

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug :session
  plug BtAttachWeb.Router

  # Function plug so `session_options/0` (and its runtime `:secure` flag) is
  # re-read per request instead of frozen by a compile-time `Plug.Session` init.
  defp session(conn, _opts), do: Plug.Session.call(conn, Plug.Session.init(session_options()))
end

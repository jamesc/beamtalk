defmodule BtAttachWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :bt_attach

  # The authenticated session cookie (ADR 0091 Decision 1 / BT-2419). It gates an
  # RCE-bearing tool, so it is `HttpOnly` (cookie store default), `SameSite=Strict`
  # — not Lax — and `Secure` outside dev/test. The one flow that genuinely needs a
  # cross-site top-level redirect to carry state (browser → IdP → /oidc/callback)
  # does NOT rely on this cookie: the OIDC handshake state lives in a separate
  # `SameSite=Lax`, `path=/oidc`, short-lived encrypted cookie scoped to that
  # handler (`BtAttachWeb.OidcHandshake`), so the main session stays Strict.
  @session_options [
    store: :cookie,
    key: "_bt_attach_key",
    signing_salt: "f7iEj/hj",
    same_site: "Strict",
    secure: Application.compile_env(:bt_attach, :secure_session, false)
  ]

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [session: @session_options]],
    longpoll: [connect_info: [session: @session_options]]

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
  plug Plug.Session, @session_options
  plug BtAttachWeb.Router
end

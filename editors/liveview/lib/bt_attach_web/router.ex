defmodule BtAttachWeb.Router do
  use BtAttachWeb, :router

  import BtAttachWeb.Auth, only: [fetch_current_user: 2, require_authenticated: 2]

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {BtAttachWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    # Load the OIDC session user (ADR 0091 Decision 1) so downstream plugs and
    # LiveView mounts can see `current_user`.
    plug :fetch_current_user
  end

  # Gate: when OIDC is enabled, redirect an unauthenticated request to the IdP.
  # A pass-through when OIDC is disabled (zero-config localhost story).
  pipeline :require_auth do
    plug :require_authenticated
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :readiness do
    plug :accepts, ["json"]
    plug :fetch_session
    # Load the OIDC session user so :require_auth below can gate on it, same
    # as the :browser pipeline.
    plug :fetch_current_user
  end

  # Desktop-attach health check (ADR 0097 Implementation §1c): the broker
  # polls this *before* opening a window, when there is no browser session
  # yet — `:require_auth` is a documented pass-through when OIDC is disabled
  # (the broker's own posture: it refuses to spawn a front with OIDC config
  # present), so this is a no-op for the intended caller. It must NOT be
  # unconditionally public, though: this same `bt_attach` release also serves
  # OIDC-authenticated remote deployments (ADR 0091), where this endpoint
  # would otherwise let any unauthenticated internet client force a dist
  # `connect/0` + RPC to the workspace and read back its version report on
  # every hit. `:require_auth` closes that: unauthenticated + OIDC-enabled
  # gets redirected to the IdP (never reaches the controller), exactly like
  # the IDE route below. No CSRF plug — GET-only, not a form submission.
  scope "/", BtAttachWeb do
    pipe_through [:readiness, :require_auth]

    get "/readiness", ReadinessController, :show
  end

  # OIDC login + callback. These must NOT require an authenticated session
  # (they are how you get one). The callback is the only route that legitimately
  # receives a cross-site top-level redirect from the IdP (SameSite handling is
  # tightened in BT-2419).
  scope "/oidc", BtAttachWeb do
    pipe_through :browser

    get "/auth", OidcController, :auth
    get "/callback", OidcController, :callback
  end

  # The IDE itself: gated behind authentication when OIDC is enabled. The
  # `live_session` runs `Auth.on_mount/4` so the socket inherits the same gate
  # on (re)connect, not just the initial HTTP render.
  scope "/", BtAttachWeb do
    pipe_through [:browser, :require_auth]

    live_session :require_authenticated,
      on_mount: [{BtAttachWeb.Auth, :require_authenticated}] do
      live "/", WorkspaceLive
    end
  end
end

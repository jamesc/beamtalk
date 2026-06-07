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

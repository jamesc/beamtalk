# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Auth do
  @moduledoc """
  Session-bound authentication for the LiveView IDE front (ADR 0091 Decision 1).

  The browser authenticates to Phoenix via OIDC; on success the user's verified
  claims are stored in a **server-side session** (the signed session cookie —
  `HttpOnly`, no token in the URL) under the `"bt_user"` key. This module is the
  single place that reads/writes that session entry and gates access:

    * `fetch_current_user/2` (plug) — load the session user into
      `conn.assigns.current_user` (`nil` when absent).
    * `require_authenticated/2` (plug) — when OIDC is enabled, redirect an
      unauthenticated request to the login route (302 → IdP); a no-op when OIDC
      is disabled (the unchanged zero-config localhost story, ADR 0020).
    * `on_mount/4` — the LiveView equivalent, run in a `live_session` so the
      socket inherits the same gate on connect (session-lifecycle re-validation
      is layered on in BT-2419).
    * `log_in/2` / `log_out/1` — mint / clear the session entry.

  Whether OIDC is enforced is driven entirely by the boot-resolved config
  (`Application.get_env(:bt_attach, :oidc)`): a non-`nil` config means enabled.
  """

  use BtAttachWeb, :verified_routes

  import Plug.Conn
  import Phoenix.Controller, only: [redirect: 2]

  @session_key "bt_user"

  @doc "True when OIDC auth is configured/enforced (boot-resolved, ADR 0091)."
  @spec oidc_enabled?() :: boolean()
  def oidc_enabled?, do: Application.get_env(:bt_attach, :oidc) != nil

  @doc "The boot-resolved OIDC config map, or `nil` when disabled."
  @spec oidc_config() :: BtAttach.IdeConfig.t() | nil
  def oidc_config, do: Application.get_env(:bt_attach, :oidc)

  # ── plugs ───────────────────────────────────────────────────────────────────

  @doc "Plug: assign `:current_user` from the session (or `nil`)."
  def fetch_current_user(conn, _opts) do
    assign(conn, :current_user, get_session(conn, @session_key))
  end

  @doc """
  Plug: require an authenticated session when OIDC is enabled.

  An unauthenticated request is redirected to the login route (which 302s on to
  the IdP). The originally-requested path is stashed so the callback can return
  the user there. When OIDC is disabled this is a pass-through.
  """
  def require_authenticated(conn, _opts) do
    cond do
      not oidc_enabled?() ->
        conn

      conn.assigns[:current_user] ->
        conn

      true ->
        conn
        |> maybe_store_return_to()
        |> redirect(to: ~p"/oidc/auth")
        |> halt()
    end
  end

  defp maybe_store_return_to(%{method: "GET", request_path: path} = conn) do
    put_session(conn, :user_return_to, path)
  end

  defp maybe_store_return_to(conn), do: conn

  # ── LiveView on_mount ─────────────────────────────────────────────────────────

  @doc """
  `on_mount` hook for a `live_session`: assign `:current_user` from the session
  and, when OIDC is enabled, halt+redirect an unauthenticated mount.
  """
  def on_mount(:require_authenticated, _params, session, socket) do
    user = session[@session_key]

    cond do
      not oidc_enabled?() ->
        {:cont, Phoenix.Component.assign(socket, :current_user, user)}

      user ->
        {:cont, Phoenix.Component.assign(socket, :current_user, user)}

      true ->
        {:halt, Phoenix.LiveView.redirect(socket, to: ~p"/oidc/auth")}
    end
  end

  # ── session minting ───────────────────────────────────────────────────────────

  @doc """
  Store the authenticated user's claims in a fresh server-side session.

  Renews the session id (`renew_session`) on login to defend against session
  fixation, then writes the claims under the session key. The claims map is the
  verified OIDC identity (`"sub"`, `"email"`, the groups claim, …) — consumed by
  RBAC (BT-2421). No URL token is ever minted.
  """
  def log_in(conn, claims) when is_map(claims) do
    return_to = get_session(conn, :user_return_to)

    conn
    |> renew_session()
    |> put_session(@session_key, claims)
    |> assign(:current_user, claims)
    |> redirect(to: return_to || ~p"/")
  end

  @doc "Drop the authenticated session (used by logout / lifecycle teardown)."
  def log_out(conn) do
    conn
    |> renew_session()
    |> redirect(to: ~p"/")
  end

  # Clear the session but preserve nothing — a clean slate on login/logout.
  defp renew_session(conn) do
    conn
    |> configure_session(renew: true)
    |> clear_session()
  end
end

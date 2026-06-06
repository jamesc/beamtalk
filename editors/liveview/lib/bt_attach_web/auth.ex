# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Auth do
  @moduledoc """
  Session-bound authentication + lifecycle for the LiveView IDE front
  (ADR 0091 Decision 1; BT-2417 flow, BT-2419 hardening).

  The browser authenticates to Phoenix via OIDC; on success the user's verified
  claims are stored in a **server-side session** (the signed, `SameSite=Strict`,
  `HttpOnly` cookie — no token in the URL) under the `"bt_user"` key, alongside a
  `"bt_logged_in_at"` timestamp. This module is the single place that
  reads/writes that session and gates access:

    * `fetch_current_user/2` (plug) — load the session user into
      `conn.assigns.current_user` (`nil` when absent/expired).
    * `require_authenticated/2` (plug) — gate the HTTP render.
    * `on_mount/4` — the LiveView gate, run in a `live_session` so the socket
      re-validates on **every (re)connect**, and (when connected) schedules a
      **re-validation timer** that tears the socket down once the session passes
      its TTL — a mounted LiveView must not outlive its authorization.
    * `log_in/2` / `log_out/1` — mint / clear the session.

  ## Session lifecycle (ADR 0091 Decision 1)

  A LiveView socket authorized at mount must not outlive the user's
  authorization. We enforce a **maximum session TTL** (`:session_ttl_secs`,
  operator-configurable, default 4h) and **re-validate on a timer**
  (`:revalidate_interval_secs`, default 60s) and on every reconnect. Because
  server-side sessions do not observe IdP de-provisioning in real time, the
  **maximum enforcement lag** between an IdP revocation/role change and effective
  teardown is bounded by the re-validation interval (default ≤ 60s) plus, for a
  user who never reconnects, the session TTL (default ≤ 4h). Operators tighten
  both for a stricter SLA.

  ## Auth modes

    * **OIDC** — enabled when `Application.get_env(:bt_attach, :oidc)` is non-nil.
    * **Dev/IdP-less** — enabled when OIDC is *not* configured and
      `BT_IDE_DEV_AUTH` is truthy. It uses the same session handling (HttpOnly
      cookie, no URL token) and is **refused on a non-loopback interface in
      code** (not merely docs): otherwise it would bypass the whole OIDC + RBAC
      stack on a remote deployment.
    * **Disabled** — neither configured: the unchanged zero-config localhost
      story (ADR 0020). No gate.

  CSRF: the LiveView socket validates the signed session and the CSRF token on
  connect (Phoenix LiveView default, given `protect_from_forgery` in the
  `:browser` pipeline and the session-bearing `connect_info`).
  """

  use BtAttachWeb, :verified_routes

  require Logger

  import Plug.Conn
  import Phoenix.Controller, only: [redirect: 2]

  @session_key "bt_user"
  @logged_in_at_key "bt_logged_in_at"
  @default_ttl_secs 4 * 60 * 60
  @default_revalidate_secs 60
  @revalidate_msg :bt_session_revalidate

  @dev_claims %{
    "sub" => "dev",
    "email" => "dev@localhost",
    "groups" => ["beamtalk-owners"],
    "dev" => true
  }

  @doc "True when OIDC auth is configured/enforced (boot-resolved, ADR 0091)."
  @spec oidc_enabled?() :: boolean()
  def oidc_enabled?, do: Application.get_env(:bt_attach, :oidc) != nil

  @doc "The boot-resolved OIDC config map, or `nil` when disabled."
  @spec oidc_config() :: BtAttach.IdeConfig.t() | nil
  def oidc_config, do: Application.get_env(:bt_attach, :oidc)

  @doc """
  True when the IdP-less dev-auth path is enabled (OIDC unconfigured *and*
  `BT_IDE_DEV_AUTH` truthy). It is still loopback-gated at the call sites.
  """
  @spec dev_auth_enabled?() :: boolean()
  def dev_auth_enabled? do
    not oidc_enabled?() and
      String.downcase(System.get_env("BT_IDE_DEV_AUTH", "")) in ~w(1 true yes)
  end

  @doc "Whether any auth gate is active (OIDC or dev-auth)."
  @spec auth_required?() :: boolean()
  def auth_required?, do: oidc_enabled?() or dev_auth_enabled?()

  @doc "Maximum session lifetime in seconds (operator-configurable; default 4h)."
  @spec session_ttl_secs() :: pos_integer()
  def session_ttl_secs, do: Application.get_env(:bt_attach, :session_ttl_secs, @default_ttl_secs)

  @doc "Re-validation interval in seconds (operator-configurable; default 60s)."
  @spec revalidate_interval_secs() :: pos_integer()
  def revalidate_interval_secs,
    do: Application.get_env(:bt_attach, :revalidate_interval_secs, @default_revalidate_secs)

  # ── plugs ───────────────────────────────────────────────────────────────────

  @doc "Plug: assign `:current_user` from the session, dropping an expired one."
  def fetch_current_user(conn, _opts) do
    user =
      if session_live?(get_session(conn, @logged_in_at_key)) do
        get_session(conn, @session_key)
      end

    assign(conn, :current_user, user)
  end

  @doc """
  Plug: require an authenticated session.

  * OIDC enabled, no/expired user → redirect to the login route (302 → IdP).
  * Dev-auth enabled, request from loopback, no user → mint a dev session.
  * Dev-auth enabled, request **not** from loopback → refuse (403): the
    IdP-less path must never authenticate a remote client.
  * No gate active → pass through.
  """
  def require_authenticated(conn, _opts) do
    cond do
      conn.assigns[:current_user] ->
        conn

      oidc_enabled?() ->
        conn
        |> maybe_store_return_to()
        |> redirect(to: ~p"/oidc/auth")
        |> halt()

      dev_auth_enabled?() and loopback?(conn.remote_ip) ->
        # IdP-less loopback auto-login: same session handling, no URL token.
        conn
        |> put_session(@session_key, @dev_claims)
        |> put_session(@logged_in_at_key, now())
        |> assign(:current_user, @dev_claims)

      dev_auth_enabled?() ->
        Logger.warning(
          "dev-auth refused for non-loopback client #{:inet.ntoa(conn.remote_ip)}",
          domain: [:beamtalk, :liveview]
        )

        conn
        |> put_status(:forbidden)
        |> Phoenix.Controller.text("dev-auth is loopback-only")
        |> halt()

      true ->
        conn
    end
  end

  defp maybe_store_return_to(%{method: "GET", request_path: path} = conn) do
    put_session(conn, :user_return_to, path)
  end

  defp maybe_store_return_to(conn), do: conn

  # ── LiveView on_mount ─────────────────────────────────────────────────────────

  @doc """
  `on_mount` hook for a `live_session`: re-validate the session on every
  (re)connect, assign `:current_user`, and (when connected) arm the
  re-validation timer that tears the socket down at TTL.
  """
  def on_mount(:require_authenticated, _params, session, socket) do
    user = session[@session_key]
    logged_in_at = session[@logged_in_at_key]
    valid_user = if session_live?(logged_in_at), do: user

    cond do
      not auth_required?() ->
        {:cont, assign_identity(socket, user, logged_in_at)}

      valid_user && dev_connect_ok?(socket) ->
        {:cont, socket |> assign_identity(valid_user, logged_in_at) |> arm_revalidation()}

      true ->
        # Unauthenticated, expired, or a dev socket connecting from off-loopback.
        {:halt, Phoenix.LiveView.redirect(socket, to: login_path())}
    end
  end

  # When connected, schedule the re-validation timer and attach the hook that
  # acts on it. The hook handles ONLY our own message; everything else passes to
  # the LiveView. On expiry it redirects, which closes the socket and triggers
  # the LiveView's `terminate/2` (dropping dist subscriptions + the session).
  defp arm_revalidation(socket) do
    if Phoenix.LiveView.connected?(socket) do
      schedule_revalidation()

      Phoenix.LiveView.attach_hook(socket, :bt_revalidate, :handle_info, fn
        @revalidate_msg, socket ->
          if session_live?(socket.assigns[:logged_in_at]) do
            schedule_revalidation()
            {:halt, socket}
          else
            Logger.info("session expired; tearing down socket", domain: [:beamtalk, :liveview])
            {:halt, Phoenix.LiveView.redirect(socket, to: login_path())}
          end

        _msg, socket ->
          {:cont, socket}
      end)
    else
      socket
    end
  end

  defp assign_identity(socket, user, logged_in_at) do
    socket
    |> Phoenix.Component.assign(:current_user, user)
    |> Phoenix.Component.assign(:logged_in_at, logged_in_at)
  end

  # Dev-auth sockets must also connect from loopback (defense in depth alongside
  # the plug). OIDC / disabled modes don't care about the socket peer here.
  defp dev_connect_ok?(socket) do
    if dev_auth_enabled?() and Phoenix.LiveView.connected?(socket) do
      case Phoenix.LiveView.get_connect_info(socket, :peer_data) do
        %{address: ip} -> loopback?(ip)
        _ -> false
      end
    else
      true
    end
  end

  defp login_path, do: if(oidc_enabled?(), do: ~p"/oidc/auth", else: ~p"/")

  defp schedule_revalidation do
    Process.send_after(self(), @revalidate_msg, revalidate_interval_secs() * 1000)
  end

  # ── session minting ───────────────────────────────────────────────────────────

  @doc """
  Store the authenticated user's claims in a fresh server-side session.

  Renews the session id (`renew_session`) on login to defend against session
  fixation, then writes the claims + a login timestamp (for TTL). The claims map
  is the verified OIDC identity (`"sub"`, `"email"`, the groups claim, …) —
  consumed by RBAC (BT-2421). No URL token is ever minted.
  """
  def log_in(conn, claims) when is_map(claims) do
    return_to = get_session(conn, :user_return_to)

    conn
    |> renew_session()
    |> put_session(@session_key, claims)
    |> put_session(@logged_in_at_key, now())
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

  # ── helpers ───────────────────────────────────────────────────────────────────

  @doc false
  # A session is "live" if it carries an integer login time within the TTL.
  @spec session_live?(term()) :: boolean()
  def session_live?(logged_in_at) when is_integer(logged_in_at) do
    now() - logged_in_at < session_ttl_secs()
  end

  def session_live?(_), do: false

  @doc false
  # True for loopback IPv4/IPv6 (including IPv4-mapped ::ffff:127.0.0.0/8).
  @spec loopback?(:inet.ip_address() | term()) :: boolean()
  def loopback?({127, _, _, _}), do: true
  def loopback?({0, 0, 0, 0, 0, 0, 0, 1}), do: true
  # IPv4-mapped ::ffff:127.0.0.0/8 — the high group `ab` is (127 <<< 8) | octet2.
  def loopback?({0, 0, 0, 0, 0, 0xFFFF, ab, _cd}) when ab >= 0x7F00 and ab <= 0x7FFF, do: true
  def loopback?(_), do: false

  defp now, do: System.system_time(:second)
end

# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.OidcHandshake do
  @moduledoc """
  A dedicated cookie for the OIDC handshake state (ADR 0091 Decision 1 / BT-2419).

  The main session cookie is `SameSite=Strict` because it gates an RCE-bearing
  tool — but a Strict cookie is **not** sent on the cross-site top-level redirect
  the IdP makes back to `/oidc/callback`, so the `state` / PKCE verifier / nonce
  (the CSRF + replay guard) cannot live there. They live here instead: a
  short-lived, **encrypted** (opaque, tamper-proof), `SameSite=Lax`,
  `HttpOnly`, `path=/oidc` cookie — the only Lax cookie in the app, scoped to the
  OIDC handler. Lax permits it on the top-level GET redirect; the path scope keeps
  it off every other route.
  """

  import Plug.Conn

  @cookie "_bt_oidc_handshake"
  @salt "oidc handshake"
  # The handshake is seconds-to-minutes; cap it tightly to bound replay.
  @max_age_secs 600

  @doc "Encrypt `session_params` into the Lax, path-scoped handshake cookie."
  @spec put(Plug.Conn.t(), map()) :: Plug.Conn.t()
  def put(conn, session_params) do
    token = Phoenix.Token.encrypt(conn, @salt, session_params)

    put_resp_cookie(conn, @cookie, token,
      http_only: true,
      same_site: "Lax",
      secure: Application.get_env(:bt_attach, :secure_session, false),
      path: "/oidc",
      max_age: @max_age_secs
    )
  end

  @doc """
  Read and consume the handshake cookie: returns `{:ok, params, conn}` with the
  cookie deleted, or `:error` (absent/expired/tampered) — fail closed, never
  proceed with an unverifiable handshake.
  """
  @spec take(Plug.Conn.t()) :: {:ok, map(), Plug.Conn.t()} | :error
  def take(conn) do
    conn = fetch_cookies(conn)

    with token when is_binary(token) <- conn.cookies[@cookie],
         {:ok, params} <- Phoenix.Token.decrypt(conn, @salt, token, max_age: @max_age_secs) do
      {:ok, params, delete_resp_cookie(conn, @cookie, path: "/oidc")}
    else
      _ -> :error
    end
  end
end

# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.OidcController do
  @moduledoc """
  OIDC login + callback endpoints for the authenticated front (ADR 0091
  Decision 1).

    * `GET /oidc/auth` — build the authorization-request URL (`state` + PKCE +
      `nonce`), stash the opaque `session_params` in the server-side session,
      and 302 the browser to the IdP. No token is ever placed in a URL we mint.
    * `GET /oidc/callback` — exchange the returned `code` for verified claims
      (Assent validates `state`, the PKCE verifier, the nonce and the ID-token
      signature), then mint the server-side session and redirect into the IDE.

  A misconfigured front (OIDC disabled but these routes hit) responds 404-ish
  via a redirect home rather than leaking a stack trace.
  """
  use BtAttachWeb, :controller

  require Logger

  alias BtAttach.Oidc
  alias BtAttachWeb.Auth

  @session_params_key :oidc_session_params

  @doc "Start the OIDC authorization-code flow: 302 → IdP."
  def auth(conn, _params) do
    with config when is_map(config) <- Auth.oidc_config(),
         {:ok, %{url: url, session_params: session_params}} <- Oidc.authorize_url(config) do
      conn
      |> put_session(@session_params_key, session_params)
      |> redirect(external: url)
    else
      nil ->
        # OIDC not configured — nothing to authenticate against.
        redirect(conn, to: ~p"/")

      {:error, reason} ->
        Logger.error("OIDC authorize_url failed: #{inspect(reason)}",
          domain: [:beamtalk, :liveview]
        )

        conn
        |> put_status(:bad_gateway)
        |> text("OIDC provider unavailable")
    end
  end

  @doc "OIDC redirect target: exchange the code, mint the session, enter the IDE."
  def callback(conn, params) do
    session_params = get_session(conn, @session_params_key)
    config = Auth.oidc_config()

    cond do
      is_nil(config) ->
        redirect(conn, to: ~p"/")

      is_nil(session_params) ->
        # No in-flight flow (stale/forged callback): refuse, don't exchange.
        Logger.warning("OIDC callback without session_params (possible CSRF/replay)",
          domain: [:beamtalk, :liveview]
        )

        deny(conn)

      true ->
        conn = delete_session(conn, @session_params_key)

        case Oidc.callback(config, params, session_params) do
          {:ok, %{claims: claims}} ->
            Logger.info("OIDC login: sub=#{inspect(claims["sub"])}",
              domain: [:beamtalk, :liveview]
            )

            Auth.log_in(conn, claims)

          {:error, reason} ->
            Logger.warning("OIDC callback rejected: #{inspect(reason)}",
              domain: [:beamtalk, :liveview]
            )

            deny(conn)
        end
    end
  end

  defp deny(conn) do
    conn
    |> put_status(:forbidden)
    |> text("Authentication failed")
  end
end

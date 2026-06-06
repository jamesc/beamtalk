# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Oidc.Assent do
  @moduledoc """
  Default `BtAttach.Oidc` implementation, backed by `Assent.Strategy.OIDC`
  (ADR 0091 Decision 1).

  Runs the OIDC **authorization-code** flow with `state`, **PKCE**
  (`code_verifier: true`) and a **nonce** (`nonce: true`); Assent fetches the
  provider's discovery document from the issuer and validates the returned
  ID token. TLS to the IdP uses OTP `:httpc` with the CA bundle from `castore`.

  We never log the client secret. The `session_params` Assent returns (state,
  PKCE verifier, nonce) are carried in the server-side session by the
  controller and replayed into `callback/3` — they are the CSRF/replay guard.
  """

  @behaviour BtAttach.Oidc

  @scope "openid email profile"

  @impl true
  def authorize_url(config) do
    Assent.Strategy.OIDC.authorize_url(assent_config(config))
  end

  @impl true
  def callback(config, params, session_params) do
    assent_config(config)
    |> Keyword.put(:session_params, session_params)
    |> then(&Assent.Strategy.OIDC.callback(&1, params))
    |> case do
      {:ok, %{user: claims}} when is_map(claims) -> {:ok, %{claims: claims}}
      {:ok, other} -> {:error, {:unexpected_callback_shape, other}}
      {:error, _} = error -> error
    end
  end

  # Build the Assent provider config from the resolved IdeConfig. PKCE + nonce
  # are enabled here (not the IdP's choice). `:scope` requests the groups-bearing
  # claims; the actual groups claim name is read at RBAC time (BT-2421).
  defp assent_config(config) do
    [
      client_id: config.client_id,
      client_secret: config.client_secret,
      base_url: config.issuer,
      redirect_uri: config.redirect_uri,
      authorization_params: [scope: @scope],
      code_verifier: true,
      nonce: true,
      http_adapter: {Assent.HTTPAdapter.Httpc, [ssl: [cacerts: :public_key.cacerts_get()]]}
    ]
  end
end

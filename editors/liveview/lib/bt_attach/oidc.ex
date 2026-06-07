# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Oidc do
  @moduledoc """
  OIDC provider seam for the authenticated front (ADR 0091 Decision 1).

  This is a thin behaviour over the real OIDC exchange so the controller can be
  driven in tests without a live IdP. The default implementation
  (`BtAttach.Oidc.Assent`) uses the `:assent` library — authorization-code flow
  with `state` + PKCE + `nonce`, ID-token validation — so we **don't roll our
  own crypto** (ADR 0091 Principle 5). A test or an air-gapped deployment can
  swap the implementation via:

      config :bt_attach, :oidc_provider, MyFakeProvider

  ## Contract

    * `authorize_url/1` — given the resolved `BtAttach.IdeConfig` map, return
      `{:ok, %{url: url, session_params: map}}`. `session_params` (state, PKCE
      verifier, nonce) is opaque and must be stashed in the server-side session
      and handed back to `callback/3` — it is the CSRF/replay guard.
    * `callback/3` — given the config, the callback query params, and the
      stashed `session_params`, return `{:ok, %{claims: map}}` where `claims`
      are the verified ID-token/userinfo claims (`"sub"`, `"email"`, the
      groups claim, …), or `{:error, term}`.
  """

  @type config :: BtAttach.IdeConfig.t()
  @type session_params :: map()

  @callback authorize_url(config()) ::
              {:ok, %{url: String.t(), session_params: session_params()}} | {:error, term()}
  @callback callback(config(), params :: map(), session_params()) ::
              {:ok, %{claims: map()}} | {:error, term()}

  @doc "The configured OIDC provider implementation (default `BtAttach.Oidc.Assent`)."
  @spec impl() :: module()
  def impl, do: Application.get_env(:bt_attach, :oidc_provider, BtAttach.Oidc.Assent)

  @doc "Build the authorization-request URL + session params via the configured impl."
  @spec authorize_url(config()) ::
          {:ok, %{url: String.t(), session_params: session_params()}} | {:error, term()}
  def authorize_url(config), do: impl().authorize_url(config)

  @doc "Exchange the callback code for verified claims via the configured impl."
  @spec callback(config(), map(), session_params()) :: {:ok, %{claims: map()}} | {:error, term()}
  def callback(config, params, session_params),
    do: impl().callback(config, params, session_params)
end

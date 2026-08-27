# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Oidc.AssentTest do
  @moduledoc """
  BT-3293: `BtAttach.Oidc.Assent` (the default `BtAttach.Oidc` implementation,
  ADR 0091 Decision 1) had 0% coverage. `BtAttachWeb.OidcFlowTest` exercises
  the controller boundary but swaps in a `BtAttach.Oidc` fake there, so it
  never touches this module's own logic: the exact `Assent.Strategy.OIDC`
  config it builds (PKCE + nonce always on, scope, the `:httpc`/`castore`
  adapter) and how it reshapes `Assent.Strategy.OIDC.callback/2`'s result
  into the `BtAttach.Oidc` contract.

  These tests stub `Assent.Strategy.OIDC` with `:meck` rather than hitting a
  live IdP, matching the pattern in
  `BtAttach.WorkspaceClassifyUnreachableHostnameTest`.

  `async: false`: `:meck` globally replaces `Assent.Strategy.OIDC` for the
  whole VM.
  """
  use ExUnit.Case, async: false

  alias BtAttach.Oidc.Assent, as: OidcAssent

  @config %{
    issuer: "https://idp.test",
    client_id: "beamtalk-ide",
    client_secret: "shhh",
    redirect_uri: "https://ide.test/oidc/callback"
  }

  setup do
    :meck.new(Assent.Strategy.OIDC, [:passthrough])

    # See BtAttach.WorkspaceClassifyUnreachableHostnameTest for why this is
    # the no-arg form: `on_exit` runs in a separate runner process after the
    # test process (meck's owner) has already exited, which auto-unloads the
    # mock; the no-arg form tolerates that, the 1-arg form raises on it.
    on_exit(fn -> :meck.unload() end)
    :ok
  end

  describe "authorize_url/1" do
    test "builds the Assent config with PKCE + nonce always on and delegates" do
      :meck.expect(Assent.Strategy.OIDC, :authorize_url, fn config ->
        assert Keyword.get(config, :client_id) == "beamtalk-ide"
        assert Keyword.get(config, :client_secret) == "shhh"
        assert Keyword.get(config, :base_url) == "https://idp.test"
        assert Keyword.get(config, :redirect_uri) == "https://ide.test/oidc/callback"
        assert Keyword.get(config, :authorization_params) == [scope: "openid email profile"]
        assert Keyword.get(config, :code_verifier) == true
        assert Keyword.get(config, :nonce) == true

        assert {Assent.HTTPAdapter.Httpc, adapter_opts} = Keyword.get(config, :http_adapter)
        assert adapter_opts[:ssl][:cacerts] == :public_key.cacerts_get()

        {:ok,
         %{
           url: "https://idp.test/authorize?state=xyz",
           session_params: %{state: "xyz"}
         }}
      end)

      assert OidcAssent.authorize_url(@config) ==
               {:ok,
                %{
                  url: "https://idp.test/authorize?state=xyz",
                  session_params: %{state: "xyz"}
                }}
    end

    test "returns the error verbatim on failure" do
      :meck.expect(Assent.Strategy.OIDC, :authorize_url, fn _config ->
        {:error, "discovery document fetch failed"}
      end)

      assert OidcAssent.authorize_url(@config) == {:error, "discovery document fetch failed"}
    end
  end

  describe "callback/3" do
    test "reshapes a successful Assent callback into {:ok, %{claims: claims}}" do
      claims = %{"sub" => "alice", "email" => "alice@example.com"}

      # BtAttach.Oidc.Assent calls Assent.Strategy.OIDC.callback/2 (arity 3
      # with a default third `strategy` arg it never overrides).
      :meck.expect(Assent.Strategy.OIDC, :callback, fn config, params ->
        assert params == %{"code" => "good"}
        # session_params must be threaded into the Assent config verbatim —
        # it's the CSRF/replay guard (state, PKCE verifier, nonce).
        assert Keyword.get(config, :session_params) == %{state: "xyz"}

        {:ok, %{user: claims, token: %{"access_token" => "abc"}}}
      end)

      assert OidcAssent.callback(@config, %{"code" => "good"}, %{state: "xyz"}) ==
               {:ok, %{claims: claims}}
    end

    test "returns {:error, {:unexpected_callback_shape, other}} when Assent's :user isn't a map" do
      :meck.expect(Assent.Strategy.OIDC, :callback, fn _config, _params ->
        {:ok, %{no_user_key: true}}
      end)

      assert OidcAssent.callback(@config, %{}, %{}) ==
               {:error, {:unexpected_callback_shape, %{no_user_key: true}}}
    end

    test "returns the error verbatim on failure" do
      :meck.expect(Assent.Strategy.OIDC, :callback, fn _config, _params ->
        {:error, :invalid_grant}
      end)

      assert OidcAssent.callback(@config, %{"code" => "bad"}, %{}) == {:error, :invalid_grant}
    end
  end
end

# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.OidcFlowTest do
  # Toggles the global :bt_attach OIDC app env, so not async.
  use BtAttachWeb.ConnCase, async: false

  # A fake OIDC provider so the flow is exercised end-to-end without a live IdP
  # (BtAttach.Oidc is a behaviour precisely so this is swappable — ADR 0091).
  defmodule FakeProvider do
    @behaviour BtAttach.Oidc

    @impl true
    def authorize_url(_config) do
      {:ok,
       %{
         url: "https://idp.test/authorize?state=xyz&code_challenge=abc",
         session_params: %{state: "xyz"}
       }}
    end

    @impl true
    def callback(_config, %{"code" => "good"}, %{state: "xyz"}) do
      {:ok,
       %{
         claims: %{
           "sub" => "alice",
           "email" => "alice@example.com",
           "groups" => ["beamtalk-owners"]
         }
       }}
    end

    def callback(_config, _params, _session_params), do: {:error, :invalid_grant}
  end

  @oidc_config %{
    issuer: "https://idp.test",
    client_id: "beamtalk-ide",
    redirect_uri: "https://ide.test/oidc/callback",
    groups_claim: "groups",
    client_secret: "shhh",
    roles: %{}
  }

  defp enable_oidc do
    Application.put_env(:bt_attach, :oidc, @oidc_config)
    Application.put_env(:bt_attach, :oidc_provider, FakeProvider)

    on_exit(fn ->
      Application.delete_env(:bt_attach, :oidc)
      Application.delete_env(:bt_attach, :oidc_provider)
    end)
  end

  describe "OIDC disabled (default localhost story)" do
    test "the IDE is reachable without authentication", %{conn: conn} do
      # No :oidc app env → auth not enforced.
      conn = get(conn, ~p"/")
      assert html_response(conn, 200) =~ "Beamtalk Workspace"
    end
  end

  describe "OIDC enabled" do
    setup do
      enable_oidc()
      :ok
    end

    test "an unauthenticated request to the IDE is redirected (302 → login)", %{conn: conn} do
      conn = get(conn, ~p"/")
      assert redirected_to(conn) == ~p"/oidc/auth"
    end

    test "GET /oidc/auth 302s to the IdP and stashes a Lax, path-scoped handshake cookie",
         %{conn: conn} do
      conn = get(conn, ~p"/oidc/auth")
      assert redirected_to(conn) =~ "https://idp.test/authorize"

      handshake = conn.resp_cookies["_bt_oidc_handshake"]
      assert handshake.value != ""
      assert handshake.same_site == "Lax"
      assert handshake.path == "/oidc"
      assert handshake.http_only
    end

    test "a valid callback mints a session and lands authenticated in the IDE", %{conn: conn} do
      conn = get(conn, ~p"/oidc/auth")

      conn =
        conn
        |> recycle()
        |> get(~p"/oidc/callback?code=good")

      assert redirected_to(conn) == ~p"/"
      assert get_session(conn, "bt_user")["sub"] == "alice"
      # The minted session cookie is SameSite=Strict (BT-2419).
      assert conn.resp_cookies["_bt_attach_key"].same_site == "Strict"

      # The session now satisfies the gate.
      authed = conn |> recycle() |> get(~p"/")
      assert html_response(authed, 200) =~ "Beamtalk Workspace"
    end

    test "a callback with no in-flight session params is refused (CSRF/replay guard)", %{
      conn: conn
    } do
      conn = get(conn, ~p"/oidc/callback?code=good")
      assert text_response(conn, 403) =~ "Authentication failed"
    end

    test "a provider error on callback is refused (no session minted)", %{conn: conn} do
      conn = get(conn, ~p"/oidc/auth")

      conn =
        conn
        |> recycle()
        |> get(~p"/oidc/callback?code=bad")

      assert text_response(conn, 403)
      assert get_session(conn, "bt_user") == nil
    end

    test "the login route honours a stored return-to path", %{conn: conn} do
      # Hitting a protected GET stores the path; after login we return there.
      conn = get(conn, ~p"/")
      assert redirected_to(conn) == ~p"/oidc/auth"

      conn = conn |> recycle() |> get(~p"/oidc/auth")

      conn =
        conn
        |> recycle()
        |> get(~p"/oidc/callback?code=good")

      assert redirected_to(conn) == ~p"/"
    end
  end
end

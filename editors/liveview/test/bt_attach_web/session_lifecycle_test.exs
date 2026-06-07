# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.SessionLifecycleTest do
  # Mutates global app env / env vars, so not async.
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias BtAttachWeb.Auth

  @oidc_config %{
    issuer: "https://idp.test",
    client_id: "beamtalk-ide",
    redirect_uri: "https://ide.test/oidc/callback",
    groups_claim: "groups",
    client_secret: "shhh",
    roles: %{"owner" => ["beamtalk-owners"]}
  }

  setup do
    on_exit(fn ->
      Application.delete_env(:bt_attach, :oidc)
      Application.delete_env(:bt_attach, :session_ttl_secs)
      Application.delete_env(:bt_attach, :revalidate_interval_secs)
      System.delete_env("BT_IDE_DEV_AUTH")
    end)

    :ok
  end

  defp enable_oidc, do: Application.put_env(:bt_attach, :oidc, @oidc_config)

  defp authed_conn(conn, age_secs) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "alice", "groups" => ["beamtalk-owners"]},
      "bt_logged_in_at" => System.system_time(:second) - age_secs
    })
  end

  describe "helpers" do
    test "session_live?/1 honours the TTL" do
      Application.put_env(:bt_attach, :session_ttl_secs, 3600)
      now = System.system_time(:second)
      assert Auth.session_live?(now)
      assert Auth.session_live?(now - 3599)
      refute Auth.session_live?(now - 3601)
      refute Auth.session_live?(nil)
      refute Auth.session_live?("bogus")
    end

    test "loopback?/1 recognises loopback v4/v6 and rejects routable" do
      assert Auth.loopback?({127, 0, 0, 1})
      assert Auth.loopback?({127, 13, 2, 9})
      assert Auth.loopback?({0, 0, 0, 0, 0, 0, 0, 1})
      assert Auth.loopback?({0, 0, 0, 0, 0, 0xFFFF, 0x7F00, 1})
      refute Auth.loopback?({203, 0, 113, 5})
      refute Auth.loopback?({0, 0, 0, 0, 0, 0xFFFF, 0xC000, 1})
      refute Auth.loopback?(:nonsense)
    end
  end

  # ProbeLive runs the same on_mount gate as the IDE but without the workspace
  # attach (Erlang distribution) the sandbox can't provide.
  defp live_probe(conn), do: live_isolated(conn, BtAttachWeb.ProbeLive)

  describe "TTL re-validation on (re)connect (on_mount)" do
    setup do
      enable_oidc()
      Application.put_env(:bt_attach, :session_ttl_secs, 3600)
      :ok
    end

    test "a fresh session mounts", %{conn: conn} do
      assert {:ok, _view, html} = live_probe(authed_conn(conn, 0))
      assert html =~ "probe ok"
    end

    test "an expired session is redirected to re-auth on connect", %{conn: conn} do
      assert {:error, {:redirect, %{to: "/oidc/auth"}}} = live_probe(authed_conn(conn, 7200))
    end
  end

  describe "TTL re-validation on a timer (tears down a mounted socket)" do
    setup do
      enable_oidc()
      Application.put_env(:bt_attach, :session_ttl_secs, 3600)
      :ok
    end

    test "an in-flight socket whose session has since expired is torn down", %{conn: conn} do
      {:ok, view, _html} = live_probe(authed_conn(conn, 0))

      # Simulate the session passing its TTL, then fire the re-validation tick the
      # on_mount hook armed. The socket must redirect to re-auth, closing it (and,
      # in the real IDE, triggering the dist teardown in WorkspaceLive.terminate/2).
      Application.put_env(:bt_attach, :session_ttl_secs, 0)
      send(view.pid, :bt_session_revalidate)

      assert_redirect(view, "/oidc/auth")
    end
  end

  describe "IdP-less dev-auth is loopback-only (refused on non-loopback in code)" do
    setup do
      # OIDC disabled + dev-auth on.
      System.put_env("BT_IDE_DEV_AUTH", "1")
      :ok
    end

    test "a loopback request is auto-logged-in", %{conn: conn} do
      conn = %{conn | remote_ip: {127, 0, 0, 1}}
      conn = get(conn, ~p"/")
      assert html_response(conn, 200) =~ "Beamtalk Workspace"
    end

    test "a non-loopback request is refused (403), never authenticated", %{conn: conn} do
      conn = %{conn | remote_ip: {203, 0, 113, 5}}
      conn = get(conn, ~p"/")
      assert response(conn, 403) =~ "loopback-only"
    end
  end
end

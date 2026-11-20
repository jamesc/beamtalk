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

  describe "dev-auth is refused behind a reverse proxy (BT-5070)" do
    setup do
      System.put_env("BT_IDE_DEV_AUTH", "1")
      :ok
    end

    for header <- ["x-forwarded-for", "forwarded", "x-real-ip", "x-forwarded-host"] do
      test "a loopback request carrying #{header} is refused (403)", %{conn: conn} do
        conn =
          %{conn | remote_ip: {127, 0, 0, 1}}
          |> put_req_header(unquote(header), "203.0.113.9")
          |> get(~p"/")

        assert response(conn, 403) =~ "loopback-only"
      end
    end

    test "forwarded?/1 spots proxy headers case-insensitively" do
      assert Auth.forwarded?([{"X-Forwarded-For", "1.2.3.4"}])
      assert Auth.forwarded?([{"host", "x"}, {"via", "1.1 proxy"}])
      refute Auth.forwarded?([{"host", "localhost"}, {"accept", "text/html"}])
    end

    test "the /live socket declares :x_headers so the socket can refuse proxied dev-auth" do
      {_path, _module, opts} =
        Enum.find(BtAttachWeb.Endpoint.__sockets__(), fn {path, _, _} -> path == "/live" end)

      assert :x_headers in get_in(opts, [:websocket, :connect_info])
      assert :x_headers in get_in(opts, [:longpoll, :connect_info])
    end

    test "a connected mount whose upgrade carried x-forwarded-for is refused", %{conn: conn} do
      conn =
        conn
        |> authed_conn(0)
        |> with_connect_info(%{
          peer_data: %{address: {127, 0, 0, 1}, port: 0, ssl_cert: nil},
          x_headers: [{"x-forwarded-for", "203.0.113.9"}]
        })

      assert {:error, {:redirect, _}} = live_probe(conn)
    end
  end

  describe "logout and cookie confidentiality (BT-5070)" do
    test "DELETE /logout clears the session and redirects", %{conn: conn} do
      conn = conn |> authed_conn(0) |> init_csrf() |> delete(~p"/logout")
      assert redirected_to(conn) == "/"
      refute get_session(conn, "bt_user")
      refute get_session(conn, "bt_logged_in_at")
    end

    test "logout broadcasts a disconnect to the session's live sockets", %{conn: conn} do
      id = "bt_session:test-logout"
      BtAttachWeb.Endpoint.subscribe(id)

      conn =
        conn
        |> authed_conn(0)
        |> Plug.Conn.put_session("live_socket_id", id)
        |> init_csrf()
        |> Plug.Conn.put_session("live_socket_id", id)
        |> delete(~p"/logout")

      assert redirected_to(conn) == "/"
      assert_receive %Phoenix.Socket.Broadcast{topic: ^id, event: "disconnect"}
    end

    test "a login mints a live_socket_id so logout can reach its sockets", %{conn: conn} do
      System.put_env("BT_IDE_DEV_AUTH", "1")
      conn = %{conn | remote_ip: {127, 0, 0, 1}} |> get(~p"/")
      assert "bt_session:" <> _ = get_session(conn, "live_socket_id")
    end

    test "GET /logout is not a route (logout needs the CSRF-protected DELETE)", %{conn: conn} do
      assert get(conn, "/logout").status == 404
    end

    test "the session cookie is encrypted, not merely signed" do
      opts = BtAttachWeb.Endpoint.session_options()
      assert is_binary(opts[:encryption_salt])
      assert is_binary(opts[:signing_salt])
    end

    test "claims written to the session cookie are not readable from the cookie value", %{
      conn: conn
    } do
      System.put_env("BT_IDE_DEV_AUTH", "1")
      conn = %{conn | remote_ip: {127, 0, 0, 1}} |> get(~p"/")
      cookie = conn.resp_cookies["_bt_attach_key"].value
      refute cookie =~ "bt_user"

      refute Enum.any?(
               [cookie, safe_decode(cookie)],
               &String.contains?(&1, "bt_logged_in_at")
             )
    end

    defp safe_decode(cookie) do
      case cookie |> String.split(".") |> hd() |> Base.url_decode64(padding: false) do
        {:ok, bin} -> bin
        _ -> ""
      end
    end

    defp init_csrf(conn) do
      Plug.Test.init_test_session(conn, %{
        "bt_user" => %{"sub" => "alice", "groups" => ["beamtalk-owners"]},
        "bt_logged_in_at" => System.system_time(:second)
      })
      |> put_req_header("x-csrf-token", Plug.CSRFProtection.get_csrf_token())
    end
  end

  describe "dev-auth connected mount requires :peer_data in connect_info (BT-3228)" do
    setup do
      System.put_env("BT_IDE_DEV_AUTH", "1")
      :ok
    end

    defp with_connect_info(conn, connect_info),
      do: Plug.Conn.put_private(conn, :live_view_connect_info, connect_info)

    test "the /live socket declares :peer_data, or dev-auth loops forever (regression guard)" do
      {_path, _module, opts} =
        Enum.find(BtAttachWeb.Endpoint.__sockets__(), fn {path, _, _} -> path == "/live" end)

      assert :peer_data in get_in(opts, [:websocket, :connect_info])
      assert :peer_data in get_in(opts, [:longpoll, :connect_info])
    end

    test "a connected mount with loopback peer_data succeeds", %{conn: conn} do
      conn =
        conn
        |> authed_conn(0)
        |> with_connect_info(%{peer_data: %{address: {127, 0, 0, 1}, port: 0, ssl_cert: nil}})

      assert {:ok, _view, html} = live_probe(conn)
      assert html =~ "probe ok"
    end

    test "a connected mount without :peer_data in connect_info (undeclared socket key, the bug) " <>
           "is redirected rather than mounted",
         %{conn: conn} do
      conn =
        conn
        |> authed_conn(0)
        |> with_connect_info(%{})

      assert {:error, {:redirect, %{to: "/"}}} = live_probe(conn)
    end
  end

  describe "session cookie Secure flag is runtime-resolved (BT-3233)" do
    # WKWebView (the Tauri desktop webview) silently drops `Secure` cookies
    # over plain http — no localhost exception like Chrome/Firefox — so the
    # desktop posture must serve the session cookie without `Secure`, while a
    # TLS deployment keeps it. The flag must flip WITHOUT recompiling: a
    # compile-time flag frozen into the endpoint was the bug.
    setup do
      on_exit(fn -> Application.delete_env(:bt_attach, :secure_session) end)
      :ok
    end

    defp session_cookie(conn) do
      conn
      |> Plug.Conn.get_resp_header("set-cookie")
      |> Enum.find(&String.starts_with?(&1, "_bt_attach_key="))
    end

    test "desktop/local posture (secure_session false): cookie is sent without Secure",
         %{conn: conn} do
      Application.put_env(:bt_attach, :secure_session, false)

      cookie = conn |> get(~p"/") |> session_cookie()
      assert cookie
      refute cookie =~ ~r/;\s*secure/i
    end

    test "TLS posture (secure_session true): cookie is Secure, same compiled endpoint",
         %{conn: conn} do
      Application.put_env(:bt_attach, :secure_session, true)

      cookie = conn |> get(~p"/") |> session_cookie()
      assert cookie
      assert cookie =~ ~r/;\s*secure/i
    end

    test "the /live socket's connect_info session options are an MFA, not a frozen list" do
      {_path, _module, opts} =
        Enum.find(BtAttachWeb.Endpoint.__sockets__(), fn {path, _, _} -> path == "/live" end)

      assert {BtAttachWeb.Endpoint, :session_options, []} =
               get_in(opts, [:websocket, :connect_info])[:session]

      assert {BtAttachWeb.Endpoint, :session_options, []} =
               get_in(opts, [:longpoll, :connect_info])[:session]
    end
  end
end

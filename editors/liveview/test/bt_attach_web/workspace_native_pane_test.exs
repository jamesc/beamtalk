# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceNativePaneTest do
  @moduledoc """
  Integration test for the System Browser native backing-source pane (BT-2578):
  opening a `native:` class's definition (ADR 0056) badges its backing Erlang
  gen_server module and, on toggle, shows that module's `.erl` read-only — since
  the real logic lives in `handle_call` clauses, not the `self delegate` facade
  methods. A backing module shipped without source degrades to a clear empty
  state, and ordinary classes show no badge.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `browse_class_definition` /
  `browse_native_source` carry the native fields the real ops added. No
  `:workspace` tag, so this runs in the bare `mix test` lane.
  """
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  setup do
    Application.put_env(:bt_attach, :workspace_client, BtAttachWeb.StubWorkspaceClient)

    Application.put_env(:bt_attach, :oidc, %{
      issuer: "https://idp",
      client_id: "id",
      redirect_uri: "https://ide/callback",
      groups_claim: "groups",
      client_secret: "x",
      roles: %{"owner" => ["beamtalk-owners"], "observer" => ["beamtalk-observers"]}
    })

    Application.put_env(:bt_attach, :session_ttl_secs, 3600)

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      Application.delete_env(:bt_attach, :oidc)
      Application.delete_env(:bt_attach, :session_ttl_secs)
      BtAttachWeb.StubWorkspaceClient.stop_state(2_000)
    end)

    {:ok, _} = BtAttachWeb.StubWorkspaceClient.start_state()

    :ok
  end

  defp owner_conn(conn) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "alice", "groups" => ["beamtalk-owners"]},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  defp observer_conn(conn) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "bob", "groups" => ["beamtalk-observers"]},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  describe "native backing-source pane (BT-2578)" do
    test "a native: class badges its backing module and toggles its source", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Open Subprocess's class definition — the stub marks it native:, backed by
      # beamtalk_subprocess.
      html = render_click(view, "browser_open_definition", %{"class" => "Subprocess"})

      # The "Erlang backend" badge names the gen_server module, with a toggle.
      assert html =~ "Erlang backend"
      assert html =~ "beamtalk_subprocess"
      assert html =~ "View Erlang source"
      # The source is lazily fetched — hidden until the toggle is clicked.
      refute html =~ ~s(class="native-pre")

      # Toggling reveals the read-only source, its origin badge, and the
      # best-effort handle_call clause map.
      html = render_click(view, "browser_open_native", %{"class" => "Subprocess"})

      assert html =~ ~s(class="native-pre")
      assert html =~ "read-only"
      assert html =~ "stdlib"
      assert html =~ "handle_call({readLine"
      assert html =~ ~s(class="native-clauses")
      # The toggle now offers to collapse the pane.
      assert html =~ "Hide Erlang source"

      # Toggling again collapses it.
      html = render_click(view, "browser_open_native", %{"class" => "Subprocess"})
      refute html =~ ~s(class="native-pre")
    end

    test "a backing module without shipped source shows a clear empty state", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_open_definition", %{"class" => "Headless"})
      html = render_click(view, "browser_open_native", %{"class" => "Headless"})

      # No source content → the honest "not available" empty state, not an error
      # and not an empty pane.
      assert html =~ "Erlang source not available"
      refute html =~ ~s(class="native-pre")
    end

    test "an ordinary class shows no Erlang-backend badge", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Counter is not native: — opening its definition surfaces the doc block but
      # no native pane.
      html = render_click(view, "browser_open_definition", %{"class" => "Counter"})

      refute html =~ "Erlang backend"
      refute html =~ "View Erlang source"
    end

    test "an observer sees the read-only native pane", %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      # The pane rides the `:read`-capability browse-native-source op, so it
      # renders for an observer even though the editable source form does not.
      render_click(view, "browser_open_definition", %{"class" => "Subprocess"})
      html = render_click(view, "browser_open_native", %{"class" => "Subprocess"})

      assert html =~ "Erlang backend"
      assert html =~ ~s(class="native-pre")
      assert html =~ "handle_call({readLine"
    end
  end
end

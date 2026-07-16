# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceTypeAliasesTest do
  @moduledoc """
  Integration test for the System Browser "Type Aliases" section (ADR 0108
  Phase 8, BT-2903): every loaded package's declared `type` aliases are
  discoverable in the IDE as a third panel mode alongside Classes/Native,
  mirroring `WorkspaceNativeModulesTest`'s coverage of the Native panel. A
  `type` declaration produces no BEAM module (aliases erase entirely), so
  unlike Native there is no editor tab to open — each row renders its
  expansion inline, with the same package/origin badge vocabulary the class
  tree and Native panel already use.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `browse_type_aliases` mirrors the
  new op. No `:workspace` tag, so this runs in the bare `mix test` lane.
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

  describe "Type Aliases browser (ADR 0108 Phase 8, BT-2903)" do
    test "the panel defaults to Classes mode; alias rows are not in the tree", %{conn: conn} do
      {:ok, _view, html} = live(owner_conn(conn), "/")

      assert html =~ ~s(aria-label="Browser mode")
      assert html =~ ~s(phx-value-mode="aliases")
      refute html =~ "RestartStrategy"
    end

    test "switching to Type Aliases mode lists aliases with expansion + package badges", %{
      conn: conn
    } do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "browser_mode", %{"mode" => "aliases"})

      # The project alias, its expansion, and (for a non-project row) the
      # DEP · <pkg> badge.
      assert html =~ "RestartStrategy"
      assert html =~ "#temporary | #transient | #permanent"
      assert html =~ "JsonValue"
      assert html =~ "DEP · json"
      # A project-origin row carries no badge (mirrors the class tree/Native
      # convention: the badge is only shown for non-project origins).
      refute html =~ ~s(source-origin-tag project)

      # The panel footer carries the count badge.
      assert html =~ "Type aliases"

      # Switching back to Classes mode hides the alias rows again.
      html = render_click(view, "browser_mode", %{"mode" => "classes"})
      refute html =~ "RestartStrategy"
    end

    test "switching to Type Aliases hides the Beamtalk method browser; Classes restores it", %{
      conn: conn
    } do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "browser_select_class", %{"class" => "Counter"})
      assert html =~ ~s(class="tree sb-classdef")
      assert html =~ "Add a method"

      html = render_click(view, "browser_mode", %{"mode" => "aliases"})
      assert html =~ "RestartStrategy"
      refute html =~ ~s(class="tree sb-classdef")
      refute html =~ "Add a method"

      html = render_click(view, "browser_mode", %{"mode" => "classes"})
      assert html =~ ~s(class="tree sb-classdef")
      assert html =~ "Add a method"
    end

    test "an observer can browse type aliases (the op is :read)", %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      html = render_click(view, "browser_mode", %{"mode" => "aliases"})

      assert html =~ "RestartStrategy"
      assert html =~ "#temporary | #transient | #permanent"
    end
  end
end

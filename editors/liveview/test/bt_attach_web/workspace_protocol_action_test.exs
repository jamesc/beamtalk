# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceProtocolActionTest do
  @moduledoc """
  Integration test for the protocol-definition action row (BT-2639): the protocol
  equivalent of Senders/Implementors. When a class-definition tab is opened for a
  Protocol (`is_protocol` true on the `browse-class-definition` row), the editor
  renders **Required methods** + **Conforming classes** buttons; clicking them
  opens the shared nav popover with navigable rows.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `browse_class_definition` reports
  `Printable` as a protocol (and `Counter` as an ordinary class), and whose
  `required_methods_of` / `conforming_classes_of` return canned rows for
  `Printable`. No `:workspace` tag, so this runs in the bare `mix test` lane.
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

  describe "protocol action row (BT-2639)" do
    test "a protocol definition tab renders the protocol action buttons", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "browser_open_definition", %{"class" => "Printable"})

      assert html =~ ~s(phx-click="required_methods")
      assert html =~ "Required methods"
      assert html =~ ~s(phx-click="conforming_classes")
      assert html =~ "Conforming classes"
    end

    test "an ordinary class definition tab shows no protocol action row", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "browser_open_definition", %{"class" => "Counter"})

      refute html =~ ~s(phx-click="required_methods")
      refute html =~ ~s(phx-click="conforming_classes")
    end

    test "Required methods opens a popover listing the protocol's contract", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_open_definition", %{"class" => "Printable"})
      html = render_click(view, "required_methods", %{})

      assert html =~ "Required methods"
      # The popover head shows the protocol name; a row shows the required selector,
      # navigable to its Implementors.
      assert html =~ "Printable"
      assert html =~ "printOn:"
      assert html =~ ~s(phx-click="nav_required_open")
    end

    test "Conforming classes opens a popover listing conforming classes", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_open_definition", %{"class" => "Printable"})
      html = render_click(view, "conforming_classes", %{})

      assert html =~ "Conforming classes"
      # A conforming-class row is navigable to that class's definition.
      assert html =~ "Counter"
      assert html =~ ~s(phx-click="nav_open_class")
    end

    test "clicking a conforming class opens its definition pane", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_open_definition", %{"class" => "Printable"})
      render_click(view, "conforming_classes", %{})
      html = render_click(view, "nav_open_class", %{"class" => "Counter"})

      # The conforming class is now the open definition tab, and the popover closed.
      assert html =~ "Counter"
      refute html =~ ~s(phx-click="nav_open_class")
    end

    test "clicking a required method opens its Implementors", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_open_definition", %{"class" => "Printable"})
      render_click(view, "required_methods", %{})

      html =
        render_click(view, "nav_required_open", %{"selector" => "printOn:", "side" => "instance"})

      # The popover now shows Implementors (the stub returns an empty list, so the
      # heading flips and the empty-state message renders).
      assert html =~ "Implementors"
    end

    test "an observer also gets the protocol action row (read capability)", %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      html = render_click(view, "browser_open_definition", %{"class" => "Printable"})

      assert html =~ ~s(phx-click="required_methods")
      assert html =~ ~s(phx-click="conforming_classes")
    end
  end
end

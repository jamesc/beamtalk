# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceGotoDefinitionTest do
  @moduledoc """
  Integration test for Ctrl/Cmd-click go-to-definition (BT-2666): the editor's
  `cm_goto.js` extension fires a `goto_definition` event carrying the clicked
  token + the line-prefix; the LiveView resolves it against the live image and
  opens the target, reusing the BT-2495 senders/implementors plumbing.

  Resolution (mirroring the LSP `definition_provider.rs` order):

    * a known CLASS name → its `:def` definition tab
    * a SELECTOR send → its implementor(s): one opens the method tab directly,
      several open the Implementors popover, none flashes a brief no-op.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`); `set_implementors/2` seeds the implementor
  rows. No `:workspace` tag, so this runs in the bare `mix test` lane.
  """
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias BtAttachWeb.StubWorkspaceClient

  setup do
    Application.put_env(:bt_attach, :workspace_client, StubWorkspaceClient)

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
      StubWorkspaceClient.stop_state(2_000)
    end)

    {:ok, _} = StubWorkspaceClient.start_state()

    :ok
  end

  defp owner_conn(conn) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "alice", "groups" => ["beamtalk-owners"]},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  # A `nav-query` implementor site row in the wire shape the LiveView opens.
  defp site(class, selector, side \\ false) do
    %{
      "class" => class,
      "class_side" => side,
      "method" => selector,
      "line" => 1,
      "source_file" => "src/#{String.downcase(class)}.bt",
      "source_origin" => "project"
    }
  end

  describe "go-to-definition (BT-2666)" do
    test "clicking a class name opens its class definition tab", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # `Counter` is a known class in the stub's class tree.
      html = render_hook(view, "goto_definition", %{"token" => "Counter", "code" => "Counter"})

      # The class-definition tab is now open (the def-tab seeds `Object subclass:
      # Counter` from the stub's `browse_class_definition`).
      assert html =~ "Counter"
      assert html =~ "subclass"
    end

    test "clicking a selector with one implementor opens that method tab", %{conn: conn} do
      StubWorkspaceClient.set_implementors("increment", [site("Counter", "increment")])

      {:ok, view, _html} = live(owner_conn(conn), "/")

      html =
        render_hook(view, "goto_definition", %{
          "token" => "increment",
          "code" => "self increment"
        })

      # The single implementor opens directly as an editable method tab — no
      # popover, the method's tab id is `method:Counter:instance:increment`.
      assert html =~ "increment"
      refute html =~ ~s(phx-click="nav_open")
    end

    test "a keyword selector resolves the full selector from the line prefix", %{conn: conn} do
      StubWorkspaceClient.set_implementors("at:put:", [site("Dictionary", "at:put:")])

      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Clicking the `at` part of `dict at: k put: v` resolves the whole `at:put:`.
      html =
        render_hook(view, "goto_definition", %{
          "token" => "at",
          "code" => "dict at: k put:"
        })

      assert html =~ "at:put:"
    end

    test "a selector with several implementors opens the Implementors popover", %{conn: conn} do
      StubWorkspaceClient.set_implementors("value", [
        site("Counter", "value"),
        site("Ledger", "value")
      ])

      {:ok, view, _html} = live(owner_conn(conn), "/")

      # The popover renders within an open editor tab (where modifier-click
      # happens), so open a method tab first — the realistic mod-click context.
      render_click(view, "browser_select_method", %{
        "class" => "Counter",
        "side" => "instance",
        "selector" => "increment"
      })

      html = render_hook(view, "goto_definition", %{"token" => "value", "code" => "self value"})

      # Ambiguous → the shared BT-2495 popover lists both implementors as
      # navigable rows (the `nav_open` open path).
      assert html =~ "Implementors"
      assert html =~ ~s(phx-click="nav_open")
      assert html =~ "Counter"
      assert html =~ "Ledger"
    end

    test "an unresolved symbol is a graceful no-op flash", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # No implementors seeded and not a known class → the no-op flash, no popover.
      html =
        render_hook(view, "goto_definition", %{"token" => "noSuchThing", "code" => "noSuchThing"})

      assert html =~ "No definition found."
      refute html =~ ~s(phx-click="nav_open")
    end

    test "a class name takes priority over a same-named selector", %{conn: conn} do
      # Seed a selector with the SAME name as a class — the class wins (LSP order).
      StubWorkspaceClient.set_implementors("Counter", [site("Other", "Counter")])

      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_hook(view, "goto_definition", %{"token" => "Counter", "code" => "Counter"})

      # The class definition opened, not the selector's implementor popover.
      assert html =~ "subclass"
      refute html =~ ~s(phx-click="nav_open")
    end

    test "an empty token is ignored", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_hook(view, "goto_definition", %{"token" => "", "code" => ""})

      # No flash, no popover — a bare no-op (the JS never sends an empty token, but
      # the server guards it anyway).
      refute html =~ "No definition found."
      refute html =~ ~s(phx-click="nav_open")
    end
  end
end

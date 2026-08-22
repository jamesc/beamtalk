# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceKeyboardShortcutsTest do
  @moduledoc """
  Workspace-wide keyboard chords: the cockpit root carries a
  window-scoped `KeyboardShortcuts` binding so Esc (and ⌘W in the desktop shell)
  closes the focused editor tab and ⌘/ toggles the documentation disclosure. The
  browser-side key handling itself is JS (`keyboard_shortcuts.js`, exercised in
  the `:playwright` lane); these tests cover the server contract — the rendered
  `data-shortcuts` map and the `tab_close_active` event the chords push.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`). No `:workspace` tag, so this runs in the
  bare `mix test` lane.
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

  test "the cockpit root declares the workspace-wide shortcut bindings", %{conn: conn} do
    {:ok, _view, html} = live(owner_conn(conn), "/")

    # The root element carries the window-scoped KeyboardShortcuts hook with
    # Esc/⌘W → tab_close_active and ⌘/ → toggle_doc. The JSON rides the DOM
    # HTML-escaped, so assert on the chord/action substrings.
    assert html =~ ~s(id="workspace-shortcuts")
    assert html =~ ~s(phx-hook="KeyboardShortcuts")
    assert html =~ ~s(data-scope="window")
    assert html =~ "escape"
    assert html =~ "mod+w"
    assert html =~ "mod+/"
    assert html =~ "tab_close_active"
    assert html =~ "toggle_doc"
  end

  test "tab_close_active closes the focused editor tab", %{conn: conn} do
    {:ok, view, _html} = live(owner_conn(conn), "/")

    html = render_click(view, "browser_open_definition", %{"class" => "Counter"})
    assert html =~ "Counter ▸ def"

    html = render_click(view, "tab_close_active", %{})
    refute html =~ "Counter ▸ def"
  end

  test "tab_close_active closes only the focused tab, refocusing its neighbour", %{conn: conn} do
    {:ok, view, _html} = live(owner_conn(conn), "/")

    render_click(view, "browser_open_definition", %{"class" => "Counter"})
    html = render_click(view, "browser_open_definition", %{"class" => "Ledger"})
    assert html =~ "Counter ▸ def"
    assert html =~ "Ledger ▸ def"

    # Ledger is the active tab; Esc closes it and Counter survives.
    html = render_click(view, "tab_close_active", %{})
    refute html =~ "Ledger ▸ def"
    assert html =~ "Counter ▸ def"
  end

  test "tab_close_active with no open tab is a no-op", %{conn: conn} do
    {:ok, view, _html} = live(owner_conn(conn), "/")

    html = render_click(view, "tab_close_active", %{})
    assert html =~ ~s(id="workspace-shortcuts")
  end
end

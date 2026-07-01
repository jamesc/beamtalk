# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceDerivedBadgeTest do
  @moduledoc """
  Integration test for the System Browser's `derived` method badge (BT-2714): the
  method list surfaces a `.derived-tag` on a selector whose xref `source_status`
  is `synthetic` — a compiler-derived method (Value accessors, `with<Field>:`
  setters, keyword constructors, actor `new`/`spawn`) — so the synthesized magic
  is visibly distinct from a hand-written method rather than indistinguishable.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `browse_protocols` reports `Counter`'s
  `value` selector as `synthetic` and `increment` as `indexed`. No `:workspace`
  tag, so this runs in the bare `mix test` lane.
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

  describe "System Browser derived-method badge (BT-2714)" do
    test "a synthetic selector shows a `derived` badge, a hand-written one does not",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      # BT-2591: the class tree loads off the mount, so await the async fold before
      # navigating into a class.
      render_async(view, 5_000)

      # Selecting the class loads its protocols + method list (a synchronous
      # server-side round-trip through the stubbed `browse_protocols`).
      html =
        view
        |> element(~s(div[phx-click="browser_select_class"][phx-value-class="Counter"]))
        |> render_click()

      frag = Floki.parse_fragment!(html)

      # The synthetic `value` accessor row carries the `derived` badge.
      derived = Floki.find(frag, ~s(div[phx-value-selector="value"] .derived-tag))
      assert derived != []
      assert Floki.text(derived) |> String.trim() == "derived"

      # The hand-written `increment` method row does not.
      assert Floki.find(frag, ~s(div[phx-value-selector="increment"] .derived-tag)) == []
    end
  end
end

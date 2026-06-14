# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceFlushBadgeTest do
  @moduledoc """
  End-to-end integration test for the flush-clears-unflushed-badge flow
  (BT-2545 / BT-2554).

  Exercises the full compile → `unflushed` shown → flush → `unflushed` cleared
  flow through the LiveView against a **flushable** method using a fully-stubbed
  workspace client (`BtAttachWeb.StubWorkspaceClient`). No `:workspace` tag, so
  this runs in the bare `mix test` lane.

  The stub client's `save_method/3` adds an entry to the shared change log,
  and `flush/0` clears it — so the flush actually "writes" the method from the
  perspective of the reconcile logic (`flushed_method_keys/3` /
  `clear_disk_differs/2`).

  No reliance on the pure-unit-test shortcut for the integration path (the unit
  tests in `workspace_live_reconcile_test.exs` stay as fast complementary
  coverage).
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
      BtAttachWeb.StubWorkspaceClient.stop_state(2_000)
      Application.delete_env(:bt_attach, :workspace_client)
      Application.delete_env(:bt_attach, :oidc)
      Application.delete_env(:bt_attach, :session_ttl_secs)
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

  defp eventually(fun), do: eventually(fun, 20)
  defp eventually(fun, 0), do: fun.()
  defp eventually(fun, retries) do
    case fun.() do
      true -> true
      _ -> Process.sleep(50); eventually(fun, retries - 1)
    end
  end

  describe "flush clears the unflushed badge (BT-2545 / BT-2554)" do
    test "compile → unflushed shown → flush → unflushed cleared", %{conn: conn} do
      suffix = System.unique_integer([:positive])
      class = "FlushBadge#{suffix}"

      # 1. Define a class via eval (first mount)
      {:ok, view, _html} = live(owner_conn(conn), "/")

      class_src = """
      Actor subclass: #{class}
        state: value = 0

        increment => self.value := self.value + 1

        value => self.value
      """

      view |> form("#eval-form") |> render_submit(%{expr: class_src})

      # 2. Remount so the system browser picks up the new class
      {:ok, view, _html} = live(owner_conn(conn), "/")
      assert eventually(fn -> render(view) =~ class end)

      # 3. Browse the class and open the method as a tab
      view |> element(~s(div[phx-value-class="#{class}"])) |> render_click()

      open_html =
        view
        |> element(~s(div[phx-value-selector="increment"]))
        |> render_click()

      assert open_html =~ "in image"

      # 4. Save a new body — compile_clean flips disk_differs to true
      save_html =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => class,
          "selector" => "increment",
          "source" => "increment => self.value := self.value + 2",
          "tab" => "method:#{class}:instance:increment"
        })

      assert save_html =~ "Saved increment on #{class}"
      assert save_html =~ "unflushed"

      # 5. Flush — the badge must clear
      flush_html =
        view
        |> element("button[phx-click='flush']")
        |> render_click()

      assert flush_html =~ "Flushed"
      refute flush_html =~ "unflushed"
    end
  end
end

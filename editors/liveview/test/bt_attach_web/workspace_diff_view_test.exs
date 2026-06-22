# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceDiffViewTest do
  @moduledoc """
  Render tests for the structured diff view (BT-2636): the Changes-pane row-LHS
  disclosure caret toggling a coloured, gutter-aligned diff, and the same shared
  `unified_diff/1` renderer reused in the Git pane. Driven through the full
  LiveView stack against the stubbed workspace client.
  """
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias BtAttachWeb.StubWorkspaceClient

  @diff " increment => self.value\n-  self.value := self.value + 1\n+  self.value := self.value + 2"

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

  defp eventually(fun, retries \\ 20)
  defp eventually(fun, 0), do: fun.()

  defp eventually(fun, retries) do
    if fun.() do
      true
    else
      Process.sleep(50)
      eventually(fun, retries - 1)
    end
  end

  describe "Changes pane row-LHS disclosure (BT-2636)" do
    test "the leading caret toggles a structured, coloured diff beneath the row", %{conn: conn} do
      # Seed a net-vs-disk diff for the change the save below records.
      StubWorkspaceClient.set_change_diff("Counter", "increment", @diff)

      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Record a change so the Changes table has a row carrying the seeded diff.
      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{
        "class" => "Counter",
        "selector" => "increment",
        "source" => "increment => self.value := self.value + 2",
        "tab" => "method:Counter:instance:increment"
      })

      # Open the Changes tab and confirm the leading caret is present and collapsed.
      changes_html = render_click(view, "dock_tab", %{"tab" => "changes"})
      assert changes_html =~ "diff-toggle"
      assert changes_html =~ "›"
      # Collapsed: no structured diff body yet.
      refute changes_html =~ "diff-add"

      # Click the caret — the structured diff expands beneath the row.
      expanded =
        view
        |> element("button.diff-toggle")
        |> render_click()

      assert expanded =~ "▼"
      assert expanded =~ ~s(class="bt-diff")
      assert expanded =~ "diff-add"
      assert expanded =~ "diff-del"
      assert expanded =~ "diff-ctx"
      assert expanded =~ "diff-gutter"

      # Clicking again collapses it.
      collapsed = view |> element("button.diff-toggle") |> render_click()
      refute collapsed =~ "diff-add"
    end
  end

  describe "Git pane reuses the shared renderer (BT-2636)" do
    test "a worktree diff renders as structured coloured lines, not a raw pre", %{conn: conn} do
      StubWorkspaceClient.set_git_status(
        {:ok,
         %{
           branch: "main",
           upstream: nil,
           ahead: 0,
           behind: 0,
           files: [%{path: "src/counter.bt", index: :unmodified, worktree: :modified}]
         }}
      )

      StubWorkspaceClient.set_git_diff({:ok, %{worktree: @diff, staged: ""}})

      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "dock_tab", %{"tab" => "git"})
      assert eventually(fn -> render(view) =~ "src/counter.bt" end)

      diff_html =
        view
        |> element(~s(button[phx-click="git_diff"][phx-value-path="src/counter.bt"]))
        |> render_click()

      assert diff_html =~ "diff-add"
      assert diff_html =~ "diff-del"
      assert diff_html =~ "diff-gutter"
    end

    test "an empty git diff keeps the 'No textual diff' note", %{conn: conn} do
      StubWorkspaceClient.set_git_status(
        {:ok,
         %{
           branch: "main",
           upstream: nil,
           ahead: 0,
           behind: 0,
           files: [%{path: "bin.dat", index: :unmodified, worktree: :modified}]
         }}
      )

      StubWorkspaceClient.set_git_diff({:ok, %{worktree: "", staged: ""}})

      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "dock_tab", %{"tab" => "git"})
      assert eventually(fn -> render(view) =~ "bin.dat" end)

      diff_html =
        view
        |> element(~s(button[phx-click="git_diff"][phx-value-path="bin.dat"]))
        |> render_click()

      assert diff_html =~ "No textual diff"
      refute diff_html =~ "diff-add"
    end
  end
end

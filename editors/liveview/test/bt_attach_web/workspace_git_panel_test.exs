# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceGitPanelTest do
  @moduledoc """
  Integration tests for the cockpit git panel's non-blocking load and the
  autoflush-gated post-save refresh (BT-2590), driven through the full LiveView
  stack against the fully-stubbed workspace client (`StubWorkspaceClient`). No
  `:workspace` tag, so this runs in the bare `mix test` lane.

  Covers the two BT-2590 acceptance criteria:

    * S1 — opening the Git tab loads its status/log off-socket via `start_async`,
      so the panel renders its "Loading…" placeholder and then fills in from the
      async result without the load ever blocking the socket. We assert the panel
      reaches the loaded state.
    * S2 — with autoflush off, an in-memory-only class save does NOT shell out to
      git (no `git_status` call recorded); with autoflush on, it does.
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

  defp eventually(fun), do: eventually(fun, 20)
  defp eventually(fun, 0), do: fun.()

  defp eventually(fun, retries) do
    if fun.() do
      true
    else
      Process.sleep(50)
      eventually(fun, retries - 1)
    end
  end

  describe "S1: non-blocking git load (start_async, BT-2590)" do
    test "opening the Git tab loads status off-socket and renders it", %{conn: conn} do
      StubWorkspaceClient.set_git_status(
        {:ok, %{branch: "feature", upstream: nil, ahead: 0, behind: 0, files: []}}
      )

      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Open the Git tab — kicks off the async load. The handler returns
      # immediately (the socket is not blocked); the rendered HTML may still show
      # the loading placeholder at this instant.
      render_click(view, "dock_tab", %{"tab" => "git"})

      # The async load resolves via handle_async and the branch lands in the panel.
      assert eventually(fn -> render(view) =~ "feature" end)
      assert Process.alive?(view.pid)
    end

    test "a malformed status degrades to a panel error, not a LiveView crash", %{conn: conn} do
      # An unexpected (non-{:ok,_}/{:error,_}) status reply must be folded into a
      # panel error by the total `apply_git_status/2` clause — the LiveView stays
      # alive rather than crashing handle_async on an unmatched shape.
      StubWorkspaceClient.set_git_status(:bogus)

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "git"})

      # The async result resolves and the panel leaves its "Loading…" state with an
      # error note instead, and the process survives.
      assert eventually(fn -> render(view) =~ "unexpected_git_status" end)
      assert Process.alive?(view.pid)
    end

    test "an independently-failed log surfaces an error beside a good status", %{conn: conn} do
      # Status succeeds but the log read fails on its own — the panel must not show
      # a valid branch beside a silently-empty commit list. `apply_git_log/2`
      # surfaces the log error because status left `git_error` clear.
      StubWorkspaceClient.set_git_status(
        {:ok, %{branch: "feature", upstream: nil, ahead: 0, behind: 0, files: []}}
      )

      StubWorkspaceClient.set_git_log({:error, :git_timeout})

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "git"})

      assert eventually(fn -> render(view) =~ "git_timeout" end)
      assert Process.alive?(view.pid)
    end
  end

  describe "S2: autoflush-gated post-save git refresh (BT-2590)" do
    test "with autoflush off, a class save does not shell out to git", %{conn: conn} do
      StubWorkspaceClient.set_autoflush(false)

      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Open the Git tab and let its initial load settle, then clear the recorded
      # calls so the assertion sees only what the save triggers.
      render_click(view, "dock_tab", %{"tab" => "git"})
      assert eventually(fn -> render(view) =~ "main" end)
      StubWorkspaceClient.clear_calls()

      define_a_class(view)

      # Autoflush off ⇒ the save patched the image only ⇒ no git refresh.
      refute_eventually(fn ->
        Enum.any?(StubWorkspaceClient.calls(), &(elem(&1, 0) == :git_status))
      end)
    end

    test "with autoflush on, a class save refreshes the git panel", %{conn: conn} do
      StubWorkspaceClient.set_autoflush(true)

      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "dock_tab", %{"tab" => "git"})
      assert eventually(fn -> render(view) =~ "main" end)
      StubWorkspaceClient.clear_calls()

      define_a_class(view)

      # Autoflush on ⇒ the save wrote to disk ⇒ the panel refreshes via start_async.
      assert eventually(fn ->
               Enum.any?(StubWorkspaceClient.calls(), &(elem(&1, 0) == :git_status))
             end)
    end
  end

  # Drive the new-class save path (`dispatch_new_class`) — one of the two saves
  # BT-2590 gates the git refresh behind. We push the `new_class` event directly
  # (the modal lives behind the ＋ affordance), exercising the same handler. The
  # modal now carries a plain class name + superclass (BT-2645); the definition is
  # synthesized server-side.
  defp define_a_class(view) do
    class = "GitGate#{System.unique_integer([:positive])}"
    render_hook(view, "new_class", %{"name" => class, "superclass" => "Actor"})
  end

  # Assert that `fun` stays false across the retry window (the negative of
  # `eventually/1`) — used to prove the git shell-out never happens.
  defp refute_eventually(fun, retries \\ 10) do
    Enum.each(1..retries, fn _ ->
      refute fun.()
      Process.sleep(30)
    end)
  end
end

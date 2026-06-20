# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceGitRevertRefreshTest do
  @moduledoc """
  Integration tests for BT-2598: a content-mutating git op (revert) must
  propagate to the live image and to open editor windows, not just the git
  panel. Driven through the full LiveView stack against the fully-stubbed
  workspace client (`StubWorkspaceClient`). No `:workspace` tag, so this runs in
  the bare `mix test` lane.

  Covers the four acceptance criteria:

    * AC1 — `subscribe_classes` is wired at mount (the class-lifecycle push the
      window refresh rides).
    * AC2 — a git revert reloads the affected module from disk into the image
      (`reload_file` is invoked).
    * AC3 — after a revert, an open method-editor tab for the affected class
      shows the reverted (disk) content without a manual refresh.
    * AC4 — a revert of a file with pending unflushed ChangeLog edits is blocked
      with a clear warning (live work not silently discarded).
    * AC5 — a `ClassLoaded` push refreshes open windows + the browser on any
      source change (a flush from another session, an external edit reload).
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

  # Open the Counter#increment method as an editable tab via the System Browser
  # select event — the same path a browser click drives. The stub serves the
  # method's image source for the tab.
  defp open_increment_tab(view) do
    render_hook(view, "browser_select_method", %{
      "class" => "Counter",
      "side" => "instance",
      "selector" => "increment"
    })
  end

  describe "AC1: subscribe_classes wired at mount" do
    test "the cockpit subscribes to the classes push stream on connect", %{conn: conn} do
      {:ok, _view, _html} = live(owner_conn(conn), "/")

      assert eventually(fn ->
               Enum.any?(StubWorkspaceClient.calls(), &(elem(&1, 0) == :subscribe_classes))
             end)
    end
  end

  describe "AC2/AC3: git revert reloads the image and refreshes open windows" do
    test "a revert invokes reload_file for the reverted path", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      StubWorkspaceClient.clear_calls()

      render_hook(view, "git_revert", %{"path" => "src/counter.bt"})

      assert eventually(fn ->
               Enum.any?(
                 StubWorkspaceClient.calls(),
                 &match?({:reload_file, "src/counter.bt"}, &1)
               )
             end)

      # The working-tree revert itself still happened (the git op ran).
      assert Enum.any?(
               StubWorkspaceClient.calls(),
               &match?({:git_revert_file, "src/counter.bt"}, &1)
             )
    end

    test "an open clean method tab is re-read after a revert (no manual refresh)", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Open the increment tab — clean, image == disk, so no "unflushed" badge.
      html = open_increment_tab(view)
      refute html =~ "unflushed"

      # An image that differs from disk (disk changed under it) — distinct from an
      # unflushed in-memory edit, so the revert is NOT blocked. Re-open so the tab
      # carries the "unflushed" (image-differs-from-disk) badge.
      StubWorkspaceClient.seed_disk_differs("Counter", "increment")
      html = open_increment_tab(view)
      assert html =~ "unflushed"

      # Revert the file. The stub's `reload_file` resets the image to disk (clears
      # the divergence), and the revert's refresh re-reads the OPEN tab without a
      # manual re-open — so the "unflushed" badge clears on its own.
      render_hook(view, "git_revert", %{"path" => "src/counter.bt"})

      assert eventually(fn -> not (render(view) =~ "unflushed") end)

      # And the reload actually ran (image == disk after revert).
      assert Enum.any?(
               StubWorkspaceClient.calls(),
               &match?({:reload_file, "src/counter.bt"}, &1)
             )
    end

    test "a revert whose reload fails still reverts and surfaces a note", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # The working tree reverts, but reloading the file into the image fails
      # (e.g. the reverted HEAD body has a compile error). The git op still ran;
      # the panel surfaces a clear note rather than silently leaving image != disk.
      StubWorkspaceClient.set_reload_file({:error, "boom"})

      html = render_hook(view, "git_revert", %{"path" => "src/counter.bt"})

      assert eventually(fn -> render(view) =~ "reload failed" end)
      assert html =~ "reload failed" or render(view) =~ "reload failed"

      assert Enum.any?(
               StubWorkspaceClient.calls(),
               &match?({:git_revert_file, "src/counter.bt"}, &1)
             )
    end
  end

  describe "AC4: pending-edit guard" do
    test "a revert is blocked when the file has unflushed in-memory edits", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Save a method on Counter (src/counter.bt) → a pending ChangeLog entry.
      render_hook(view, "save_method", %{
        "class" => "Counter",
        "selector" => "increment",
        "source" => "increment => self.value := self.value + 7"
      })

      StubWorkspaceClient.clear_calls()

      # Reverting that file must be blocked, with a clear warning — and NO git
      # revert nor reload may run (live work is not silently discarded).
      html = render_hook(view, "git_revert", %{"path" => "src/counter.bt"})

      assert html =~ "unflushed in-memory edits"
      assert html =~ "Flush"

      refute Enum.any?(StubWorkspaceClient.calls(), &(elem(&1, 0) == :git_revert_file))
      refute Enum.any?(StubWorkspaceClient.calls(), &(elem(&1, 0) == :reload_file))
    end

    test "a revert of a clean (no pending edits) file proceeds", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      StubWorkspaceClient.clear_calls()

      # No pending edits for subprocess → the revert runs.
      render_hook(view, "git_revert", %{"path" => "src/subprocess.bt"})

      assert eventually(fn ->
               Enum.any?(StubWorkspaceClient.calls(), &(elem(&1, 0) == :git_revert_file))
             end)
    end
  end

  describe "AC5: class-change push refreshes open windows + browser" do
    test "a ClassLoaded push re-reads an open clean method tab", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Open the tab while the image matches disk — no badge.
      html = open_increment_tab(view)
      refute html =~ "unflushed"

      # An out-of-band image change lands AFTER the tab is open (another session's
      # patch, an external edit reloaded): the served method now differs from disk.
      # The class-lifecycle push must re-read the open clean tab so its badge
      # appears WITHOUT the user re-focusing the tab.
      StubWorkspaceClient.seed_disk_differs("Counter", "increment")

      send(view.pid, {:beamtalk_announcement, make_ref(), :ClassLoaded, :handler, %{}})

      assert eventually(fn -> render(view) =~ "unflushed" end)
    end

    test "a ClassRemoved push is handled without crashing the LiveView", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      send(view.pid, {:beamtalk_announcement, make_ref(), :ClassRemoved, :handler, %{}})

      # The process survives and the refresh re-pulls the browser class list.
      assert eventually(fn -> Process.alive?(view.pid) end)
      assert render(view) =~ "Counter"
    end
  end
end

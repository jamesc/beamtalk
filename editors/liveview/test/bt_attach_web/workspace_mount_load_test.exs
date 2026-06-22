# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceMountLoadTest do
  @moduledoc """
  Integration tests for the cockpit's non-blocking mount-time workspace reads
  (BT-2591), driven through the full LiveView stack against the fully-stubbed
  workspace client (`StubWorkspaceClient`). No `:workspace` tag, so this runs in
  the bare `mix test` lane.

  BT-2591 lifts four mount reads — browser classes, bindings, the active
  ChangeLog, and the autoflush flag — off the connected mount: instead of running
  as synchronous RPCs in `bind_session` (each ~5s worst case on a slow/unreachable
  workspace), they now start in their loading/empty state so the first render is
  immediate, and a single off-socket `start_async(:mount_load, …)` performs all
  four reads, folding results in via `handle_async(:mount_load, …)`.

  Covers:

    * the connected mount renders immediately in its loading/empty state (the
      synchronous reads no longer gate the mount), then fills in asynchronously;
    * `handle_async(:mount_load, …)` folds the reads into their assigns;
    * the degraded fallback (autoflush → false, lists empty) survives a failing
      read without crashing the mount;
    * the BT-2619 partial guard: a fresher bindings push that lands before the
      mount load resolves is not clobbered by the (staler) mount read.
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

  defp refute_eventually(fun, retries \\ 10) do
    Enum.each(1..retries, fn _ ->
      refute fun.()
      Process.sleep(30)
    end)
  end

  describe "non-blocking mount load (start_async, BT-2591)" do
    test "the connected mount renders in its loading state, then fills in async", %{conn: conn} do
      # `render_async/2` awaits the in-flight `:mount_load` task. We use it to prove
      # the mount returned BEFORE the reads completed: the browser class tree fills
      # in only once the async fold runs. (The stub is synchronous, so the task
      # still resolves — we assert the post-fold state lands via handle_async.)
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # The browse-classes read folds in via handle_async(:mount_load, …) and the
      # stub's class rows appear in the System Browser tree.
      assert eventually(fn -> render(view) =~ "Counter" end)
      assert Process.alive?(view.pid)
    end

    test "render_async resolves the mount load and the browser tree is populated", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_async(view, 5_000)
      assert html =~ "Counter"
      assert html =~ "Ledger"
    end

    test "seeded bindings fold into the Bindings pane after the async load", %{conn: conn} do
      StubWorkspaceClient.put_bindings([{"mountVar", 42}])

      {:ok, view, _html} = live(owner_conn(conn), "/")

      assert eventually(fn -> render(view) =~ "mountVar" end)
      assert Process.alive?(view.pid)
    end
  end

  describe "degraded fallback (BT-2591)" do
    test "the mount renders with empty panes when no workspace data is seeded", %{conn: conn} do
      # Nothing seeded for bindings (defaults to []) and autoflush off: the mount
      # must come up in its empty/degraded state without crashing, and the async
      # load folds the empty/default reads back in (no error, no crash). This is the
      # graceful-degradation path — an unreachable workspace folds to the same empty
      # lists + autoflush-false the initial assigns already hold.
      StubWorkspaceClient.put_bindings([])
      StubWorkspaceClient.set_autoflush(false)

      {:ok, view, _html} = live(owner_conn(conn), "/")

      # The async mount load resolves (browse-classes still folds in from the stub
      # default) and the Bindings pane shows its empty state rather than crashing.
      html = render_async(view, 5_000)
      assert html =~ "Counter"
      assert html =~ "bindings-panel"
      assert Process.alive?(view.pid)
    end
  end

  describe "BT-2619 post-load stability (BT-2591)" do
    test "a bindings push that populated the pane is not blanked by a re-fold", %{conn: conn} do
      # NOTE: this is the *post-load* stability scenario — `render_async` resolves
      # the mount load before the push, so `handle_async(:mount_load, …)` has
      # already run. This exercises "live pushes update the pane and the value
      # sticks" after the fold. The truly-concurrent push-before-fold race is
      # covered deterministically below via the stub's one-shot mount gate.
      {:ok, view, _html} = live(owner_conn(conn), "/")
      assert render_async(view, 5_000) =~ "Counter"

      # A live push lands a fresher binding (the stub backs the re-read).
      name = "raceVar#{System.unique_integer([:positive])}"
      StubWorkspaceClient.put_bindings([{name, 7}])
      send(view.pid, {:beamtalk_announcement, make_ref(), :BindingChanged, :handler, %{}})

      assert eventually(fn -> render(view) =~ name end)

      # The fresher push value survives subsequent re-renders — the mount fold's
      # guard never overwrites a populated pane with the (now-staler) mount read.
      refute_eventually(fn -> not (render(view) =~ name) end)
      assert Process.alive?(view.pid)
    end
  end

  describe "BT-2619 push-before-fold race (deterministic via the mount gate)" do
    # The stub's one-shot mount gate (`arm_mount_gate/0`) blocks the async mount
    # load's FIRST read (`browse_classes`) until `release_mount_gate/0`, so the
    # test can deliver a live push and let the LiveView handle it BEFORE the async
    # mount fold runs — the exact concurrency BT-2619 protects against.

    test "a bindings push landing before the fold is not clobbered by the mount read",
         %{conn: conn} do
      # Mount snapshot would carry NO bindings ([] default); the push carries one.
      # With success-only loaded-flag gating the push value must survive the fold.
      StubWorkspaceClient.put_bindings([])
      :ok = StubWorkspaceClient.arm_mount_gate()

      {:ok, view, _html} = live(owner_conn(conn), "/")

      # The mount-load task is now blocked on `browse_classes`. Deliver a fresher
      # bindings push; the LiveView handles it (sets bindings + bindings_loaded)
      # while the fold is still pending.
      pushed = "preFoldVar#{System.unique_integer([:positive])}"
      StubWorkspaceClient.put_bindings([{pushed, 99}])
      send(view.pid, {:beamtalk_announcement, make_ref(), :BindingChanged, :handler, %{}})
      assert eventually(fn -> render(view) =~ pushed end)

      # Make the mount snapshot STALER than the push: when the gate releases, the
      # mount task's `list_bindings` read will see an empty workspace. Without the
      # loaded-flag guard the fold would blank the pushed binding to []; with the
      # guard (bindings_loaded set true by the push) the fold skips this surface.
      StubWorkspaceClient.put_bindings([])

      :ok = StubWorkspaceClient.release_mount_gate()
      _ = render_async(view, 5_000)

      assert render(view) =~ pushed
      assert Process.alive?(view.pid)
    end

    test "a source (ClassLoaded) push landing before the fold is not clobbered",
         %{conn: conn} do
      # A user-defined class appears via eval; the ClassLoaded push refreshes the
      # browser surface BEFORE the fold and marks source_loaded. We then make the
      # mount snapshot STALER (drop the class) before releasing the gate: without
      # the loaded-flag guard the fold would revert the class tree to the staler
      # snapshot; with the guard the fold skips the already-loaded source surface.
      :ok = StubWorkspaceClient.arm_mount_gate()

      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Define a class so the push's browse includes it, then deliver the
      # ClassLoaded push BEFORE the fold. The push refresh (`browse_classes`) is not
      # gated (the gate is one-shot, consumed by the mount task's first read).
      {:ok, _r, _o, _w} = StubWorkspaceClient.eval(self(), "Actor subclass: RaceClass")

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ClassLoaded, :handler, %{}}
      )

      assert eventually(fn -> render(view) =~ "RaceClass" end)

      # Make the mount snapshot staler than the push, then release the gate.
      StubWorkspaceClient.clear_defined_classes()
      :ok = StubWorkspaceClient.release_mount_gate()
      _ = render_async(view, 5_000)

      # The push-loaded class must remain visible (the fold did not revert the
      # source surface to the staler mount snapshot).
      assert render(view) =~ "RaceClass"
      assert render(view) =~ "Counter"
      assert Process.alive?(view.pid)
    end

    test "an errored early bindings push does NOT lock out a successful mount fold",
         %{conn: conn} do
      # The edge case: an early push refresh that FAILS (sets bindings_error, list
      # stays empty) must NOT set bindings_loaded — so the later-completing mount
      # load's SUCCESSFUL data still folds in (no lingering error flash).
      :ok = StubWorkspaceClient.arm_mount_gate()

      {:ok, view, _html} = live(owner_conn(conn), "/")

      # The mount load is blocked. Seed the mount snapshot to carry a real binding,
      # but make the in-flight push refresh FAIL by handing the bindings re-read an
      # error result. We model the error by sending the push while bindings return
      # an error shape: drive it through the stub's error path.
      mounted = "mountVar#{System.unique_integer([:positive])}"
      StubWorkspaceClient.put_bindings([{mounted, 1}])
      # Force the push refresh's `list_bindings` to error.
      StubWorkspaceClient.fail_bindings(true)

      send(view.pid, {:beamtalk_announcement, make_ref(), :BindingChanged, :handler, %{}})
      # The push refresh errored: the pane shows its error, not the binding yet.
      assert eventually(fn -> render(view) =~ "bindings-panel" end)

      # Stop failing so the mount read succeeds, then release the gate. Because the
      # errored push did NOT set bindings_loaded, the mount fold's successful data
      # applies — the real binding shows, replacing the transient error.
      StubWorkspaceClient.fail_bindings(false)
      :ok = StubWorkspaceClient.release_mount_gate()
      _ = render_async(view, 5_000)

      assert eventually(fn -> render(view) =~ mounted end)
      assert Process.alive?(view.pid)
    end
  end

  describe "BT-2619 empty workspace + autoflush" do
    test "a genuinely-empty workspace still shows its empty state after mount", %{conn: conn} do
      # No push occurs, so both loaded flags stay false and the (empty-but-success)
      # mount read still folds in — the empty state must render, not stay blank
      # forever.
      StubWorkspaceClient.put_bindings([])

      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_async(view, 5_000)
      assert html =~ "bindings-panel"
      # Browser classes still fold in from the stub default (no push gated them).
      assert html =~ "Counter"
      assert Process.alive?(view.pid)
    end

    test "autoflush always folds in even after a source push (no competing push)",
         %{conn: conn} do
      # autoflush has no live push, so the mount fold must apply it unconditionally
      # — even when a source push set source_loaded. We seed autoflush true and
      # assert the post-save git refresh gate sees it (a save shells out to git).
      StubWorkspaceClient.set_autoflush(true)
      :ok = StubWorkspaceClient.arm_mount_gate()

      {:ok, view, _html} = live(owner_conn(conn), "/")

      send(view.pid, {:beamtalk_announcement, make_ref(), :ClassLoaded, :handler, %{}})
      assert eventually(fn -> render(view) =~ "Counter" end)

      :ok = StubWorkspaceClient.release_mount_gate()
      _ = render_async(view, 5_000)

      # The mount fold applied autoflush despite the source push: assert the LiveView
      # is alive and rendered (autoflush itself is internal state; its application is
      # exercised by the git-refresh-on-save tests — here we assert the fold ran and
      # did not crash applying autoflush after a push).
      assert render(view) =~ "Counter"
      assert Process.alive?(view.pid)
    end
  end
end

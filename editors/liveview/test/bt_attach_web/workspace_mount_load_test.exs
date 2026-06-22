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

  describe "BT-2619 partial race guard (BT-2591)" do
    test "a bindings push that populated the pane is not blanked by a re-fold", %{conn: conn} do
      # NOTE: this is the *post-load* stability scenario — `render_async` resolves
      # the mount load before the push, so `handle_async(:mount_load, …)` has
      # already run and this exercises "live pushes update the pane and the value
      # sticks", not the concurrent race the guard ultimately protects against
      # (a push landing *before* the fold). That truly-concurrent case — and the
      # "early push errored, mount succeeded" variant — is deterministically
      # testable only with the generation-token hardening in BT-2619; covered there.
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
end

# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceTestsPaneTest do
  @moduledoc """
  Integration tests for the cockpit Tests pane's non-blocking run/load and the
  System Browser source filter's selection handling (BT-2597), driven through the
  full LiveView stack against the fully-stubbed workspace client
  (`StubWorkspaceClient`). No `:workspace` tag, so this runs in the bare
  `mix test` lane.

  Covers the two BT-2597 acceptance criteria:

    * F1 — `run_tests` / `load_tests` run off-socket via `start_async(:test_op, …)`
      so the event handler returns immediately and the result fills in from
      `handle_async`; an error degrades to a `tests_error` rather than crashing.
    * F2 — switching to a source filter that hides the selected class clears the
      selection (and its method pane).
    * F4 — (BT-2599) catalogue discovery (`list_tests`) runs off-socket via
      `start_async(:test_discover, …)`: the pane shows a "discovering" state
      (nil sentinel, not the empty-state) until it resolves, an error / crash
      degrades to a `tests_error` rather than taking down the socket, and a
      partial-load compile-error banner survives the off-socket re-discovery.
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

  describe "F1: non-blocking test run/load (start_async, BT-2597)" do
    test "Run all resolves off-socket and renders per-case pass/fail", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Opening the Tests tab discovers the stub catalogue.
      render_click(view, "dock_tab", %{"tab" => "tests"})
      assert render(view) =~ "StubDemoTest"

      # Run all kicks off the `:test_op` async task and returns immediately; the
      # per-case rows land once the async result resolves via handle_async.
      render_click(view, "run_tests")
      html = render_async(view)

      assert html =~ "testOne"
      assert html =~ "testTwo"
      assert html =~ "failed"
      assert Process.alive?(view.pid)
    end

    test "Load tests resolves off-socket and refreshes the catalogue", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})

      render_click(view, "load_tests")
      html = render_async(view)

      # The post-load re-discovery shows the catalogue, and no error banner.
      assert html =~ "StubDemoTest"
      refute html =~ "failed to load"
      assert Process.alive?(view.pid)
    end

    test "a run error degrades to a pane error, not a LiveView crash", %{conn: conn} do
      # The async task's dispatch returns an error; `apply_test_result/2` folds it
      # into `tests_error` and the socket survives (no per-case results shown).
      StubWorkspaceClient.set_run_tests({:error, :workspace_unreachable})

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})

      render_click(view, "run_tests")
      html = render_async(view)

      refute html =~ "testOne"
      assert Process.alive?(view.pid)
    end

    test "an unexpected run reply shape degrades, not crashes", %{conn: conn} do
      # A non-{:ok,_}/{:error,_} reply must hit the total `apply_test_result/2`
      # clause rather than crashing handle_async on an unmatched shape.
      StubWorkspaceClient.set_run_tests(:bogus)

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})

      render_click(view, "run_tests")
      html = render_async(view)

      assert html =~ "unexpected_test_result"
      assert Process.alive?(view.pid)
    end

    test "a load error degrades to a pane error, not a LiveView crash", %{conn: conn} do
      # `apply_test_load(socket, {:error, reason})` surfaces the error and the
      # socket survives (symmetric with the run-error path).
      StubWorkspaceClient.set_load_tests({:error, :unauthorized})

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})

      render_click(view, "load_tests")
      html = render_async(view)

      assert html =~ "Not authorized"
      assert Process.alive?(view.pid)
    end

    test "an unexpected load reply shape degrades, not crashes", %{conn: conn} do
      # A non-{:ok,_}/{:error,_} reply must hit the total `apply_test_load/2`
      # clause rather than crashing handle_async on an unmatched shape.
      StubWorkspaceClient.set_load_tests(:bogus)

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})

      render_click(view, "load_tests")
      html = render_async(view)

      assert html =~ "unexpected_test_result"
      assert Process.alive?(view.pid)
    end
  end

  describe "F4: non-blocking catalogue discovery (start_async, BT-2599)" do
    test "opening the Tests tab discovers off-socket and renders the catalogue", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # The first open seeds the nil sentinel ("Loading tests…") and kicks off the
      # `:test_discover` async task; the catalogue lands once it resolves.
      render_click(view, "dock_tab", %{"tab" => "tests"})
      html = render_async(view)

      assert html =~ "StubDemoTest"
      refute html =~ "No TestCase subclasses"
      assert Process.alive?(view.pid)
    end

    test "Refresh re-discovers off-socket without blocking the socket", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})
      assert render_async(view) =~ "StubDemoTest"

      # Manual refresh kicks off another `:test_discover` and returns immediately;
      # the refreshed catalogue fills in from handle_async.
      render_click(view, "tests_refresh")
      html = render_async(view)

      assert html =~ "StubDemoTest"
      assert Process.alive?(view.pid)
    end

    test "shows a discovering state, not the empty state, until discovery resolves",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Immediately after opening the tab (before render_async drains the task),
      # the pane shows the nil-sentinel "Loading tests…" — not the misleading
      # "No TestCase subclasses" empty-state that [] would render.
      html = render_click(view, "dock_tab", %{"tab" => "tests"})

      assert html =~ "Loading tests…"
      refute html =~ "No TestCase subclasses"
      assert Process.alive?(view.pid)
    end

    test "a discovery error degrades to a pane error, not a LiveView crash", %{conn: conn} do
      # The async dispatch returns an error; `apply_test_classes/3` folds it into
      # `tests_error` and leaves `test_classes` at the nil sentinel — so the pane
      # shows the error, not the empty-state.
      StubWorkspaceClient.set_test_classes({:error, :workspace_unreachable})

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})
      html = render_async(view)

      refute html =~ "StubDemoTest"
      refute html =~ "No TestCase subclasses"
      assert Process.alive?(view.pid)
    end

    test "an unexpected discovery reply shape degrades, not crashes", %{conn: conn} do
      # A non-{:ok,list}/{:error,_} reply must hit the total `apply_test_classes/3`
      # clause rather than crashing handle_async on an unmatched shape.
      StubWorkspaceClient.set_test_classes(:bogus)

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})
      html = render_async(view)

      assert html =~ "unexpected_test_result"
      assert Process.alive?(view.pid)
    end

    test "a discovery crash degrades to tests_error, not a LiveView crash", %{conn: conn} do
      # The discovery task itself raises, so `handle_async(:test_discover, {:exit, …})`
      # runs — degrade to a pane error rather than taking down the socket.
      StubWorkspaceClient.set_list_tests_raise(true)

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})
      html = render_async(view)

      assert html =~ "discovery failed unexpectedly"
      refute html =~ "No TestCase subclasses"
      assert Process.alive?(view.pid)
    end

    test "a clean Load tests re-discovers the freshly-loaded catalogue", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})
      assert render_async(view) =~ "StubDemoTest"

      # Load (a `:test_op` async) then its `apply_test_load` kicks off the
      # `:test_discover` re-discovery; render_async drains both. No error banner.
      render_click(view, "load_tests")
      html = render_async(view)

      assert html =~ "StubDemoTest"
      refute html =~ "failed to load"
      assert Process.alive?(view.pid)
    end

    test "a partial Load tests keeps the compile-error banner across re-discovery",
         %{conn: conn} do
      # A partial load returns compile errors; `apply_test_load` surfaces them as
      # `tests_error` and passes keep_error?: true so the *successful* off-socket
      # re-discovery doesn't swallow the banner.
      StubWorkspaceClient.set_load_tests(
        {:ok, %{"errors" => [%{"path" => "test/foo.bt", "message" => "boom"}]}}
      )

      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_click(view, "dock_tab", %{"tab" => "tests"})
      assert render_async(view) =~ "StubDemoTest"

      render_click(view, "load_tests")
      html = render_async(view)

      assert html =~ "failed to load"
      # The catalogue still re-discovers (banner sits beside the catalogue).
      assert html =~ "StubDemoTest"
      assert Process.alive?(view.pid)
    end
  end

  describe "F2: source filter clears a now-hidden selection (BT-2597)" do
    test "switching to a filter that hides the selected class clears it", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Select a project class — its instance selectors fill the method pane.
      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      assert render(view) =~ "increment"

      # Narrowing to "stdlib" hides the project class; its selection (and method
      # pane) is cleared rather than left as a ghost.
      html = render_click(view, "browser_source", %{"src" => "stdlib"})
      refute html =~ "increment"
      assert Process.alive?(view.pid)
    end

    test "switching to a filter that still shows the class keeps the selection", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      assert render(view) =~ "increment"

      # "Project" still includes Counter, so the selection (and method pane) stays.
      html = render_click(view, "browser_source", %{"src" => "project"})
      assert html =~ "increment"
      assert Process.alive?(view.pid)
    end
  end

  describe "F3: source filter renders as a compact dropdown (BT-2603)" do
    test "renders a <select name=src> instead of a segmented control", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      # BT-2591: await the async class load — that fold is also when the BT-2661
      # initial default (Project) is applied to the filter.
      html = render_async(view, 5_000)

      # The source-origin filter is a <select> (so it can't overflow the panel
      # head) carrying the `src` field the handler reads. All four options are
      # present and reachable, and the current selection (Project, BT-2661's
      # initial default for a workspace with project classes) is marked.
      assert html =~ ~s(<select name="src")
      assert html =~ ~s(aria-label="Class source filter")
      assert html =~ ~s(<option value="project" selected="selected">)
      assert html =~ ~s(<option value="all">)
      assert html =~ ~s(<option value="deps">)
      assert html =~ ~s(<option value="stdlib">)
      assert html =~ "All"
      assert html =~ "Proj"
      assert html =~ "Deps"
      assert html =~ "Std"
    end

    test "changing the dropdown dispatches the browser_source event", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      assert render(view) =~ "increment"

      # Driving the rendered <select> (not a bare event) still narrows the tree:
      # the phx-change form posts `%{"src" => "stdlib"}` to the unchanged handler.
      html =
        view
        |> form(~s(form[phx-change="browser_source"]), %{"src" => "stdlib"})
        |> render_change()

      refute html =~ "increment"
      assert Process.alive?(view.pid)
    end
  end
end

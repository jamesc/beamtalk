# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceSupervisorInspectTest do
  @moduledoc """
  Integration tests for supervisor-aware inspection (BT-2634, ADR 0095): a
  supervisor inspects to its CHILDREN / supervision tree — drillable child rows
  (class, pid, status), NOT an empty/erroring instance-field table — driven
  through the full LiveView stack against the fully-stubbed workspace client
  (`StubWorkspaceClient`). No `:workspace` tag, so this runs in the bare
  `mix test` lane.

  Covers the BT-2634 acceptance criteria reachable through the stub:

    * a supervisor renders a children view (child class + pid + kind/childCount),
    * each Beamtalk child row is drillable (a nested supervisor re-inspects as its
      own supervisor; breadcrumbs lead back),
    * a foreign OTP child renders but is NOT drillable,
    * a live supervisor with no running children shows a clear empty state,
    * a dead/unreachable supervisor degrades to a clear error (no crash),
    * live-tracking (field-flash / pid-stats / watch) is NOT armed for a
      supervisor — the pid-stat chips never appear.

  (The runtime `child_handles/1` reshaping — real `which_children`, live handles,
  DynamicSupervisor parity, dead-supervisor error — is covered by the
  `beamtalk_process_navigation` EUnit suite against a real supervision tree.)
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

  # A live, pid-backed supervisor handle (the test pid is always alive, so the
  # real `Workspace.inspectable?/1` `is_pid/1` gate passes and the bindings pane
  # offers the Inspect button).
  defp sup_handle(class \\ :AppSup),
    do: {:beamtalk_supervisor, class, :"Elixir.#{class}", self()}

  describe "supervisor inspection renders the children / supervision tree (BT-2634)" do
    test "inspecting a supervisor shows its children, not an empty field table", %{conn: conn} do
      StubWorkspaceClient.put_bindings([{"app_sup", sup_handle()}])

      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = view |> element("button[phx-value-name='app_sup']") |> render_click()

      # The Inspector head is up, and the body is the children view — the stub's
      # representative tree (a Counter actor, a nested WorkerPool supervisor, a
      # foreign logger process) — NOT the "no fields" / empty actor table.
      assert html =~ "Inspecting"
      assert html =~ "Counter"
      assert html =~ "WorkerPool"
      assert html =~ "logger_std_h"
      # The child rows carry kind + pid so the supervision structure is legible.
      assert html =~ "beamtalkActor"
      assert html =~ "beamtalkSupervisor"
      assert html =~ "&lt;0.310.0&gt;" or html =~ "<0.310.0>"
      # A nested supervisor child shows its live child count.
      assert html =~ "2 children"
    end

    test "head chips reflect the supervisor identity (class + pid)", %{conn: conn} do
      StubWorkspaceClient.put_bindings([{"app_sup", sup_handle(:MyAppSup)}])

      {:ok, view, _html} = live(owner_conn(conn), "/")
      html = view |> element("button[phx-value-name='app_sup']") |> render_click()

      # The supervisor's class chip is shown (target_info supervisor clause).
      assert html =~ "MyAppSup"
      # Live pid-stat chips are NOT shown — a supervisor is not field-tracked
      # (no pid-stats poll, no watch), so the process-health chips never populate.
      refute html =~ "mailbox"
      refute html =~ "reductions"
    end
  end

  describe "child drill-through and breadcrumbs (ADR 0095 reference-following)" do
    test "drilling a nested supervisor child re-inspects it as its own supervisor", %{conn: conn} do
      StubWorkspaceClient.put_bindings([{"app_sup", sup_handle()}])

      {:ok, view, _html} = live(owner_conn(conn), "/")
      view |> element("button[phx-value-name='app_sup']") |> render_click()

      # The nested supervisor child (WorkerPool) is the 2nd drillable row (index 1).
      # Drilling follows its live {:beamtalk_supervisor, …} handle, re-inspecting it
      # as its own supervisor — whose stub children include the same representative
      # tree, so the drilled view again shows Counter + WorkerPool children.
      html = view |> element("tr.drillable[phx-value-index='1']") |> render_click()

      assert html =~ "Inspecting"
      # Breadcrumbs lead back to the supervisor we drilled from (more than one
      # level deep → the crumb trail renders).
      assert html =~ "app_sup"
      assert html =~ "WorkerPool"
      # Drilled into a supervisor, we again see a children view.
      assert html =~ "Counter"
    end

    test "a foreign OTP child renders but is not drillable", %{conn: conn} do
      StubWorkspaceClient.put_bindings([{"app_sup", sup_handle()}])

      {:ok, view, _html} = live(owner_conn(conn), "/")
      html = view |> element("button[phx-value-name='app_sup']") |> render_click()

      # The foreign logger row is present...
      assert html =~ "logger_std_h"
      # ...but only the two Beamtalk children (Counter, WorkerPool) are drillable.
      assert html |> drillable_row_count() == 2
    end
  end

  describe "degraded states (BT-2634 graceful handling)" do
    test "a live supervisor with no children shows a clear empty state", %{conn: conn} do
      StubWorkspaceClient.put_bindings([{"empty_sup", sup_handle(:EmptySup)}])

      {:ok, view, _html} = live(owner_conn(conn), "/")
      html = view |> element("button[phx-value-name='empty_sup']") |> render_click()

      assert html =~ "Inspecting"
      assert html =~ "no fields or children"
      # Nothing crashed and no child rows were rendered.
      assert html |> drillable_row_count() == 0
    end

    test "a dead/unreachable supervisor degrades to an error, not a crash", %{conn: conn} do
      StubWorkspaceClient.put_bindings([{"dead_sup", sup_handle(:DeadSup)}])

      {:ok, view, _html} = live(owner_conn(conn), "/")
      html = view |> element("button[phx-value-name='dead_sup']") |> render_click()

      # The inspect failed (stale_handle) → the pane shows the error rather than a
      # half-rendered head, and the LiveView survives.
      assert html =~ "not alive" or html =~ "stale"
      assert Process.alive?(view.pid)
    end
  end

  # Count the drillable child rows in the rendered Inspector children table.
  defp drillable_row_count(html) do
    html
    |> String.split("tr class=\"drillable\"")
    |> length()
    |> Kernel.-(1)
  end
end

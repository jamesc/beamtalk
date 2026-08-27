# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.InspectorTest do
  @moduledoc """
  Direct unit tests for `BtAttachWeb.Live.Inspector` (BT-3291), driving its
  `handle_event/3` / `handle_info/2` clauses and pure helpers against a
  hand-built `%Phoenix.LiveView.Socket{}` and the fully-stubbed workspace
  client (`BtAttachWeb.StubWorkspaceClient`, BT-2554) — no full LiveView
  mount, no real workspace node.

  These cover the branches BT-3291's acceptance criteria calls out
  specifically: drill/crumb navigation, freeze/unfreeze, field-flash
  coalescing, the owner-only poke RBAC gate, and window position reset —
  previously reachable only through the `:workspace`-tagged full-stack
  `WorkspaceLiveTest`, which is excluded from the default `mix test` lane.
  """
  use ExUnit.Case, async: false

  alias BtAttachWeb.Live.Inspector
  alias BtAttachWeb.StubWorkspaceClient

  setup do
    Application.put_env(:bt_attach, :workspace_client, StubWorkspaceClient)
    {:ok, _} = StubWorkspaceClient.start_state()

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  # A bare socket carrying exactly the assigns Inspector's functions read —
  # the subset of `WorkspaceLive.bind_session/3`'s init relevant to the
  # docked Inspector + floating windows. `role: :owner` by default (most
  # tests aren't about RBAC); override per test.
  defp base_socket(overrides \\ %{}) do
    assigns =
      %{
        __changed__: %{},
        current_user: nil,
        role: :owner,
        session_id: "sess-1",
        session_pid: self(),
        inspector_mode: "docked",
        inspect_target: nil,
        inspect_rows: [],
        inspect_crumbs: [],
        inspect_error: nil,
        inspect_watch: nil,
        inspect_stats: nil,
        inspect_frozen: false,
        flash_gen: 0,
        refresh_pending: false,
        poke_result: nil,
        poke_error: nil,
        windows: [],
        window_z: 10,
        next_window_id: 1
      }
      |> Map.merge(overrides)

    %Phoenix.LiveView.Socket{assigns: assigns}
  end

  # A synthetic live-object term backed by the test process's own pid — safe
  # to "subscribe"/"inspect" against the stub client without a real node.
  defp object_term(pid \\ self()), do: {:beamtalk_object, TestClass, TestClass, pid}

  describe "pure classifiers" do
    test "term_kind/1 maps object refs to \"ref\" and scalars to their chip" do
      assert Inspector.term_kind(object_term()) == "ref"
      assert Inspector.term_kind(42) == "int"
      assert Inspector.term_kind("hi") == "string"
      assert Inspector.term_kind(true) == "bool"
    end

    test "stat_status/mailbox/reductions read the pid_stats snapshot, nil when absent" do
      assert Inspector.stat_status(%{"status" => "running"}) == "running"
      assert Inspector.stat_status(nil) == nil
      assert Inspector.stat_mailbox(%{"queue_depth" => 0}) == 0
      assert Inspector.stat_mailbox(nil) == nil
      assert Inspector.stat_reductions(%{"reductions" => 1_234_567}) == "1,234,567"
      assert Inspector.stat_reductions(%{}) == nil
    end

    test "pokeable?/1 requires a single named-binding crumb with a valid identifier" do
      refute Inspector.pokeable?(%{inspect_crumbs: []})
      refute Inspector.pokeable?(%{inspect_crumbs: [%{label: "→ result"}]})
      assert Inspector.pokeable?(%{inspect_crumbs: [%{label: "counter"}]})
    end
  end

  describe "inspect / drill / crumb navigation" do
    test "inspect resolves a session binding and starts a fresh drill breadcrumb" do
      pid = self()
      term = object_term(pid)
      StubWorkspaceClient.put_bindings([{"counter", term}])

      {:noreply, socket} =
        Inspector.handle_event("inspect", %{"name" => "counter"}, base_socket())

      assert socket.assigns.inspect_target.label == "counter"
      assert socket.assigns.inspect_crumbs == [%{label: "counter", term: term}]
      assert socket.assigns.inspect_error == nil
      # `track_object/2` armed the live subscription for a pid-backed head.
      assert socket.assigns.inspect_watch == term
    end

    test "inspecting an unknown binding surfaces an error, not a crash" do
      StubWorkspaceClient.put_bindings([])

      {:noreply, socket} = Inspector.handle_event("inspect", %{"name" => "ghost"}, base_socket())

      assert socket.assigns.inspect_error =~ "binding not found"
    end

    test "drill follows an object-valued row by index, extending the breadcrumb" do
      inner = object_term()
      row = %{term: inner, name: "child", drillable: true}

      socket =
        base_socket(%{
          inspect_crumbs: [%{label: "counter", term: object_term()}],
          inspect_rows: [row]
        })

      {:noreply, socket} = Inspector.handle_event("drill", %{"index" => "0"}, socket)

      assert socket.assigns.inspect_target.label == "child"
      assert length(socket.assigns.inspect_crumbs) == 2
      assert List.last(socket.assigns.inspect_crumbs) == %{label: "child", term: inner}
    end

    test "drill with a malformed index is a no-op, not a crash" do
      socket = base_socket(%{inspect_rows: [%{term: object_term(), name: "x"}]})

      {:noreply, result} = Inspector.handle_event("drill", %{"index" => "not-a-number"}, socket)
      assert result == socket

      {:noreply, result2} = Inspector.handle_event("drill", %{}, socket)
      assert result2 == socket
    end

    test "crumb walks the breadcrumb back to an earlier level, truncating the trail" do
      root = object_term()
      mid = object_term()

      socket =
        base_socket(%{
          inspect_crumbs: [
            %{label: "counter", term: root},
            %{label: "child", term: mid}
          ]
        })

      {:noreply, socket} = Inspector.handle_event("crumb", %{"index" => "0"}, socket)

      assert socket.assigns.inspect_target.label == "counter"
      assert socket.assigns.inspect_crumbs == [%{label: "counter", term: root}]
    end
  end

  describe "close_inspector" do
    test "resets target/rows/crumbs/error and unfreezes, leaving show_inspector untouched" do
      socket =
        base_socket(%{
          inspect_target: %{label: "counter"},
          inspect_rows: [%{name: "x"}],
          inspect_crumbs: [%{label: "counter", term: object_term()}],
          inspect_error: "boom",
          inspect_frozen: true
        })

      {:noreply, socket} = Inspector.handle_event("close_inspector", %{}, socket)

      assert socket.assigns.inspect_target == nil
      assert socket.assigns.inspect_rows == []
      assert socket.assigns.inspect_crumbs == []
      assert socket.assigns.inspect_error == nil
      assert socket.assigns.inspect_frozen == false
    end
  end

  describe "freeze / unfreeze (BT-2492 live tracking)" do
    test "freeze_toggle flips inspect_frozen when nothing is watched" do
      {:noreply, socket} = Inspector.handle_event("freeze_toggle", %{}, base_socket())
      assert socket.assigns.inspect_frozen == true

      {:noreply, socket} = Inspector.handle_event("freeze_toggle", %{}, socket)
      assert socket.assigns.inspect_frozen == false
    end

    test "unfreezing a watched head re-arms the subscription and refreshes it" do
      term = object_term()

      socket =
        base_socket(%{
          inspect_frozen: true,
          inspect_crumbs: [%{label: "counter", term: term}],
          flash_gen: 0
        })

      {:noreply, socket} = Inspector.handle_event("freeze_toggle", %{}, socket)

      assert socket.assigns.inspect_frozen == false
      assert socket.assigns.inspect_watch == term
      # `refresh_inspector/2` bumped the flash generation on the catch-up read.
      assert socket.assigns.flash_gen == 1
    end
  end

  describe "owner-only poke RBAC gate (BT-2492)" do
    test "an Observer's poke is refused by the facade, not sent" do
      socket =
        base_socket(%{
          role: :observer,
          inspect_crumbs: [%{label: "counter", term: object_term()}]
        })

      {:noreply, socket} = Inspector.handle_event("poke", %{"message" => "increment"}, socket)

      assert socket.assigns.poke_result == nil
      assert socket.assigns.poke_error =~ "Not authorized"
    end

    test "an Owner's poke against a named binding succeeds" do
      socket =
        base_socket(%{
          role: :owner,
          inspect_crumbs: [%{label: "counter", term: object_term()}]
        })

      {:noreply, socket} = Inspector.handle_event("poke", %{"message" => "increment"}, socket)

      assert socket.assigns.poke_error == nil
      assert socket.assigns.poke_result =~ "→ "
    end

    test "poke with no addressable binding (a drilled field) reports it can't send" do
      socket =
        base_socket(%{
          inspect_crumbs: [
            %{label: "counter", term: object_term()},
            %{label: "child", term: object_term()}
          ]
        })

      {:noreply, socket} = Inspector.handle_event("poke", %{"message" => "increment"}, socket)

      assert socket.assigns.poke_error =~ "Can only send to a bound object"
    end

    test "poke with a blank message is a local validation error" do
      socket = base_socket(%{inspect_crumbs: [%{label: "counter", term: object_term()}]})

      {:noreply, socket} = Inspector.handle_event("poke", %{"message" => "   "}, socket)

      assert socket.assigns.poke_error == "Enter a message to send."
    end
  end

  describe "field-flash coalescing ({:object_changed, …} → :do_object_refresh)" do
    test "the first push schedules a deferred refresh and sets refresh_pending" do
      pid = self()
      term = object_term(pid)
      socket = base_socket(%{inspect_watch: term, inspect_frozen: false})

      {:noreply, socket} = Inspector.handle_info({:object_changed, pid, %{}}, socket)

      assert socket.assigns.refresh_pending == true
      assert_receive :do_object_refresh, 200
    end

    test "a burst collapses into the queued refresh (no second timer)" do
      pid = self()
      term = object_term(pid)
      socket = base_socket(%{inspect_watch: term, inspect_frozen: false, refresh_pending: true})

      {:noreply, socket} = Inspector.handle_info({:object_changed, pid, %{}}, socket)

      assert socket.assigns.refresh_pending == true
      refute_received :do_object_refresh
    end

    test "a frozen pane ignores the push" do
      pid = self()
      term = object_term(pid)
      socket = base_socket(%{inspect_watch: term, inspect_frozen: true})

      {:noreply, socket} = Inspector.handle_info({:object_changed, pid, %{}}, socket)

      assert socket.assigns.refresh_pending == false
      refute_received :do_object_refresh
    end

    test "the deferred refresh re-reads the object and bumps flash_gen" do
      term = object_term()
      socket = base_socket(%{inspect_watch: term, refresh_pending: true, flash_gen: 0})

      {:noreply, socket} = Inspector.handle_info(:do_object_refresh, socket)

      assert socket.assigns.refresh_pending == false
      assert socket.assigns.flash_gen == 1
    end

    test "a stale :do_object_refresh with nothing watched is a harmless no-op" do
      socket = base_socket(%{inspect_watch: nil, refresh_pending: true})

      {:noreply, socket} = Inspector.handle_info(:do_object_refresh, socket)

      assert socket.assigns.refresh_pending == false
    end
  end

  describe "floating windows: open / drill / poke / close / focus" do
    test "inspect in float mode opens a window instead of driving the docked pane" do
      term = object_term()
      StubWorkspaceClient.put_bindings([{"counter", term}])
      socket = base_socket(%{inspector_mode: "float"})

      {:noreply, socket} = Inspector.handle_event("inspect", %{"name" => "counter"}, socket)

      assert socket.assigns.inspect_target == nil
      assert [win] = socket.assigns.windows
      assert win.id == "win-1"
      assert win.crumbs == [%{label: "counter", term: term}]
      assert socket.assigns.next_window_id == 2
    end

    test "window_close drops the window and releases its watch" do
      win = %{
        id: "win-1",
        watch: nil,
        crumbs: [],
        rows: [],
        target: nil,
        error: nil,
        stats: nil,
        frozen: false,
        refresh_pending: false,
        flash_gen: 0,
        poke_result: nil,
        poke_error: nil,
        x: 0,
        y: 0,
        z: 10
      }

      socket = base_socket(%{windows: [win]})
      {:noreply, socket} = Inspector.handle_event("window_close", %{"id" => "win-1"}, socket)

      assert socket.assigns.windows == []
    end

    test "window_close for an unknown id is a no-op" do
      socket = base_socket()
      {:noreply, result} = Inspector.handle_event("window_close", %{"id" => "nope"}, socket)
      assert result == socket
    end

    test "window_focus bumps the clicked window's z above the current max" do
      win_a = %{id: "a", z: 10, watch: nil}
      win_b = %{id: "b", z: 11, watch: nil}
      socket = base_socket(%{windows: [win_a, win_b], window_z: 11})

      {:noreply, socket} = Inspector.handle_event("window_focus", %{"id" => "a"}, socket)

      assert socket.assigns.window_z == 12
      assert Enum.find(socket.assigns.windows, &(&1.id == "a")).z == 12
    end
  end

  describe "window position reset (BT-2527 #4)" do
    test "re-cascades every open window back onto the default on-screen ladder" do
      windows = [
        %{id: "a", x: 999, y: 999},
        %{id: "b", x: -5, y: 42}
      ]

      socket = base_socket(%{windows: windows})
      {:noreply, socket} = Inspector.handle_event("window_reset_positions", %{}, socket)

      assert [%{x: 120, y: 96}, %{x: 148, y: 124}] =
               Enum.map(socket.assigns.windows, &Map.take(&1, [:x, :y]))
    end
  end

  describe "window_moved clamps client-reported coordinates" do
    test "negative/float/non-numeric drag coordinates never place a window off into NaN-land" do
      win = %{id: "a", x: 0, y: 0}
      socket = base_socket(%{windows: [win]})

      {:noreply, socket} =
        Inspector.handle_event("window_moved", %{"id" => "a", "x" => -5, "y" => 12.7}, socket)

      moved = Enum.find(socket.assigns.windows, &(&1.id == "a"))
      assert moved.x == 0
      assert moved.y == 12
    end
  end

  describe "set_inspector_mode" do
    test "flips between docked and float, ignoring an invalid value" do
      {:noreply, socket} =
        Inspector.handle_event("set_inspector_mode", %{"mode" => "float"}, base_socket())

      assert socket.assigns.inspector_mode == "float"

      {:noreply, result} =
        Inspector.handle_event("set_inspector_mode", %{"mode" => "bogus"}, socket)

      assert result == socket
    end
  end

  describe "inspector_windows/1 function component" do
    test "renders the tidy-windows control only when a window is open" do
      import Phoenix.LiveViewTest

      empty = render_component(&Inspector.inspector_windows/1, windows: [], role: :owner)
      refute empty =~ "insp-tidy"

      win = %{
        id: "win-1",
        label: "counter",
        target: nil,
        crumbs: [],
        rows: [],
        error: nil,
        stats: nil,
        frozen: false,
        flash_gen: 0,
        poke_result: nil,
        poke_error: nil,
        x: 0,
        y: 0,
        z: 10
      }

      html = render_component(&Inspector.inspector_windows/1, windows: [win], role: :owner)
      assert html =~ "insp-tidy"
      assert html =~ ~s(id="inspector-window-win-1")
    end
  end
end

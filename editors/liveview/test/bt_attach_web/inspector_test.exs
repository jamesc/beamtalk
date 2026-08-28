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
  alias BtAttachWeb.WorkspaceLive

  @inspector_source Path.expand("../../lib/bt_attach_web/live/inspector.ex", __DIR__)

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
  defp object_term(pid \\ self()), do: {:beamtalk_object, :TestClass, :TestClass, pid}

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
      refute_receive :do_object_refresh, 200
    end

    test "a frozen pane ignores the push" do
      pid = self()
      term = object_term(pid)
      socket = base_socket(%{inspect_watch: term, inspect_frozen: true})

      {:noreply, socket} = Inspector.handle_info({:object_changed, pid, %{}}, socket)

      assert socket.assigns.refresh_pending == false
      refute_receive :do_object_refresh, 200
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

  describe "window_drill into a non-empty field row (BT-3303)" do
    test "follows an object-valued field into a further inspect, extending the window's crumbs" do
      pid = self()
      root = {:beamtalk_object, :Counter, :Counter, pid}
      child = {:beamtalk_object, :Child, :Child, pid}
      StubWorkspaceClient.seed_inspect_value("Counter", %{"count" => 1, "child" => child})
      StubWorkspaceClient.seed_inspect_value("Child", %{"name" => "kid"})

      socket = Inspector.open_window_for_term(base_socket(), "counter", root)
      assert [win] = socket.assigns.windows
      # `field_rows/1` sorts by name: "child" < "count".
      assert Enum.map(win.rows, & &1.name) == ["child", "count"]
      child_index = Enum.find_index(win.rows, &(&1.name == "child"))
      assert Enum.at(win.rows, child_index).drillable

      {:noreply, socket} =
        Inspector.handle_event(
          "window_drill",
          %{"id" => win.id, "index" => to_string(child_index)},
          socket
        )

      assert [win] = socket.assigns.windows
      assert win.target.label == "child"
      assert List.last(win.crumbs) == %{label: "child", term: child}
      assert Enum.map(win.rows, & &1.name) == ["name"]
    end

    test "window_drill with a malformed index or unknown window id is a no-op" do
      root = object_term()
      StubWorkspaceClient.seed_inspect_value("TestClass", %{"count" => 1})
      socket = Inspector.open_window_for_term(base_socket(), "counter", root)

      {:noreply, result} =
        Inspector.handle_event("window_drill", %{"id" => "nope", "index" => "0"}, socket)

      assert result == socket

      [win] = socket.assigns.windows

      {:noreply, result2} =
        Inspector.handle_event(
          "window_drill",
          %{"id" => win.id, "index" => "not-a-number"},
          socket
        )

      assert result2 == socket
    end
  end

  describe "reference-counted unsubscribe across windows (BT-3303)" do
    test "closing one of two windows on the same pid keeps the subscription alive for the other, closing both releases it" do
      pid = self()
      term = object_term(pid)

      socket =
        base_socket()
        |> Inspector.open_window_for_term("a", term)
        |> Inspector.open_window_for_term("b", term)

      assert [win_a, win_b] = socket.assigns.windows
      assert win_a.watch == term
      assert win_b.watch == term

      StubWorkspaceClient.clear_calls()

      {:noreply, socket} = Inspector.handle_event("window_close", %{"id" => win_a.id}, socket)

      refute {:unsubscribe_object_changes, term, pid} in StubWorkspaceClient.calls()
      assert [remaining] = socket.assigns.windows
      assert remaining.id == win_b.id
      assert remaining.watch == term

      {:noreply, socket} = Inspector.handle_event("window_close", %{"id" => win_b.id}, socket)

      assert {:unsubscribe_object_changes, term, pid} in StubWorkspaceClient.calls()
      assert socket.assigns.windows == []
    end

    test "the docked pane watching the same pid also keeps a window's subscription alive" do
      pid = self()
      term = object_term(pid)
      StubWorkspaceClient.put_bindings([{"counter", term}])

      socket = Inspector.open_window_for_term(base_socket(), "counter", term)
      {:noreply, socket} = Inspector.handle_event("inspect", %{"name" => "counter"}, socket)

      assert socket.assigns.inspect_watch == term
      [win] = socket.assigns.windows

      StubWorkspaceClient.clear_calls()

      {:noreply, socket} = Inspector.handle_event("window_close", %{"id" => win.id}, socket)

      refute {:unsubscribe_object_changes, term, pid} in StubWorkspaceClient.calls()
      assert socket.assigns.windows == []

      # Closing the docked pane too — now nothing watches `term` — actually
      # releases it.
      {:noreply, socket} = Inspector.handle_event("close_inspector", %{}, socket)

      assert {:unsubscribe_object_changes, term, pid} in StubWorkspaceClient.calls()
      assert socket.assigns.inspect_watch == nil
    end
  end

  describe "window_poke (BT-3303)" do
    test "an owner's window poke sends eval, renders the real result, and refreshes the live window" do
      pid = self()
      term = object_term(pid)
      StubWorkspaceClient.seed_inspect_value("TestClass", %{"count" => 1})

      socket = Inspector.open_window_for_term(base_socket(), "counter", term)
      [win] = socket.assigns.windows
      assert win.watch == term
      assert win.flash_gen == 0

      {:noreply, socket} =
        Inspector.handle_event(
          "window_poke",
          %{"id" => win.id, "message" => "Actor subclass: Gadget"},
          socket
        )

      [win] = socket.assigns.windows
      assert win.poke_error == nil
      assert win.poke_result =~ "→ "
      assert win.poke_result =~ "Gadget"
      # A live (watched) window re-reads after a successful poke (send_window_poke),
      # bumping its flash generation.
      assert win.flash_gen == 1
    end

    test "window_poke with no addressable binding (a drilled window) reports it can't send" do
      pid = self()
      root = object_term(pid)
      child = object_term(pid)
      StubWorkspaceClient.seed_inspect_value("TestClass", %{"child" => child})

      socket = Inspector.open_window_for_term(base_socket(), "counter", root)
      [win] = socket.assigns.windows
      child_index = Enum.find_index(win.rows, &(&1.name == "child"))

      {:noreply, socket} =
        Inspector.handle_event(
          "window_drill",
          %{"id" => win.id, "index" => to_string(child_index)},
          socket
        )

      {:noreply, socket} =
        Inspector.handle_event(
          "window_poke",
          %{"id" => win.id, "message" => "increment"},
          socket
        )

      [win] = socket.assigns.windows
      assert win.poke_result == nil
      assert win.poke_error =~ "Can only send to a bound object"
    end
  end

  describe "window_freeze (BT-3303)" do
    test "freezing a live window drops its subscription and holds the snapshot; unfreezing re-arms and catches up" do
      pid = self()
      term = object_term(pid)
      StubWorkspaceClient.seed_inspect_value("TestClass", %{"count" => 1})

      socket = Inspector.open_window_for_term(base_socket(), "counter", term)
      [win] = socket.assigns.windows
      assert win.watch == term
      assert win.frozen == false

      StubWorkspaceClient.clear_calls()

      {:noreply, socket} = Inspector.handle_event("window_freeze", %{"id" => win.id}, socket)

      [win] = socket.assigns.windows
      assert win.frozen == true
      assert win.watch == nil
      assert {:unsubscribe_object_changes, term, pid} in StubWorkspaceClient.calls()

      {:noreply, socket} = Inspector.handle_event("window_freeze", %{"id" => win.id}, socket)

      [win] = socket.assigns.windows
      assert win.frozen == false
      assert win.watch == term
      # Unfreezing re-reads to catch up, bumping the flash generation.
      assert win.flash_gen == 1
    end
  end

  describe "pid_stats parameterization (BT-3303)" do
    test "a window's head chips read a seeded pid_stats snapshot for its class" do
      pid = self()
      term = {:beamtalk_object, :Instance, :Instance, pid}
      StubWorkspaceClient.seed_pid_stats("Instance", %{"status" => "running", "queue_depth" => 3})

      socket = Inspector.open_window_for_term(base_socket(), "counter", term)
      [win] = socket.assigns.windows

      assert Inspector.stat_status(win.stats) == "running"
      assert Inspector.stat_mailbox(win.stats) == 3
    end

    test "an unseeded class keeps the default empty stats" do
      term = object_term()
      socket = Inspector.open_window_for_term(base_socket(), "counter", term)
      [win] = socket.assigns.windows

      assert Inspector.stat_status(win.stats) == nil
    end
  end

  describe "dismiss_window_error" do
    test "clears only the matching window's error" do
      win_a = %{id: "a", error: "boom"}
      win_b = %{id: "b", error: "also boom"}
      socket = base_socket(%{windows: [win_a, win_b]})

      {:noreply, socket} = Inspector.handle_event("dismiss_window_error", %{"id" => "a"}, socket)

      assert Enum.find(socket.assigns.windows, &(&1.id == "a")).error == nil
      assert Enum.find(socket.assigns.windows, &(&1.id == "b")).error == "also boom"
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

  describe "@inspector_events coverage (BT-3301)" do
    test "WorkspaceLive's @inspector_events IS Inspector's canonical list, not a copy" do
      assert WorkspaceLive.inspector_events() == Inspector.__inspector_events__()
    end

    test "every event WorkspaceLive delegates to Inspector resolves to an implemented clause" do
      params_by_event = %{
        "inspect" => %{"name" => "counter"},
        "drill" => %{"index" => "0"},
        "crumb" => %{"index" => "0"},
        "freeze_toggle" => %{},
        "poke" => %{"message" => "increment"},
        "close_inspector" => %{},
        "set_inspector_mode" => %{"mode" => "float"},
        "window_close" => %{"id" => "win-1"},
        "window_crumb" => %{"id" => "win-1", "index" => "0"},
        "window_drill" => %{"id" => "win-1", "index" => "0"},
        "window_focus" => %{"id" => "win-1"},
        "window_freeze" => %{"id" => "win-1"},
        "window_moved" => %{"id" => "win-1", "x" => 5, "y" => 5},
        "window_poke" => %{"id" => "win-1", "message" => "increment"},
        "window_reset_positions" => %{},
        "dismiss_window_error" => %{"id" => "win-1"}
      }

      # A hardcoded event-name list here would itself be an unenforced "keep
      # in sync" copy of `@inspector_events` — read it from `Inspector`
      # instead (the module both `WorkspaceLive` and this test now derive
      # from), so adding/renaming/removing a name fails here rather than only
      # at runtime in the browser.
      for event <- Inspector.__inspector_events__() do
        params = Map.fetch!(params_by_event, event)

        assert {:noreply, %Phoenix.LiveView.Socket{}} =
                 Inspector.handle_event(event, params, base_socket()),
               "Inspector.handle_event/3 has no clause for #{inspect(event)} (or it crashed)"
      end
    end

    test "no handle_event/3 clause head names an event missing from the canonical list" do
      # The test above only catches a name in the canonical list with no
      # matching clause (a rename/removal that leaves the list stale). It
      # can't catch the OTHER direction: a brand-new
      # `Inspector.handle_event("some_new_event", ...)` clause added without
      # adding "some_new_event" to `__inspector_events__/0` is unreachable
      # dead code (`WorkspaceLive`'s `when event in @inspector_events` guard
      # never lets it through) rather than a crash, so nothing above would
      # fail. Elixir doesn't expose clause-head literals through module
      # reflection, so this scans the module's own source text for
      # `handle_event("...", ...)` clause heads instead and asserts the
      # literal name set matches the canonical list exactly.
      source = File.read!(@inspector_source)

      clause_names =
        ~r/def handle_event\("([a-z0-9_]+)"/
        |> Regex.scan(source)
        |> Enum.map(fn [_, name] -> name end)
        |> MapSet.new()

      assert clause_names == MapSet.new(Inspector.__inspector_events__())
    end
  end
end

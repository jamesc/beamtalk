# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.InspectorFakeClient do
  @moduledoc """
  A per-test-configurable veneer over `BtAttachWeb.StubWorkspaceClient`
  (BT-3305), used only by `BtAttachWeb.Live.InspectorTest`.

  `BtAttachWeb.Live.Inspector` has a handful of degrade-gracefully branches
  that only fire on a workspace reply the shared stub deliberately never
  produces — an `inspect` that resolves to a *scalar* (not a fields map) or
  fails outright, a `pid_stats` read that errors, a `subscribe_object` the
  workspace refuses, an `eval` that returns the structured-failure 4-tuple. The
  stub is shared by the whole suite and its per-class seeding helpers
  (`seed_inspect_value/2`, `seed_pid_stats/2`) are shaped for the success path,
  so rather than widen it with failure knobs no other test needs, this module
  wraps it — mechanics shared with `BtAttachWeb.Live.DockFakeClient` via the
  `BtAttachWeb.ForcedReplyClient` macro (BT-3316) — every function delegates
  to the stub unless the test forced a reply for that op via
  `Application.put_env(:bt_attach, :inspector_fake, %{op => reply})`.
  """

  use BtAttachWeb.ForcedReplyClient, key: :inspector_fake

  forceable(inspect_value(term))
  forceable(pid_stats(term))
  forceable(eval(pid, code))
  forceable(subscribe_object_changes(term, pid))

  defdelegate unsubscribe_object_changes(term, pid), to: StubWorkspaceClient
  defdelegate list_bindings(pid), to: StubWorkspaceClient
end

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

  alias BtAttach.SessionRegistry
  alias BtAttachWeb.Live.Inspector
  alias BtAttachWeb.Live.InspectorFakeClient
  alias BtAttachWeb.StubWorkspaceClient
  alias BtAttachWeb.WorkspaceLive

  @inspector_source Path.expand("../../lib/bt_attach_web/live/inspector.ex", __DIR__)
  @workspace_live_source Path.expand("../../lib/bt_attach_web/live/workspace_live.ex", __DIR__)

  setup do
    Application.put_env(:bt_attach, :workspace_client, StubWorkspaceClient)
    {:ok, _} = StubWorkspaceClient.start_state()

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      Application.delete_env(:bt_attach, :inspector_fake)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  # Swap the workspace client for `InspectorFakeClient` and force the replies in
  # `overrides` (see that module's doc) — the degrade-gracefully branches the
  # shared stub's success-shaped replies never reach.
  defp force_workspace_replies(overrides) when is_map(overrides) do
    Application.put_env(:bt_attach, :inspector_fake, overrides)
    Application.put_env(:bt_attach, :workspace_client, InspectorFakeClient)
  end

  # A bare socket carrying exactly the assigns Inspector's functions read —
  # the subset of `WorkspaceLive.bind_session/3`'s init relevant to the
  # docked Inspector + floating windows, PLUS the handful of WorkspaceLive-
  # context keys Inspector only ever reads (never initialises):
  # `:current_user`/`:role`/`:session_id`/`:session_pid`/`:__changed__`.
  # `role: :owner` by default (most tests aren't about RBAC); override per
  # test.
  #
  # The Inspector-owned half comes straight from `Inspector.init_assigns/0`
  # (BT-3302) rather than a hand-copied literal map — the same function
  # `bind_session/3` itself calls — so this fixture and production init
  # can never drift apart: a key this module's `handle_event`/`handle_info`
  # clauses read but `init_assigns/0` no longer provides shows up here as a
  # `KeyError` on the very next `mix test`, not only live in a browser.
  defp base_socket(overrides \\ %{}) do
    assigns =
      Inspector.init_assigns()
      |> Map.merge(%{
        __changed__: %{},
        current_user: nil,
        role: :owner,
        session_id: "sess-1",
        session_pid: self()
      })
      |> Map.merge(overrides)

    %Phoenix.LiveView.Socket{assigns: assigns}
  end

  # A synthetic live-object term backed by the test process's own pid — safe
  # to "subscribe"/"inspect" against the stub client without a real node.
  defp object_term(pid \\ self()), do: {:beamtalk_object, :TestClass, :TestClass, pid}

  # One floating window in its freshly-opened shape (the literal
  # `open_window_for_term/3` builds), with `overrides` merged over it. Hand-built
  # rather than opened through the module when a test needs a window state the
  # open path can't reach directly — a stale watch, a pending refresh, a frozen
  # snapshot mid-poke.
  defp window(overrides \\ %{}) do
    Map.merge(
      %{
        id: "win-1",
        label: "counter",
        crumbs: [],
        target: nil,
        rows: [],
        error: nil,
        watch: nil,
        stats: nil,
        frozen: false,
        refresh_pending: false,
        flash_gen: 0,
        poke_result: nil,
        poke_error: nil,
        x: 0,
        y: 0,
        z: 10
      },
      overrides
    )
  end

  # A live process standing in for a *different* watched actor, so a test can
  # prove a push/refresh for one pid leaves windows watching another alone.
  defp other_object_term do
    pid = spawn(fn -> Process.sleep(:infinity) end)
    on_exit(fn -> Process.exit(pid, :kill) end)
    {:beamtalk_object, :OtherClass, :OtherClass, pid}
  end

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

    test "closing the docked pane while a window still watches the same pid keeps the window's subscription alive" do
      pid = self()
      term = object_term(pid)
      StubWorkspaceClient.put_bindings([{"counter", term}])

      socket = Inspector.open_window_for_term(base_socket(), "counter", term)
      {:noreply, socket} = Inspector.handle_event("inspect", %{"name" => "counter"}, socket)

      assert socket.assigns.inspect_watch == term
      [win] = socket.assigns.windows
      assert win.watch == term

      StubWorkspaceClient.clear_calls()

      # Closing the docked pane first (the reverse order of the test above) —
      # the open window still needs the subscription, so it must survive.
      {:noreply, socket} = Inspector.handle_event("close_inspector", %{}, socket)

      refute {:unsubscribe_object_changes, term, pid} in StubWorkspaceClient.calls()
      assert socket.assigns.inspect_watch == nil
      [win] = socket.assigns.windows
      assert win.watch == term

      # Closing the window too — now nothing watches `term` — actually
      # releases it.
      {:noreply, socket} = Inspector.handle_event("window_close", %{"id" => win.id}, socket)

      assert {:unsubscribe_object_changes, term, pid} in StubWorkspaceClient.calls()
      assert socket.assigns.windows == []
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

  describe "init_assigns/0 (BT-3302 socket-assign contract)" do
    test "returns exactly the keys the docked Inspector + floating windows own" do
      # Pins the canonical key SET (not just its use as a socket-assign
      # source elsewhere in this file) so an accidental key removal/rename
      # inside `Inspector.init_assigns/0` itself — with no other call site
      # touched — still fails here, rather than silently starting to omit an
      # assign `bind_session/3` used to initialise.
      expected =
        MapSet.new(~w(
          inspect_target inspect_rows inspect_crumbs inspect_error
          inspect_watch inspect_stats inspect_frozen flash_gen
          refresh_pending poke_result poke_error windows window_z
          next_window_id inspector_mode
        )a)

      assert MapSet.new(Map.keys(Inspector.init_assigns())) == expected
    end

    test "WorkspaceLive.bind_session/3 assigns come from Inspector.init_assigns/0, not a copy" do
      # `bind_session/3` is private, so this asserts the tether the same way
      # the `@inspector_events` coverage test above does for events: scan
      # `workspace_live.ex`'s own source and confirm it calls
      # `Inspector.init_assigns()` rather than hand-listing the keys via a
      # local `assign/3` pipe (the pre-BT-3302 shape this guards against
      # regressing to).
      source = File.read!(@workspace_live_source)

      assert source =~ "assign(Inspector.init_assigns())"

      # And none of the canonical keys are hand-assigned as a literal
      # `assign(:key, ...)` call elsewhere in the file — that would be
      # exactly the second, unenforced copy BT-3302 removed.
      for key <- Map.keys(Inspector.init_assigns()) do
        refute source =~ "assign(:#{key},",
               "workspace_live.ex still hand-assigns :#{key} instead of getting it from Inspector.init_assigns/0"
      end
    end
  end

  # ── BT-3305: the branches the suite above never reached ────────────────────

  describe "unmatched-params fallback clauses (BT-3305)" do
    test "every docked/window event with params its real clause can't match is an untouched no-op" do
      # Each of these events has a real implementation whose clause head
      # destructures the params the markup sends (`id`, `index`, `message`,
      # `x`/`y`) plus a one-line catch-all beneath it. The catch-all is the
      # defence against a crafted / stale client event: it must leave the socket
      # exactly as it found it rather than raise a `FunctionClauseError` and take
      # the whole LiveView down.
      socket = base_socket(%{windows: [window()]})

      unmatched = [
        {"crumb", %{}},
        {"poke", %{}},
        # A non-binary message fails the `is_binary/1` guard on the real clause.
        {"poke", %{"message" => 42}},
        {"window_drill", %{}},
        {"window_crumb", %{}},
        {"window_close", %{}},
        {"window_focus", %{}},
        {"window_moved", %{"id" => "win-1"}},
        {"window_freeze", %{}},
        {"window_poke", %{"id" => "win-1"}},
        {"window_poke", %{"id" => "win-1", "message" => 42}},
        {"dismiss_window_error", %{}}
      ]

      for {event, params} <- unmatched do
        assert {:noreply, ^socket} = Inspector.handle_event(event, params, socket),
               "#{event} with #{inspect(params)} should fall through to an untouched no-op"
      end
    end
  end

  describe "window_crumb navigation (BT-3305)" do
    test "truncates the window's crumb trail at the clicked level and re-inspects that term" do
      pid = self()
      root = {:beamtalk_object, :Counter, :Counter, pid}
      child = {:beamtalk_object, :Child, :Child, pid}
      StubWorkspaceClient.seed_inspect_value("Counter", %{"count" => 1, "child" => child})
      StubWorkspaceClient.seed_inspect_value("Child", %{"name" => "kid"})

      socket = Inspector.open_window_for_term(base_socket(), "counter", root)
      [win] = socket.assigns.windows
      child_index = Enum.find_index(win.rows, &(&1.name == "child"))

      {:noreply, socket} =
        Inspector.handle_event(
          "window_drill",
          %{"id" => win.id, "index" => to_string(child_index)},
          socket
        )

      [win] = socket.assigns.windows
      assert length(win.crumbs) == 2

      {:noreply, socket} =
        Inspector.handle_event("window_crumb", %{"id" => win.id, "index" => "0"}, socket)

      [win] = socket.assigns.windows
      assert win.crumbs == [%{label: "counter", term: root}]
      assert win.target.label == "counter"
      # Walking back re-reads the root, so its own fields are showing again.
      assert Enum.map(win.rows, & &1.name) == ["child", "count"]
      assert win.watch == root
    end

    test "a crumb index that no longer maps to a level, or an unknown window, is a no-op" do
      root = object_term()
      StubWorkspaceClient.seed_inspect_value("TestClass", %{"count" => 1})
      socket = Inspector.open_window_for_term(base_socket(), "counter", root)
      [win] = socket.assigns.windows

      {:noreply, unknown} =
        Inspector.handle_event("window_crumb", %{"id" => "nope", "index" => "0"}, socket)

      assert unknown == socket

      {:noreply, malformed} =
        Inspector.handle_event("window_crumb", %{"id" => win.id, "index" => "9"}, socket)

      assert malformed == socket
    end
  end

  describe "coalesced per-window refresh ({:do_window_refresh, pid}) (BT-3305)" do
    test "re-reads only the windows pending on that pid, clears stale flags, leaves others alone" do
      pid = self()
      term = object_term(pid)
      other = other_object_term()
      StubWorkspaceClient.seed_inspect_value("TestClass", %{"count" => 1})

      pending =
        window(%{
          id: "pending",
          watch: term,
          refresh_pending: true,
          crumbs: [%{label: "counter", term: term}]
        })

      # Watching the same pid but with nothing queued: the timer must clear any
      # stale flag without spending a re-read.
      serviced = window(%{id: "serviced", watch: term, refresh_pending: false})
      # A different pid: its own timer is still in flight and must not be pre-empted.
      elsewhere = window(%{id: "elsewhere", watch: other, refresh_pending: true})

      socket = base_socket(%{windows: [pending, serviced, elsewhere]})

      {:noreply, socket} = Inspector.handle_info({:do_window_refresh, pid}, socket)

      by_id = Map.new(socket.assigns.windows, &{&1.id, &1})

      assert by_id["pending"].refresh_pending == false
      assert by_id["pending"].flash_gen == 1
      assert by_id["pending"].target.label == "counter"
      assert Enum.map(by_id["pending"].rows, & &1.name) == ["count"]

      assert by_id["serviced"].refresh_pending == false
      assert by_id["serviced"].flash_gen == 0

      assert by_id["elsewhere"].refresh_pending == true
      assert by_id["elsewhere"].flash_gen == 0
    end

    test "an object-changed push arms one deferred refresh, skipping frozen / pending / other-pid windows" do
      pid = self()
      term = object_term(pid)
      other = other_object_term()

      live = window(%{id: "live", watch: term})
      frozen = window(%{id: "frozen", watch: term, frozen: true})
      already = window(%{id: "already", watch: term, refresh_pending: true})
      elsewhere = window(%{id: "elsewhere", watch: other})

      socket = base_socket(%{windows: [live, frozen, already, elsewhere]})

      {:noreply, socket} = Inspector.handle_info({:object_changed, pid, %{}}, socket)

      by_id = Map.new(socket.assigns.windows, &{&1.id, &1})
      assert by_id["live"].refresh_pending == true
      assert by_id["frozen"].refresh_pending == false
      assert by_id["already"].refresh_pending == true
      assert by_id["elsewhere"].refresh_pending == false

      assert_receive {:do_window_refresh, ^pid}, 500
    end
  end

  describe "reconnect persistence (BT-2527 #3, covered in BT-3305)" do
    test "build_window_stash snapshots each window's root, placement and freeze" do
      term = object_term()

      socket =
        base_socket(%{
          inspector_mode: "float",
          windows: [
            window(%{
              id: "a",
              crumbs: [%{label: "counter", term: term}, %{label: "child", term: term}],
              x: 10,
              y: 20,
              z: 15,
              frozen: true
            }),
            # An error-only window carries no root term, so there is nothing live
            # to reopen — the crumb pattern skips it.
            window(%{id: "b", crumbs: [], error: "binding not found: ghost"})
          ]
        })

      assert %{mode: "float", windows: [root]} = Inspector.build_window_stash(socket)
      # Drilled levels are deliberately dropped: a resume restores the root.
      assert root == %{label: "counter", term: term, x: 10, y: 20, z: 15, frozen: true}
    end

    test "a fresh session, a disconnected mount, or a token with no stash restores nothing" do
      socket = base_socket()

      assert Inspector.restore_windows(socket, "tab-1", :fresh) == socket
      # Not connected: short-circuits before the registry read.
      assert Inspector.restore_windows(socket, "tab-1", :resumed) == socket

      connected = base_socket(%{connected: true})
      assert Inspector.restore_windows(connected, "no-such-token", :resumed) == connected
    end

    test "a resumed session rebuilds each stashed root at its saved placement and freeze" do
      pid = self()
      live_term = object_term(pid)
      chilled_term = {:beamtalk_object, :Chilled, :Chilled, pid}
      StubWorkspaceClient.seed_inspect_value("TestClass", %{"count" => 1})
      StubWorkspaceClient.seed_inspect_value("Chilled", %{"count" => 2})

      token = "insp-restore-#{System.unique_integer([:positive])}"
      session = spawn(fn -> Process.sleep(:infinity) end)
      :ok = SessionRegistry.register(token, "sess-restore", session)

      on_exit(fn ->
        SessionRegistry.discard(token)
        Process.exit(session, :kill)
      end)

      :ok =
        SessionRegistry.stash_windows(token, %{
          mode: "float",
          windows: [
            %{label: "counter", term: live_term, x: 300, y: 200, z: 42, frozen: false},
            %{label: "chilled", term: chilled_term, x: 5, y: 6, z: 7, frozen: true}
          ]
        })

      socket = Inspector.restore_windows(base_socket(%{connected: true}), token, :resumed)

      assert socket.assigns.inspector_mode == "float"
      assert [live, chilled] = socket.assigns.windows

      assert Map.take(live, [:x, :y, :z]) == %{x: 300, y: 200, z: 42}
      assert live.frozen == false
      # A restored live root re-arms its watch for the NEW LiveView pid.
      assert live.watch == live_term

      assert Map.take(chilled, [:x, :y, :z]) == %{x: 5, y: 6, z: 7}
      assert chilled.frozen == true
      # A re-frozen window holds its snapshot: no live subscription.
      assert chilled.watch == nil
      assert Enum.map(chilled.rows, & &1.name) == ["count"]

      # The z counter never regresses below a restored window's saved stacking.
      assert socket.assigns.window_z >= 42
    end
  end

  describe "opening a window on a binding that can't be resolved (BT-3305)" do
    test "a vanished binding opens a closable error window rather than doing nothing" do
      StubWorkspaceClient.put_bindings([])
      socket = base_socket(%{inspector_mode: "float"})

      {:noreply, socket} = Inspector.handle_event("inspect", %{"name" => "ghost"}, socket)

      assert [win] = socket.assigns.windows
      assert win.error =~ "binding not found: ghost"
      assert win.label == "ghost"
      assert win.crumbs == []
      assert win.target == nil
      assert socket.assigns.window_z == 11
      assert socket.assigns.next_window_id == 2
    end

    test "a failed bindings read opens an error window carrying the workspace error" do
      StubWorkspaceClient.fail_bindings(true)
      socket = base_socket(%{inspector_mode: "float"})

      {:noreply, socket} = Inspector.handle_event("inspect", %{"name" => "counter"}, socket)

      assert [win] = socket.assigns.windows
      assert win.error =~ "unreachable"
    end

    test "the same failure in docked mode surfaces on the pane instead" do
      StubWorkspaceClient.fail_bindings(true)

      {:noreply, socket} =
        Inspector.handle_event("inspect", %{"name" => "counter"}, base_socket())

      assert socket.assigns.inspect_error =~ "unreachable"
      assert socket.assigns.windows == []
    end
  end

  describe "supervisor heads in a floating window (BT-2634, covered in BT-3305)" do
    test "a window on a supervisor renders its children, drillable ones flagged, with no watch armed" do
      sup = {:beamtalk_supervisor, :AppSup, :AppSup, self()}

      socket = Inspector.open_window_for_term(base_socket(), "appSup", sup)

      assert [win] = socket.assigns.windows
      assert Enum.map(win.rows, & &1.name) == ["Counter", "WorkerPool", "logger_std_h"]
      assert Enum.map(win.rows, & &1.drillable) == [true, true, false]
      assert win.target.class_name == "AppSup"
      # Live-tracking is deliberately NOT armed for a supervisor head.
      assert win.watch == nil
    end

    test "a child mid-restart reports \"restarting\" and a one-child supervisor reads singular" do
      force_workspace_replies(%{
        inspect_value:
          {:ok,
           {:supervisor_children,
            [
              %{
                "label" => "Restarting",
                "kind" => "beamtalkActor",
                "pid" => :null,
                "isSupervisor" => false,
                "handle" => :null
              },
              %{
                "label" => "Vanished",
                "kind" => "beamtalkActor",
                "pid" => nil,
                "isSupervisor" => false,
                "handle" => nil
              },
              %{
                "label" => "Solo",
                "kind" => "beamtalkSupervisor",
                # An atom pid (not a binary) still renders through to_string/1.
                "pid" => :"<0.9.0>",
                "isSupervisor" => true,
                "childCount" => 1,
                "handle" => :null
              }
            ]}}
      })

      sup = {:beamtalk_supervisor, :AppSup, :AppSup, self()}
      socket = Inspector.open_window_for_term(base_socket(), "appSup", sup)

      assert [win] = socket.assigns.windows

      assert Enum.map(win.rows, & &1.value) == [
               "beamtalkActor · restarting",
               "beamtalkActor · restarting",
               "beamtalkSupervisor · <0.9.0> · 1 child"
             ]

      refute Enum.any?(win.rows, & &1.drillable)
    end

    test "a window on an unreachable supervisor shows the error head and drops its watch" do
      dead = {:beamtalk_supervisor, :DeadSup, :DeadSup, self()}

      socket = Inspector.open_window_for_term(base_socket(), "deadSup", dead)

      assert [win] = socket.assigns.windows
      assert win.error =~ "supervisor is not alive"
      assert win.target == nil
      assert win.rows == []
      assert win.crumbs == []
      assert win.watch == nil
    end
  end

  describe "degraded workspace replies (BT-3305)" do
    test "an object whose inspect resolves to a scalar renders one value row, docked and windowed" do
      force_workspace_replies(%{inspect_value: {:ok, 42}})
      term = object_term()
      StubWorkspaceClient.put_bindings([{"counter", term}])

      {:noreply, docked} =
        Inspector.handle_event("inspect", %{"name" => "counter"}, base_socket())

      assert [%{name: "value", value: "42", drillable: false, kind: "int"}] =
               docked.assigns.inspect_rows

      # BT-3319: the docked pane and a window share one `inspect_pane/5` core,
      # so a scalar head has no pid to watch on EITHER side (this used to
      # differ: the docked pane armed a watch on the original pid-backed
      # term, while the window correctly tracked the scalar — see the window
      # assertion below, which pinned only the window's half of this).
      assert docked.assigns.inspect_watch == nil
      assert docked.assigns.inspect_stats == nil

      floated = Inspector.open_window_for_term(base_socket(), "counter", term)

      assert [win] = floated.assigns.windows
      assert [%{name: "value", value: "42", drillable: false}] = win.rows
      # A scalar head has no pid to watch.
      assert win.watch == nil
    end

    test "a live refresh that resolves to a scalar still refreshes stats and flashes" do
      force_workspace_replies(%{inspect_value: {:ok, "hi"}})
      pid = self()
      term = object_term(pid)

      docked =
        base_socket(%{
          inspect_watch: term,
          refresh_pending: true,
          inspect_crumbs: [%{label: "counter", term: term}]
        })

      {:noreply, docked} = Inspector.handle_info(:do_object_refresh, docked)
      assert docked.assigns.flash_gen == 1
      assert docked.assigns.refresh_pending == false

      floated =
        base_socket(%{windows: [window(%{watch: term, refresh_pending: true})]})

      {:noreply, floated} = Inspector.handle_info({:do_window_refresh, pid}, floated)
      assert [%{flash_gen: 1}] = floated.assigns.windows
    end

    test "a transient read failure on a live refresh keeps the rows already showing" do
      force_workspace_replies(%{inspect_value: {:error, :unreachable}})
      pid = self()
      term = object_term(pid)
      rows = [%{name: "count", value: "1", term: 1, drillable: false, kind: "int"}]

      docked =
        base_socket(%{
          inspect_watch: term,
          inspect_rows: rows,
          flash_gen: 3,
          inspect_crumbs: [%{label: "counter", term: term}]
        })

      {:noreply, refreshed} = Inspector.handle_info(:do_object_refresh, docked)
      assert refreshed.assigns.inspect_rows == rows
      assert refreshed.assigns.flash_gen == 3

      floated =
        base_socket(%{
          windows: [window(%{watch: term, refresh_pending: true, rows: rows, flash_gen: 3})]
        })

      {:noreply, floated} = Inspector.handle_info({:do_window_refresh, pid}, floated)
      assert [%{rows: ^rows, flash_gen: 3}] = floated.assigns.windows
    end

    test "a failed pid-stats read clears the chips rather than rendering stale numbers" do
      force_workspace_replies(%{pid_stats: {:error, :dead}})
      term = object_term()
      StubWorkspaceClient.put_bindings([{"counter", term}])

      {:noreply, docked} =
        Inspector.handle_event("inspect", %{"name" => "counter"}, base_socket())

      assert docked.assigns.inspect_stats == nil

      floated = Inspector.open_window_for_term(base_socket(), "counter", term)
      assert [%{stats: nil}] = floated.assigns.windows
    end

    test "a refused subscription leaves the pane / window unwatched, never claiming a live watch" do
      force_workspace_replies(%{subscribe_object_changes: {:error, :not_watchable}})
      term = object_term()
      StubWorkspaceClient.put_bindings([{"counter", term}])

      {:noreply, docked} =
        Inspector.handle_event("inspect", %{"name" => "counter"}, base_socket())

      assert docked.assigns.inspect_watch == nil

      # The docked unfreeze re-arm takes the same refusal.
      frozen =
        base_socket(%{
          inspect_frozen: true,
          inspect_crumbs: [%{label: "counter", term: term}]
        })

      {:noreply, thawed} = Inspector.handle_event("freeze_toggle", %{}, frozen)
      assert thawed.assigns.inspect_frozen == false
      assert thawed.assigns.inspect_watch == nil

      # …and so do a window's initial arm and its unfreeze re-arm.
      floated = Inspector.open_window_for_term(base_socket(), "counter", term)
      assert [%{watch: nil}] = floated.assigns.windows

      chilled =
        base_socket(%{
          windows: [window(%{id: "w", frozen: true, crumbs: [%{label: "counter", term: term}]})]
        })

      {:noreply, chilled} = Inspector.handle_event("window_freeze", %{"id" => "w"}, chilled)
      assert [%{frozen: false, watch: nil}] = chilled.assigns.windows
    end

    test "a poke whose eval fails renders the workspace error, docked and windowed" do
      force_workspace_replies(%{eval: {:error, :boom, "", []}})
      term = object_term()

      docked = base_socket(%{inspect_crumbs: [%{label: "counter", term: term}]})
      {:noreply, docked} = Inspector.handle_event("poke", %{"message" => "increment"}, docked)
      assert docked.assigns.poke_result == nil
      assert docked.assigns.poke_error =~ "boom"

      floated =
        base_socket(%{windows: [window(%{id: "w", crumbs: [%{label: "counter", term: term}]})]})

      {:noreply, floated} =
        Inspector.handle_event("window_poke", %{"id" => "w", "message" => "increment"}, floated)

      assert [%{poke_result: nil, poke_error: error}] = floated.assigns.windows
      assert error =~ "boom"
    end
  end

  describe "poke gates on a floating window (BT-3305)" do
    test "an Observer's window poke is refused by the facade, not sent" do
      socket =
        base_socket(%{
          role: :observer,
          windows: [window(%{id: "w", crumbs: [%{label: "counter", term: object_term()}]})]
        })

      {:noreply, socket} =
        Inspector.handle_event("window_poke", %{"id" => "w", "message" => "increment"}, socket)

      assert [%{poke_result: nil, poke_error: error}] = socket.assigns.windows
      assert error =~ "Not authorized"
    end

    test "a window poke with no workspace attachment, or a blank message, is a local error" do
      win = window(%{id: "w", crumbs: [%{label: "counter", term: object_term()}]})

      detached = base_socket(%{session_pid: nil, windows: [win]})

      {:noreply, detached} =
        Inspector.handle_event("window_poke", %{"id" => "w", "message" => "increment"}, detached)

      assert [%{poke_error: "not attached to workspace"}] = detached.assigns.windows

      blank = base_socket(%{windows: [win]})

      {:noreply, blank} =
        Inspector.handle_event("window_poke", %{"id" => "w", "message" => "   "}, blank)

      assert [%{poke_error: "Enter a message to send."}] = blank.assigns.windows
    end

    test "a frozen window shows the poke result but deliberately holds its snapshot" do
      socket =
        base_socket(%{
          windows: [
            window(%{
              id: "w",
              frozen: true,
              watch: nil,
              crumbs: [%{label: "counter", term: object_term()}]
            })
          ]
        })

      {:noreply, socket} =
        Inspector.handle_event("window_poke", %{"id" => "w", "message" => "increment"}, socket)

      assert [%{poke_result: result, poke_error: nil, flash_gen: 0, rows: []}] =
               socket.assigns.windows

      assert result =~ "→ "
    end
  end

  describe "docked poke edge cases (BT-3305)" do
    test "a poke with no workspace attachment is a local error" do
      socket =
        base_socket(%{
          session_pid: nil,
          inspect_crumbs: [%{label: "counter", term: object_term()}]
        })

      {:noreply, socket} = Inspector.handle_event("poke", %{"message" => "increment"}, socket)

      assert socket.assigns.poke_error == "not attached to workspace"
    end

    test "a successful poke re-reads the watched object immediately, without waiting on the push" do
      term = object_term()
      StubWorkspaceClient.seed_inspect_value("TestClass", %{"count" => 1})

      socket =
        base_socket(%{
          inspect_crumbs: [%{label: "counter", term: term}],
          inspect_watch: term,
          flash_gen: 0
        })

      {:noreply, socket} = Inspector.handle_event("poke", %{"message" => "increment"}, socket)

      assert socket.assigns.poke_error == nil
      assert socket.assigns.poke_result =~ "→ "
      # `refresh_poked_inspector/1` re-read the object and flashed the change.
      assert socket.assigns.flash_gen == 1
      assert Enum.map(socket.assigns.inspect_rows, & &1.name) == ["count"]
    end
  end

  describe "freeze holds the snapshot without a subscription (BT-3305)" do
    test "inspecting while the docked pane is frozen reads stats but arms no watch" do
      term = object_term()
      StubWorkspaceClient.put_bindings([{"counter", term}])
      StubWorkspaceClient.seed_pid_stats("TestClass", %{"status" => "waiting"})

      socket = base_socket(%{inspect_frozen: true})
      StubWorkspaceClient.clear_calls()

      {:noreply, socket} = Inspector.handle_event("inspect", %{"name" => "counter"}, socket)

      assert socket.assigns.inspect_watch == nil
      assert socket.assigns.inspect_stats == %{"status" => "waiting"}

      refute Enum.any?(
               StubWorkspaceClient.calls(),
               &match?({:subscribe_object_changes, _, _}, &1)
             )
    end

    test "drilling a frozen window re-inspects it without re-arming its watch" do
      pid = self()
      root = {:beamtalk_object, :Counter, :Counter, pid}
      child = {:beamtalk_object, :Child, :Child, pid}
      StubWorkspaceClient.seed_inspect_value("Counter", %{"child" => child})
      StubWorkspaceClient.seed_inspect_value("Child", %{"name" => "kid"})

      socket = Inspector.open_window_for_term(base_socket(), "counter", root)
      [win] = socket.assigns.windows

      {:noreply, socket} = Inspector.handle_event("window_freeze", %{"id" => win.id}, socket)
      [win] = socket.assigns.windows
      assert win.frozen == true

      child_index = Enum.find_index(win.rows, &(&1.name == "child"))

      {:noreply, socket} =
        Inspector.handle_event(
          "window_drill",
          %{"id" => win.id, "index" => to_string(child_index)},
          socket
        )

      assert [win] = socket.assigns.windows
      assert win.target.label == "child"
      assert win.frozen == true
      assert win.watch == nil
    end

    test "unfreezing a window with no live object head just flips the flag" do
      socket =
        base_socket(%{
          windows: [
            # A scalar head: nothing to re-subscribe to.
            window(%{id: "scalar", frozen: true, crumbs: [%{label: "n", term: 42}]}),
            # No head at all (an error-only window).
            window(%{id: "headless", frozen: true, crumbs: []})
          ]
        })

      {:noreply, socket} = Inspector.handle_event("window_freeze", %{"id" => "scalar"}, socket)
      {:noreply, socket} = Inspector.handle_event("window_freeze", %{"id" => "headless"}, socket)

      assert Enum.map(socket.assigns.windows, & &1.frozen) == [false, false]
      assert Enum.all?(socket.assigns.windows, &(&1.watch == nil))
    end
  end

  describe "a watch term that is no longer a pid-backed object (BT-3305)" do
    test "refreshing it is a no-op and releasing it skips the reference-count check" do
      socket =
        base_socket(%{
          inspect_watch: :stale,
          windows: [window(%{id: "w", watch: :stale})]
        })

      # Nothing to re-read: the deferred refresh only clears its pending flag.
      {:noreply, refreshed} = Inspector.handle_info(:do_object_refresh, socket)
      assert refreshed.assigns.refresh_pending == false
      assert refreshed.assigns.inspect_rows == []
      assert refreshed.assigns.inspect_target == nil

      {:noreply, socket} = Inspector.handle_event("window_close", %{"id" => "w"}, socket)
      assert socket.assigns.windows == []

      {:noreply, socket} = Inspector.handle_event("close_inspector", %{}, socket)
      assert socket.assigns.inspect_watch == nil
    end

    test "a window with no crumbs falls back to its target label on a refresh" do
      pid = self()
      term = object_term(pid)

      socket =
        base_socket(%{
          windows: [
            window(%{id: "w", watch: term, refresh_pending: true, crumbs: [], target: nil})
          ]
        })

      {:noreply, socket} = Inspector.handle_info({:do_window_refresh, pid}, socket)

      assert [%{target: %{label: "value"}}] = socket.assigns.windows
    end
  end

  describe "window_watched_terms/1 (BT-3305)" do
    test "dedupes the pids an open desk watches and answers [] for no desk" do
      term = object_term()
      other = other_object_term()

      assert Inspector.window_watched_terms(nil) == []

      windows = [
        window(%{id: "a", watch: term}),
        window(%{id: "b", watch: term}),
        window(%{id: "c", watch: other}),
        window(%{id: "d", watch: nil})
      ]

      assert Inspector.window_watched_terms(windows) == [term, other]
    end
  end

  describe "value classification + chip formatting (BT-3305)" do
    test "term_kind/1 maps floats, atoms and collections" do
      assert Inspector.term_kind(1.5) == "int"
      assert Inspector.term_kind(:done) == "symbol"
      assert Inspector.term_kind([1, 2]) == "value"
      assert Inspector.term_kind(%{"a" => 1}) == "value"
    end

    test "stat_reductions/1 groups a negative reduction count too" do
      assert Inspector.stat_reductions(%{"reductions" => -1_234_567}) == "-1,234,567"
      assert Inspector.stat_reductions(%{"reductions" => "many"}) == nil
    end

    test "inspecting a non-object binding names the kind of value it is" do
      for {name, term, kind} <- [
            {"pi", 3.14, "number"},
            {"flag", true, "boolean"},
            {"items", [1, 2], "collection"},
            {"conf", %{"a" => 1}, "map"},
            {"marker", :done, "value"}
          ] do
        StubWorkspaceClient.put_bindings([{name, term}])

        {:noreply, socket} = Inspector.handle_event("inspect", %{"name" => name}, base_socket())

        assert socket.assigns.inspect_error == "#{name} is a #{kind} — no fields to inspect"
        assert socket.assigns.inspect_rows == []
        assert socket.assigns.inspect_watch == nil
      end
    end
  end

  describe "window_moved coordinate clamping (BT-3305)" do
    test "integer, string and unparseable drag coordinates all land on a non-negative integer" do
      socket = base_socket(%{windows: [window(%{id: "a"})]})

      {:noreply, moved} =
        Inspector.handle_event("window_moved", %{"id" => "a", "x" => 40, "y" => "25"}, socket)

      assert Map.take(hd(moved.assigns.windows), [:x, :y]) == %{x: 40, y: 25}

      {:noreply, clamped} =
        Inspector.handle_event("window_moved", %{"id" => "a", "x" => "nope", "y" => "-3"}, socket)

      assert Map.take(hd(clamped.assigns.windows), [:x, :y]) == %{x: 0, y: 0}
    end
  end

  describe "inspector_windows/1 renders a populated window (BT-3305)" do
    test "a drilled window renders crumbs, pid-stat chips, a dismissable error and drillable rows" do
      import Phoenix.LiveViewTest

      term = object_term()

      drilled = %{
        id: "win-9",
        label: "counter",
        target: %{
          label: "child",
          header: "a Child",
          class_name: "Child",
          pid: "#PID<0.310.0>"
        },
        crumbs: [%{label: "counter", term: term}, %{label: "child", term: term}],
        rows: [
          %{name: "kid", value: "a Kid", term: term, drillable: true, kind: "ref"},
          %{name: "count", value: "1", term: 1, drillable: false, kind: "int"}
        ],
        error: "stale read",
        stats: %{"status" => "running", "queue_depth" => 2, "reductions" => 1_234_567},
        frozen: true,
        flash_gen: 4,
        poke_result: nil,
        poke_error: nil,
        x: 10,
        y: 20,
        z: 30
      }

      html = render_component(&Inspector.inspector_windows/1, windows: [drilled], role: :owner)

      assert html =~ ~s(id="inspector-window-win-9")
      # Breadcrumb walk-back for a drilled window.
      assert html =~ "insp-crumbs"
      assert html =~ "window_crumb"
      assert html =~ "Inspecting"
      assert html =~ "a Child"
      assert html =~ "#PID&lt;0.310.0&gt;"
      # pid-stat chips.
      assert html =~ "running"
      assert html =~ "mailbox"
      assert html =~ "1,234,567"
      # Dismissable per-window error.
      assert html =~ "stale read"
      assert html =~ "dismiss_window_error"
      # Field table: the drillable row carries the follow affordance.
      assert html =~ ~s(id="inspector-window-fields-win-9")
      assert html =~ "window_drill"
      assert html =~ "follow →"
      # Frozen windows chip their state on the freeze toggle.
      assert html =~ ~s(class="insp-freeze frozen")
      assert html =~ "window_freeze"
      # A drilled head is not a named binding, so no poke bar — for anyone.
      refute html =~ "poke-row"
    end

    test "a window at a named-binding root renders the owner-only poke bar and its results" do
      import Phoenix.LiveViewTest

      root = %{
        id: "win-1",
        label: "counter",
        target: %{
          label: "counter",
          header: "a Counter",
          class_name: "Counter",
          pid: "#PID<0.311.0>"
        },
        crumbs: [%{label: "counter", term: object_term()}],
        rows: [%{name: "count", value: "1", term: 1, drillable: false, kind: "int"}],
        error: nil,
        stats: %{"status" => "waiting"},
        frozen: false,
        flash_gen: 0,
        poke_result: "→ 2",
        poke_error: "previous send failed",
        x: 0,
        y: 0,
        z: 10
      }

      owner_html = render_component(&Inspector.inspector_windows/1, windows: [root], role: :owner)

      assert owner_html =~ "poke-row"
      assert owner_html =~ "window_poke"
      assert owner_html =~ "Send a message to"
      assert owner_html =~ "→ 2"
      assert owner_html =~ "previous send failed"
      # A live (unfrozen) window chips "live" on the freeze toggle.
      assert owner_html =~ ~s(class="insp-freeze live")

      observer_html =
        render_component(&Inspector.inspector_windows/1, windows: [root], role: :observer)

      refute observer_html =~ "poke-row"
    end
  end
end

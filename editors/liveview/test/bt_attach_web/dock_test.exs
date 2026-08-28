# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.DockTest do
  @moduledoc """
  Direct unit tests for `BtAttachWeb.Live.Dock` (BT-3295), driving its
  `handle_event/3` / `handle_async/3` clauses and pure helpers against a
  hand-built `%Phoenix.LiveView.Socket{}` and the fully-stubbed workspace
  client (`BtAttachWeb.StubWorkspaceClient`, BT-2554) — no full LiveView
  mount, no real workspace node. Mirrors `BtAttachWeb.Live.InspectorTest`
  (BT-3291), the precedent this extraction follows.

  Covers the branches BT-3295's acceptance criteria calls out specifically:
  the REPL history ring at the composer's edges, eval action routing (Do
  it/Print it/Inspect it), the git stage/unstage/commit/revert error paths,
  and the destructive-flush confirmation path — previously reachable only
  through the `:workspace`-tagged full-stack `WorkspaceLiveTest`, which needs
  a live workspace node and is excluded from the default `mix test` lane.
  """
  use ExUnit.Case, async: false

  alias BtAttachWeb.Live.Dock
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

  # A bare, disconnected socket carrying exactly the assigns Dock's functions
  # read — the subset of `WorkspaceLive.bind_session/3`'s init relevant to the
  # eval/REPL/Changes/Git dock. `role: :owner` by default (most tests aren't
  # about RBAC); override per test. Disconnected (`Phoenix.LiveView.connected?/1`
  # is false), so `start_async`/`cancel_async` (the git panel's async load)
  # degrade to synchronous no-ops rather than spawning a linked Task — exactly
  # the behaviour `Phoenix.LiveView.Async` documents for a disconnected socket,
  # so `assign_git/1`'s synchronous reset assigns are still directly
  # assertable. The `:repl` stream is pre-configured (`Phoenix.LiveView.stream/3`)
  # since `stream_insert/3` raises against an unconfigured stream name.
  defp base_socket(overrides \\ %{}) do
    assigns =
      %{
        __changed__: %{},
        current_user: nil,
        role: :owner,
        session_id: "sess-1",
        session_pid: self(),
        dock_tab: "workspace",
        result: nil,
        output: nil,
        error: nil,
        expr: "3 + 4",
        eval_seq: 0,
        ws_selection: nil,
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
        next_window_id: 1,
        repl_seq: 0,
        repl_terms: %{},
        repl_history: [],
        repl_history_pos: nil,
        changes: [],
        changes_error: nil,
        expanded_changes: MapSet.new(),
        browser_classes: [],
        tabs: [],
        autoflush: false,
        git_status: nil,
        git_log: [],
        git_error: nil,
        git_diff: nil,
        git_diff_path: nil,
        flush_result: nil,
        flush_error: nil,
        save_result: nil,
        save_error: nil,
        test_classes: nil
      }
      |> Map.merge(overrides)

    %Phoenix.LiveView.Socket{
      assigns: assigns,
      private: %{live_temp: %{}, lifecycle: %Phoenix.LiveView.Lifecycle{}}
    }
    |> Phoenix.LiveView.stream(:repl, [])
  end

  describe "eval action routing (Do it / Print it / Inspect it)" do
    test "do_it evaluates for side effects and shows a terse confirmation, no buffer insert" do
      {:noreply, socket} =
        Dock.handle_event("eval", %{"expr" => "1 + 1", "action" => "do_it"}, base_socket())

      assert socket.assigns.result == "✓ evaluated"
      assert socket.assigns.error == nil
      # `eval_status/4` bumps the re-key sequence on every success.
      assert socket.assigns.eval_seq == 1
      # do_it never pushes the inline-insert event (print_it's affordance).
      assert socket.private.live_temp[:push_events] in [nil, []]
    end

    test "print_it (default, no action field) inserts the result inline and flashes the status" do
      {:noreply, socket} = Dock.handle_event("eval", %{"expr" => "1 + 1"}, base_socket())

      assert socket.assigns.result =~ "→ "

      assert [["ws_insert_result", %{text: _, target: "workspace-editor-overlay"}]] =
               socket.private.live_temp[:push_events]
    end

    test "inspect_it evaluates then opens the result in the docked Inspector" do
      {:noreply, socket} =
        Dock.handle_event("eval", %{"expr" => "1 + 1", "action" => "inspect_it"}, base_socket())

      assert socket.assigns.result =~ "→ "
      # Inspector.inspect_term/4 ran against the eval's result term (a scalar
      # stub value) and set the docked pane's target.
      assert socket.assigns.inspect_target != nil
      assert socket.assigns.inspect_crumbs == [%{label: "→ result", term: "stub-result"}]
    end

    test "inspect_it in float mode opens a floating window instead of the docked pane" do
      {:noreply, socket} =
        Dock.handle_event(
          "eval",
          %{"expr" => "1 + 1", "action" => "inspect_it"},
          base_socket(%{inspector_mode: "float"})
        )

      assert socket.assigns.inspect_target == nil
      assert [%{label: "→ result"}] = socket.assigns.windows
    end

    test "no session (attach failed) reports 'not attached' rather than crashing" do
      socket = base_socket(%{session_pid: nil})

      {:noreply, socket} = Dock.handle_event("eval", %{"expr" => "1 + 1"}, socket)

      assert socket.assigns.error == "not attached to workspace"
      assert socket.assigns.result == nil
    end

    test "an RBAC-denied eval (Observer) surfaces the facade's short-circuit error" do
      {:noreply, socket} =
        Dock.handle_event("eval", %{"expr" => "1 + 1"}, base_socket(%{role: :observer}))

      assert socket.assigns.error =~ "not"
      assert socket.assigns.result == nil
    end
  end

  describe "select_workspace" do
    test "a well-formed selection is tracked separately from the method editor's" do
      {:noreply, socket} =
        Dock.handle_event(
          "select_workspace",
          %{"text" => "1 + 1", "start" => 0, "end" => 5},
          base_socket()
        )

      assert socket.assigns.ws_selection == %{text: "1 + 1", start: 0, end: 5}
    end

    test "a malformed payload is a no-op" do
      socket = base_socket()

      assert {:noreply, ^socket} = Dock.handle_event("select_workspace", %{}, socket)
    end
  end

  describe "REPL history ring at the composer's edges" do
    test "↑ walks back through the ring; ↓ past the newest restores the live input" do
      socket = base_socket(%{repl_history: ["third", "second", "first"], repl_history_pos: nil})

      # First ↑: the most recent entry.
      {:noreply, socket} = Dock.handle_event("repl_history_prev", %{}, socket)
      assert socket.assigns.repl_history_pos == 0
      assert [["repl_set_input", %{text: "third"}]] = socket.private.live_temp[:push_events]

      socket = clear_push_events(socket)

      # Second ↑: one further back.
      {:noreply, socket} = Dock.handle_event("repl_history_prev", %{}, socket)
      assert socket.assigns.repl_history_pos == 1
      assert [["repl_set_input", %{text: "second"}]] = socket.private.live_temp[:push_events]

      socket = clear_push_events(socket)

      # A further ↑ at the oldest entry clamps — does not walk off the ring.
      {:noreply, socket} = Dock.handle_event("repl_history_prev", %{}, socket)
      {:noreply, socket} = Dock.handle_event("repl_history_prev", %{}, socket)
      assert socket.assigns.repl_history_pos == 2

      socket = clear_push_events(socket)

      # Walking ↓ back past the newest restores the empty live input (pos: nil).
      # `push_event/3` prepends, so the LAST push (the empty-input restore) is
      # the head of the accumulated list.
      {:noreply, socket} = Dock.handle_event("repl_history_next", %{}, socket)
      {:noreply, socket} = Dock.handle_event("repl_history_next", %{}, socket)
      {:noreply, socket} = Dock.handle_event("repl_history_next", %{}, socket)
      assert socket.assigns.repl_history_pos == nil
      assert [["repl_set_input", %{text: ""}] | _] = socket.private.live_temp[:push_events]
    end

    test "↓ at the live input (pos: nil) is a no-op — never disturbs an in-progress draft" do
      socket = base_socket(%{repl_history: ["only"], repl_history_pos: nil})

      {:noreply, result} = Dock.handle_event("repl_history_next", %{}, socket)

      assert result == socket
    end

    test "an empty ring is a no-op for both directions" do
      socket = base_socket(%{repl_history: []})

      assert {:noreply, ^socket} = Dock.handle_event("repl_history_prev", %{}, socket)
      assert {:noreply, ^socket} = Dock.handle_event("repl_history_next", %{}, socket)
    end

    test "a crafted history recall during the attach-failure window (no :repl_history) is a no-op" do
      socket = %Phoenix.LiveView.Socket{assigns: %{__changed__: %{}}}

      assert {:noreply, ^socket} = Dock.handle_event("repl_history_prev", %{}, socket)
      assert {:noreply, ^socket} = Dock.handle_event("repl_history_next", %{}, socket)
    end

    defp clear_push_events(socket), do: put_in(socket.private.live_temp[:push_events], [])
  end

  describe "repl_eval" do
    test "a successful eval appends a › request / → response pair and records history" do
      {:noreply, socket} = Dock.handle_event("repl_eval", %{"expr" => "1 + 1"}, base_socket())

      assert socket.assigns.repl_seq == 1
      assert socket.assigns.repl_history == ["1 + 1"]
      assert map_size(socket.assigns.repl_terms) == 1
    end

    test "an empty submit is a no-op — never appends a blank entry" do
      socket = base_socket()

      assert {:noreply, ^socket} = Dock.handle_event("repl_eval", %{"expr" => "   "}, socket)
    end

    test "a `:help` meta-command appends an info entry instead of evaluating" do
      {:noreply, socket} = Dock.handle_event("repl_eval", %{"expr" => ":help"}, base_socket())

      assert socket.assigns.repl_seq == 1
      # Meta-commands never reach `eval` (no live term stashed).
      assert socket.assigns.repl_terms == %{}
    end

    test "a `:test` meta-command switches the dock to the Tests tab" do
      {:noreply, socket} = Dock.handle_event("repl_eval", %{"expr" => ":test"}, base_socket())

      assert socket.assigns.dock_tab == "tests"
    end
  end

  describe "dock_tab" do
    test "switching to changes refreshes the ChangeLog" do
      {:noreply, socket} = Dock.handle_event("dock_tab", %{"tab" => "changes"}, base_socket())

      assert socket.assigns.dock_tab == "changes"
      assert socket.assigns.changes == []
    end

    test "switching to git resets the panel to its loading state" do
      socket =
        base_socket(%{git_status: %{branch: "stale"}, git_diff_path: "src/Foo.bt"})

      {:noreply, socket} = Dock.handle_event("dock_tab", %{"tab" => "git"}, socket)

      assert socket.assigns.dock_tab == "git"
      assert socket.assigns.git_status == nil
      assert socket.assigns.git_diff_path == nil
    end

    test "an unknown tab is ignored rather than blanking the dock" do
      socket = base_socket()

      assert {:noreply, ^socket} = Dock.handle_event("dock_tab", %{"tab" => "bogus"}, socket)
    end
  end

  describe "git panel — stage/unstage/commit/revert error paths" do
    test "git_stage is refused for the Observer role (RBAC gate)" do
      {:noreply, socket} =
        Dock.handle_event(
          "git_stage",
          %{"path" => "src/Foo.bt"},
          base_socket(%{role: :observer})
        )

      assert socket.assigns.git_error != nil
    end

    test "git_unstage is refused for the Observer role" do
      {:noreply, socket} =
        Dock.handle_event(
          "git_unstage",
          %{"path" => "src/Foo.bt"},
          base_socket(%{role: :observer})
        )

      assert socket.assigns.git_error != nil
    end

    test "git_commit is refused for the Observer role" do
      {:noreply, socket} =
        Dock.handle_event(
          "git_commit",
          %{"message" => "fix"},
          base_socket(%{role: :observer})
        )

      assert socket.assigns.git_error != nil
    end

    test "git_commit with a blank message is a validation error, no dispatch" do
      {:noreply, socket} =
        Dock.handle_event("git_commit", %{"message" => "   "}, base_socket())

      assert socket.assigns.git_error == "Enter a commit message."
    end

    test "git_revert is blocked while the path has unflushed in-memory ChangeLog edits" do
      socket =
        base_socket(%{
          changes: [%{class: "Foo", selector: "bar"}],
          browser_classes: [%{"name" => "Foo", "source_file" => "src/Foo.bt"}]
        })

      {:noreply, socket} =
        Dock.handle_event("git_revert", %{"path" => "src/Foo.bt"}, socket)

      assert socket.assigns.git_error =~ "unflushed in-memory edits"
    end

    test "a malformed git_diff payload surfaces a validation error, not a crash" do
      {:noreply, socket} = Dock.handle_event("git_diff", %{}, base_socket())

      assert socket.assigns.git_error == "Invalid diff request."
    end

    test "a malformed git_stage payload surfaces a validation error" do
      {:noreply, socket} = Dock.handle_event("git_stage", %{}, base_socket())

      assert socket.assigns.git_error == "Invalid stage request."
    end

    test "git_refresh resets the panel to its loading state" do
      socket = base_socket(%{git_error: "stale error"})

      {:noreply, socket} = Dock.handle_event("git_refresh", %{}, socket)

      assert socket.assigns.git_error == nil
      assert socket.assigns.git_status == nil
    end
  end

  describe "destructive-flush confirmation path" do
    test "a non-owner (Observer) click is a no-op — the button isn't even rendered for them" do
      socket = base_socket(%{role: :observer})

      assert {:noreply, ^socket} =
               Dock.handle_event("flush_destructive", %{"class" => "Foo"}, socket)
    end

    test "a crafted event with a missing class is a no-op" do
      socket = base_socket()

      assert {:noreply, ^socket} = Dock.handle_event("flush_destructive", %{}, socket)
    end

    test "an invalid (non-PascalCase) class name is rejected before any dispatch" do
      {:noreply, socket} =
        Dock.handle_event("flush_destructive", %{"class" => "not_a_class!"}, base_socket())

      assert socket.assigns.save_error == "Invalid class name."
    end

    test "no session (attach failed) reports 'not attached'" do
      socket = base_socket(%{session_pid: nil})

      {:noreply, socket} =
        Dock.handle_event("flush_destructive", %{"class" => "Foo"}, socket)

      assert socket.assigns.save_error == "not attached to workspace"
    end

    test "a successful destructive flush of a remove-class row reports the default message" do
      {:noreply, socket} =
        Dock.handle_event("flush_destructive", %{"class" => "Foo"}, base_socket())

      assert socket.assigns.flush_result == "Flushed the pending removal for Foo"
      assert socket.assigns.flush_error == nil
    end

    test "a successful destructive flush of a rename-class row reports the rename message" do
      {:noreply, socket} =
        Dock.handle_event(
          "flush_destructive",
          %{"class" => "Foo", "kind" => "rename-class"},
          base_socket()
        )

      assert socket.assigns.flush_result == "Flushed the pending rename to Foo"
    end
  end

  describe "flush / revert / toggle_change_diff" do
    test "flush drives the write-surface flush and renders its summary" do
      {:noreply, socket} = Dock.handle_event("flush", %{}, base_socket())

      assert socket.assigns.flush_result != nil
      assert socket.assigns.flush_error == nil
    end

    test "revert re-installs the prior body and refreshes the Changes pane" do
      {:noreply, socket} =
        Dock.handle_event(
          "revert",
          %{"class" => "Foo", "selector" => "bar"},
          base_socket()
        )

      assert socket.assigns.save_result =~ "Reverted bar on Foo"
      assert socket.assigns.save_error == nil
    end

    test "a malformed revert payload surfaces a validation error" do
      {:noreply, socket} = Dock.handle_event("revert", %{}, base_socket())

      assert socket.assigns.save_error == "Invalid revert request."
    end

    test "toggle_change_diff flips a row's expanded key in and back out" do
      params = %{"class" => "Foo", "selector" => "bar"}
      socket = base_socket()

      {:noreply, socket} = Dock.handle_event("toggle_change_diff", params, socket)
      assert MapSet.member?(socket.assigns.expanded_changes, {"Foo", "bar", nil})

      {:noreply, socket} = Dock.handle_event("toggle_change_diff", params, socket)
      refute MapSet.member?(socket.assigns.expanded_changes, {"Foo", "bar", nil})
    end
  end

  describe "handle_async(:git_load, …)" do
    test "a successful load folds the status + log into the panel assigns" do
      status = %{branch: "main", upstream: nil, ahead: 0, behind: 0, files: []}

      {:noreply, socket} =
        Dock.handle_async(
          :git_load,
          {:ok, {{:ok, status}, {:ok, [%{sha: "abc"}]}}},
          base_socket()
        )

      assert socket.assigns.git_status == status
      assert socket.assigns.git_log == [%{sha: "abc"}]
      assert socket.assigns.git_error == nil
    end

    test "a cancelled load is a no-op" do
      socket = base_socket()

      assert {:noreply, ^socket} = Dock.handle_async(:git_load, {:exit, :cancelled}, socket)
    end

    test "a crashed load surfaces a panel error and clears stale data" do
      {:noreply, socket} =
        Dock.handle_async(:git_load, {:exit, :boom}, base_socket(%{git_log: [%{sha: "stale"}]}))

      assert socket.assigns.git_status == nil
      assert socket.assigns.git_log == []
      assert socket.assigns.git_error =~ "failed unexpectedly"
    end
  end
end

# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.DockFakeClient do
  @moduledoc """
  A per-test-configurable veneer over `BtAttachWeb.StubWorkspaceClient`
  (BT-3308), used only by `BtAttachWeb.Live.DockTest`.

  `BtAttachWeb.Live.Dock` has a handful of degrade-gracefully branches that
  only fire on a workspace reply the shared stub deliberately never
  produces — a genuine `eval` compile failure (distinct from the facade's
  RBAC short-circuit), a `git_diff`/`git_revert_file` that fails for a real
  reason (not just an Observer denial), and a `symbols` read that actually
  resolves a class (the shared stub always reports an empty class index). The
  stub is shared by the whole suite and shaped for the success path, so
  rather than widen it with failure knobs no other test needs, this module
  wraps it: every function delegates to the stub unless the test forced a
  reply for that op via `Application.put_env(:bt_attach, :dock_fake, %{op =>
  reply})`. Mirrors `BtAttachWeb.Live.InspectorFakeClient` (BT-3305), the
  precedent this follows.
  """

  alias BtAttachWeb.StubWorkspaceClient

  # The reply this test forced for `key`, or `:error` when it forced none (in
  # which case the caller falls through to the real stub).
  defp forced(key), do: Map.fetch(Application.get_env(:bt_attach, :dock_fake, %{}), key)

  def eval(pid, code) do
    case forced(:eval) do
      {:ok, reply} -> reply
      :error -> StubWorkspaceClient.eval(pid, code)
    end
  end

  def git_diff(path) do
    case forced(:git_diff) do
      {:ok, reply} -> reply
      :error -> StubWorkspaceClient.git_diff(path)
    end
  end

  def git_revert_file(path) do
    case forced(:git_revert_file) do
      {:ok, reply} -> reply
      :error -> StubWorkspaceClient.git_revert_file(path)
    end
  end

  def symbol_index(scope) do
    case forced(:symbol_index) do
      {:ok, reply} -> reply
      :error -> StubWorkspaceClient.symbol_index(scope)
    end
  end

  # `repl_focus_class/3`'s found branch drives the System Browser's
  # `open_class/2`, which immediately re-loads protocols + categories for the
  # focused class — delegate straight through so a forced `symbol_index`
  # reply doesn't also need a matching `browse_protocols`/`browse_categories`
  # fixture.
  defdelegate browse_protocols(class, side), to: StubWorkspaceClient
  defdelegate browse_categories(class), to: StubWorkspaceClient
end

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
  alias BtAttachWeb.Live.DockFakeClient
  alias BtAttachWeb.StubWorkspaceClient

  setup do
    Application.put_env(:bt_attach, :workspace_client, StubWorkspaceClient)
    {:ok, _} = StubWorkspaceClient.start_state()

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      Application.delete_env(:bt_attach, :dock_fake)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  # Swap the workspace client for `DockFakeClient` and force the replies in
  # `overrides` (see that module's doc) — the degrade-gracefully branches the
  # shared stub's success-shaped replies never reach.
  defp force_workspace_replies(overrides) when is_map(overrides) do
    Application.put_env(:bt_attach, :dock_fake, overrides)
    Application.put_env(:bt_attach, :workspace_client, DockFakeClient)
  end

  # The most recently `stream_insert`-ed `:repl` entry (streams prepend, so
  # the head of `inserts` is the latest) — lets a test assert the actual
  # `request`/`response`/`kind` of an appended scrollback row instead of only
  # the side-effecting assigns (`repl_seq`/`repl_terms`).
  defp last_repl_entry(socket) do
    {_id, _at, item, _limit} = hd(socket.assigns.streams.repl.inserts)
    item
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
      # do_it never pushes the inline-insert event (print_it's affordance) —
      # assert the specific event is absent rather than the whole push list,
      # so an unrelated future push doesn't make this pass vacuously.
      pushed_events = socket.private.live_temp[:push_events] || []
      refute Enum.any?(pushed_events, &match?(["ws_insert_result", _], &1))
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

    test "a class name shaped as an injection attempt is rejected — `valid_class_name?/1` is the only guard before it is interpolated into a raw `evaluate` expression" do
      {:noreply, socket} =
        Dock.handle_event(
          "flush_destructive",
          %{"class" => "Foo. Session current clear"},
          base_socket()
        )

      assert socket.assigns.save_error == "Invalid class name."
      # The flush_result stays nil: rejected before any eval dispatch, so
      # nothing this class-shape check exists to prevent actually ran.
      assert socket.assigns.flush_result == nil
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

  describe "repl_inspect" do
    test "docked mode inspects the stashed REPL result term" do
      socket = base_socket(%{repl_terms: %{"repl-entry-1" => 42}})

      {:noreply, socket} =
        Dock.handle_event("repl_inspect", %{"id" => "repl-entry-1"}, socket)

      assert socket.assigns.inspect_target != nil
      assert socket.assigns.inspect_crumbs == [%{label: "REPL result", term: 42}]
    end

    test "float mode opens a floating window instead of the docked pane" do
      socket =
        base_socket(%{repl_terms: %{"repl-entry-1" => 42}, inspector_mode: "float"})

      {:noreply, socket} =
        Dock.handle_event("repl_inspect", %{"id" => "repl-entry-1"}, socket)

      assert socket.assigns.inspect_target == nil
      assert [%{label: "REPL result"}] = socket.assigns.windows
    end

    test "an unknown / stale entry id is a no-op, not a crash" do
      socket = base_socket(%{repl_terms: %{}})

      assert {:noreply, ^socket} =
               Dock.handle_event("repl_inspect", %{"id" => "repl-entry-99"}, socket)
    end
  end

  describe "eval target — tracked selection vs whole buffer" do
    test "a non-blank selection is evaluated instead of the submitted expr" do
      socket = base_socket(%{ws_selection: %{text: "Object subclass: Foo", start: 0, end: 20}})

      {:noreply, socket} = Dock.handle_event("eval", %{"expr" => "ignored buffer text"}, socket)

      # The stub only recognises a class-defining send, so the result
      # reflects the SELECTION having been evaluated, not the literal expr.
      assert socket.assigns.result =~ "Foo"
    end

    test "a blank/whitespace-only selection falls back to the submitted expr" do
      socket = base_socket(%{ws_selection: %{text: "   ", start: 0, end: 3}})

      {:noreply, socket} =
        Dock.handle_event("eval", %{"expr" => "Object subclass: Bar"}, socket)

      assert socket.assigns.result =~ "Bar"
    end

    test "a selection with no trailing offset anchors the inline result at the buffer end (ws_anchor/1 fallback)" do
      socket = base_socket(%{ws_selection: %{text: "Object subclass: Baz", start: 0, end: nil}})

      {:noreply, socket} = Dock.handle_event("eval", %{"expr" => "ignored"}, socket)

      assert [["ws_insert_result", %{anchor: nil}]] = socket.private.live_temp[:push_events]
    end
  end

  describe "eval / flush_destructive — genuine workspace op failures (not RBAC)" do
    test "a real eval compile failure renders render_error/1 and the captured output" do
      force_workspace_replies(%{eval: {:error, :compile_failed, "line 1: syntax error", []}})

      {:noreply, socket} = Dock.handle_event("eval", %{"expr" => "bad code"}, base_socket())

      assert socket.assigns.result == nil
      assert socket.assigns.output == "line 1: syntax error"
      assert socket.assigns.error =~ "compile_failed"
    end

    test "a real eval failure with no captured output leaves output blank (present/1's nil clause)" do
      force_workspace_replies(%{eval: {:error, :boom, nil, []}})

      {:noreply, socket} = Dock.handle_event("eval", %{"expr" => "bad code"}, base_socket())

      assert socket.assigns.output == nil
      assert socket.assigns.error =~ "boom"
    end

    test "a destructive flush whose eval fails outright surfaces render_error/1" do
      force_workspace_replies(%{eval: {:error, :compile_failed, "some output", []}})

      {:noreply, socket} =
        Dock.handle_event("flush_destructive", %{"class" => "Foo"}, base_socket())

      assert socket.assigns.flush_result == nil
      assert socket.assigns.save_error =~ "compile_failed"
    end

    test "a destructive flush whose eval is refused (facade short-circuit shape) surfaces facade_error/1" do
      force_workspace_replies(%{eval: {:error, :forbidden_op}})

      {:noreply, socket} =
        Dock.handle_event("flush_destructive", %{"class" => "Foo"}, base_socket())

      assert socket.assigns.flush_result == nil
      assert socket.assigns.save_error == "Operation not permitted."
    end
  end

  describe "flush_destructive — rename-method kind" do
    test "a successful destructive flush of a rename-method row reports the method-rename message" do
      {:noreply, socket} =
        Dock.handle_event(
          "flush_destructive",
          %{"class" => "Foo", "kind" => "rename-method"},
          base_socket()
        )

      assert socket.assigns.flush_result == "Flushed the pending method rename on Foo"
    end
  end

  describe "git panel — malformed unstage/revert/commit fallbacks (not RBAC — plain params catch-alls)" do
    test "a malformed git_unstage payload surfaces a validation error" do
      {:noreply, socket} = Dock.handle_event("git_unstage", %{}, base_socket())

      assert socket.assigns.git_error == "Invalid unstage request."
    end

    test "a malformed git_revert payload surfaces a validation error" do
      {:noreply, socket} = Dock.handle_event("git_revert", %{}, base_socket())

      assert socket.assigns.git_error == "Invalid revert request."
    end

    test "a malformed git_commit payload surfaces a validation error" do
      {:noreply, socket} = Dock.handle_event("git_commit", %{}, base_socket())

      assert socket.assigns.git_error == "Invalid commit request."
    end
  end

  describe "git panel — genuine op failures, distinct from RBAC denial" do
    test "git_diff surfaces a real workspace failure (git_diff's own {:error, reason} branch)" do
      force_workspace_replies(%{git_diff: {:error, :git_unreachable}})

      {:noreply, socket} = Dock.handle_event("git_diff", %{"path" => "src/Foo.bt"}, base_socket())

      assert socket.assigns.git_error =~ "git_unreachable"
    end

    test "git_revert surfaces a real workspace failure, distinct from the pending-edit guard" do
      force_workspace_replies(%{git_revert_file: {:error, :git_conflict}})

      {:noreply, socket} =
        Dock.handle_event("git_revert", %{"path" => "src/Foo.bt"}, base_socket())

      assert socket.assigns.git_error =~ "git_conflict"
    end

    test "a pending-edit row recorded with a non-binary source_file never blocks a revert (paths_match?/2 fallback)" do
      socket =
        base_socket(%{
          changes: [%{class: "Foo", selector: "bar"}],
          browser_classes: [%{"name" => "Foo", "source_file" => nil}]
        })

      {:noreply, socket} = Dock.handle_event("git_revert", %{"path" => "src/Foo.bt"}, socket)

      # `paths_match?(nil, "src/Foo.bt")` falls to its non-binary fallback
      # clause (`false`), so the row never matches the reverted path and the
      # unflushed-edits guard never blocks — the revert proceeds to its
      # ordinary success path instead (a clean revert clears `git_error`).
      assert socket.assigns.git_error == nil
    end
  end

  describe "handle_async(:git_load, …) — status/log read failures" do
    test "a status-read failure surfaces its own reason and is not overwritten by a subsequent log failure" do
      {:noreply, socket} =
        Dock.handle_async(
          :git_load,
          {:ok, {{:error, :status_down}, {:error, :log_down}}},
          base_socket()
        )

      assert socket.assigns.git_status == nil
      assert socket.assigns.git_log == []
      # `log_failed/2`'s `do: socket` branch: the status error (surfaced
      # first) is not clobbered by the log read's own failure.
      assert socket.assigns.git_error =~ "status_down"
      refute socket.assigns.git_error =~ "log_down"
    end

    test "an unrecognised log-read shape degrades to the unexpected_git_log fallback" do
      status = %{branch: "main", upstream: nil, ahead: 0, behind: 0, files: []}

      {:noreply, socket} =
        Dock.handle_async(
          :git_load,
          {:ok, {{:ok, status}, :not_a_recognised_shape}},
          base_socket()
        )

      assert socket.assigns.git_log == []
      assert socket.assigns.git_error =~ "unexpected_git_log"
    end
  end

  describe "revert / flush — RBAC-denied (genuinely role-gated, unlike the git-panel catch-alls)" do
    test "an Observer's revert is refused by the facade" do
      {:noreply, socket} =
        Dock.handle_event(
          "revert",
          %{"class" => "Foo", "selector" => "bar"},
          base_socket(%{role: :observer})
        )

      assert socket.assigns.save_error =~ "Not authorized"
    end

    test "an Observer's flush is refused by the facade" do
      {:noreply, socket} = Dock.handle_event("flush", %{}, base_socket(%{role: :observer}))

      assert socket.assigns.flush_result == nil
      assert socket.assigns.flush_error =~ "Not authorized"
    end
  end

  describe "toggle_change_diff — malformed payload fallback" do
    test "a malformed toggle_change_diff payload is a no-op, not a crash" do
      socket = base_socket()

      assert {:noreply, ^socket} = Dock.handle_event("toggle_change_diff", %{}, socket)
    end
  end

  describe "repl_eval — the attach-failure-window clause's own guards and fallback" do
    test "no session + a blank expr is a no-op (never appends, even without :repl_history assigns wired up)" do
      socket = base_socket(%{session_pid: nil})

      assert {:noreply, ^socket} = Dock.handle_event("repl_eval", %{"expr" => "   "}, socket)
    end

    test "no session + a non-blank expr reports 'not attached' and still records history" do
      socket = base_socket(%{session_pid: nil})

      {:noreply, socket} = Dock.handle_event("repl_eval", %{"expr" => "1 + 1"}, socket)

      assert socket.assigns.repl_seq == 1
      assert socket.assigns.repl_history == ["1 + 1"]
      assert last_repl_entry(socket).response == "not attached to workspace"
    end

    test "a repl_eval event with no expr param at all is a no-op" do
      socket = base_socket()

      assert {:noreply, ^socket} = Dock.handle_event("repl_eval", %{}, socket)
    end

    test "an RBAC-denied repl eval renders the facade's short-circuit error (facade_error branch)" do
      {:noreply, socket} =
        Dock.handle_event("repl_eval", %{"expr" => "1 + 1"}, base_socket(%{role: :observer}))

      entry = last_repl_entry(socket)
      assert entry.kind == :error
      assert entry.response =~ "Not authorized"
    end
  end

  describe "repl_eval — meta-command dispatch table" do
    test "each point-style meta-command resolves to its own clause and appends an :info entry" do
      for cmd <- [":bindings", ":dirty", ":flush", ":sync", ":clear", ":show-codegen", ":quit"] do
        {:noreply, socket} = Dock.handle_event("repl_eval", %{"expr" => cmd}, base_socket())

        entry = last_repl_entry(socket)
        assert entry.kind == :info, "#{cmd} did not append an :info entry"
        assert entry.request == cmd
      end
    end

    test "an unrecognised meta-command falls to the :unknown clause" do
      {:noreply, socket} = Dock.handle_event("repl_eval", %{"expr" => ":wat"}, base_socket())

      entry = last_repl_entry(socket)
      assert entry.kind == :info
      assert entry.response =~ "Unknown command :wat"
    end

    test "a meta-command argument stripped down to nothing (all-# arg) parses to no class (meta_arg/1's \"\" clause)" do
      {:noreply, socket} = Dock.handle_event("repl_eval", %{"expr" => ":help ###"}, base_socket())

      # No argument survived stripping, so this is the same as bare `:help` —
      # the general help text, not a class lookup.
      entry = last_repl_entry(socket)
      assert entry.response =~ "IDE REPL"
    end

    test "`:help <Class>` on an unknown class reports it wasn't found" do
      {:noreply, socket} =
        Dock.handle_event("repl_eval", %{"expr" => ":help Bogus"}, base_socket())

      entry = last_repl_entry(socket)
      assert entry.response =~ "No class named Bogus"
    end

    test "`:help <Class>` on a class the live symbol index resolves focuses the System Browser" do
      force_workspace_replies(%{symbol_index: {:value, %{"classes" => [%{"name" => "Counter"}]}}})

      socket = base_socket(%{browser_side: "instance"})
      {:noreply, socket} = Dock.handle_event("repl_eval", %{"expr" => ":help Counter"}, socket)

      assert socket.assigns.selected_class == "Counter"
      entry = last_repl_entry(socket)
      assert entry.response =~ "Opened Counter in the System Browser"
    end

    test "a successful eval of a `Beamtalk help: Class` send also focuses the System Browser" do
      socket = base_socket(%{browser_side: "instance"})

      {:noreply, socket} =
        Dock.handle_event("repl_eval", %{"expr" => "Beamtalk help: #Counter"}, socket)

      assert socket.assigns.selected_class == "Counter"
    end
  end

  describe "dock_tab tests — re-opening keeps an already-loaded catalogue" do
    test "re-opening the Tests tab with a catalogue already loaded does not clobber it (ensure_test_classes/1 else clause)" do
      loaded = [%{"class" => "AlreadyLoaded", "selectors" => []}]
      socket = base_socket(%{test_classes: loaded})

      {:noreply, socket} = Dock.handle_event("dock_tab", %{"tab" => "tests"}, socket)

      assert socket.assigns.dock_tab == "tests"
      assert socket.assigns.test_classes == loaded
    end
  end

  describe "Dock.repl_preview/1 (public, template-facing)" do
    test "a short single-line response is shown verbatim" do
      assert Dock.repl_preview("42") == "42"
    end

    test "a long single-line response is truncated with an ellipsis" do
      long = String.duplicate("a", 100)

      assert Dock.repl_preview(long) == String.slice(long, 0, 80) <> "…"
    end

    test "a short first line of a multi-line response gets its own ellipsis marker" do
      assert Dock.repl_preview("first\nsecond") == "first …"
    end
  end

  describe "git panel mutations — success path (Owner)" do
    test "git_stage dispatches to the workspace and refreshes the panel" do
      {:noreply, socket} =
        Dock.handle_event("git_stage", %{"path" => "src/Foo.bt"}, base_socket())

      assert socket.assigns.git_error == nil
      assert {:git_stage, "src/Foo.bt"} in StubWorkspaceClient.calls()
    end

    test "git_unstage dispatches to the workspace" do
      {:noreply, socket} =
        Dock.handle_event("git_unstage", %{"path" => "src/Foo.bt"}, base_socket())

      assert socket.assigns.git_error == nil
      assert {:git_unstage, "src/Foo.bt"} in StubWorkspaceClient.calls()
    end

    test "git_commit dispatches to the workspace with the message" do
      {:noreply, socket} =
        Dock.handle_event("git_commit", %{"message" => "fix bug"}, base_socket())

      assert socket.assigns.git_error == nil
      assert {:git_commit, "fix bug"} in StubWorkspaceClient.calls()
    end
  end

  describe "flush clears the unflushed badge on only the tabs it actually wrote" do
    test "a method whose entry stopped being pending is cleared; an unrelated dirty tab is not" do
      tabs = [
        %{kind: :method, class: "Foo", selector: "bar", disk_differs: true},
        %{kind: :method, class: "Foo", selector: "untouched", disk_differs: true}
      ]

      socket = base_socket(%{changes: [%{class: "Foo", selector: "bar"}], tabs: tabs})

      {:noreply, socket} = Dock.handle_event("flush", %{}, socket)

      assert [
               %{selector: "bar", disk_differs: false},
               %{selector: "untouched", disk_differs: true}
             ] = socket.assigns.tabs
    end
  end

  describe "@dock_events coverage" do
    test "every event WorkspaceLive delegates to Dock resolves to an implemented clause" do
      params_by_event = %{
        "dock_tab" => %{"tab" => "workspace"},
        "eval" => %{"expr" => "1 + 1"},
        "select_workspace" => %{"text" => "1"},
        "repl_eval" => %{"expr" => "1 + 1"},
        "repl_history_prev" => %{},
        "repl_history_next" => %{},
        "repl_inspect" => %{"id" => "repl-entry-1"},
        "git_refresh" => %{},
        "git_diff" => %{"path" => "src/Foo.bt"},
        "git_stage" => %{"path" => "src/Foo.bt"},
        "git_unstage" => %{"path" => "src/Foo.bt"},
        "git_revert" => %{"path" => "src/Foo.bt"},
        "git_commit" => %{"message" => "msg"},
        "toggle_change_diff" => %{"class" => "Foo", "selector" => "bar"},
        "revert" => %{"class" => "Foo", "selector" => "bar"},
        "flush" => %{},
        "flush_destructive" => %{"class" => "Foo"}
      }

      # A hardcoded event-name list would itself be an unenforced "keep in
      # sync" copy of `@dock_events` — read it from `WorkspaceLive` instead,
      # so adding/removing a name on one side without the other fails here.
      for event <- BtAttachWeb.WorkspaceLive.dock_events() do
        params = Map.fetch!(params_by_event, event)

        assert {:noreply, %Phoenix.LiveView.Socket{}} =
                 Dock.handle_event(event, params, base_socket()),
               "Dock.handle_event/3 has no clause for #{inspect(event)} (or it crashed)"
      end
    end
  end
end

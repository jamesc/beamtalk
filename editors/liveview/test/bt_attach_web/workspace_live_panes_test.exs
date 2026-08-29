# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLivePanesTest do
  @moduledoc """
  The cockpit branches `BtAttachWeb.WorkspaceLive` still owns after the BT-3290
  pane extraction (BT-3306), driven through the full LiveView stack against the
  stubbed workspace client — no `:workspace` tag, so this runs in the bare
  `mix test` lane.

  What lives here (everything the extracted `Dock`/`Inspector`/`MethodEditor`/
  `SystemBrowser`/`TestRunner` modules do NOT own):

    * the omni-search palette — the symbol-index flattening, ranked filtering,
      and the class / selector / unrecognised open paths;
    * the panel-visibility toggles and the pass-through `handle_info`/
      `handle_async` clauses (unknown message, per-session `BindingChanged`
      filtering, the coalesced class-lifecycle refresh, a crashed mount load);
    * the degraded read folds — an unreachable or nonsense `changes`/
      `bindings`/`autoflush`/`browse_classes` reply must render an error pane,
      never crash the socket;
    * render-only helpers reached solely through the template: the git
      porcelain state labels, the ChangeLog Kind/Side labels and their
      destructive-tier confirm prompts, the structured unified-diff line
      classes, and the reload-findings rows.
  """
  # Mutates global app env, so not async.
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias BtAttachWeb.SessionProbe
  alias BtAttachWeb.StubWorkspaceClient
  alias BtAttachWeb.WorkspaceLive

  # ── test workspace clients ──────────────────────────────────────────────────

  defmodule SymbolClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    # The `nav-symbols` outline: two classes with locally-defined selectors
    # (instance- and class-side), plus a malformed row the flattener must drop
    # rather than crash on.
    def symbol_index(_scope) do
      {:value,
       %{
         "classes" => [
           %{
             "name" => "Counter",
             "methods" => [
               %{"selector" => "increment"},
               %{"selector" => "startingAt:", "class_side" => true},
               # No selector: skipped by the comprehension's `is_binary` filter.
               %{"class_side" => false}
             ]
           },
           %{"name" => "CounterView", "methods" => []},
           %{"no_name" => true}
         ]
       }}
    end
  end

  defmodule DegradedReadsClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    def browse_classes, do: {:error, :nodedown}
    def change_history, do: {:error, :nodedown}
  end

  defmodule NonsenseReadsClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    # Shapes no contract allows — an old workspace, a half-decoded reply. Each
    # must degrade to an empty pane with an error, never crash the LiveView.
    def change_history, do: :not_a_list
    def list_bindings(_pid), do: :not_a_list
    def autoflush, do: :not_a_boolean
  end

  defmodule ReplClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    # One canned answer per REPL entry shape the scrollback renders.
    def eval(_pid, code) do
      if String.contains?(code, "boom"),
        do: {:error, :not_understood, "", []},
        else: {:ok, "stub-result", "", []}
    end
  end

  defmodule CrashingMountClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    def browse_classes, do: raise("simulated browse_classes crash")
  end

  setup do
    {:ok, _} = SessionProbe.start_link()
    {:ok, _} = StubWorkspaceClient.start_state()
    Application.put_env(:bt_attach, :workspace_client, SessionProbe.Client)

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  defp mount_cockpit(conn) do
    {:ok, view, _html} = live(conn, "/")
    # The mount-time reads run off-socket (BT-2591) — settle them so the panes
    # hold real data before a test drives them.
    render_async(view, 5_000)
    view
  end

  # ── omni search (BT-2495) ───────────────────────────────────────────────────

  describe "omni search" do
    setup do
      Application.put_env(:bt_attach, :workspace_client, SymbolClient)
      :ok
    end

    test "a query ranks matching class and selector rows into the popover", %{conn: conn} do
      view = mount_cockpit(conn)

      html = render_hook(view, "omni_search", %{"value" => "counter"})

      # Prefix matches rank ahead of substring matches, and a class-side
      # selector row is tagged as such.
      assert html =~ ~s(class="omni-results")
      assert html =~ "Counter » increment"
      assert html =~ "Counter » startingAt: (class)"
      assert html =~ "CounterView"
    end

    test "an unchanged query is a no-op, and a blank query closes the popover", %{conn: conn} do
      view = mount_cockpit(conn)

      opened = render_hook(view, "omni_search", %{"value" => "counter"})
      assert opened =~ ~s(class="omni-results")

      # `phx-keyup` fires on every key release (arrows/enter included): an
      # identical query must not re-render the list out from under the hook's
      # client-side highlight.
      assert render_hook(view, "omni_search", %{"value" => "counter"}) == opened

      # Whitespace-only is an empty query: the popover closes.
      refute render_hook(view, "omni_search", %{"value" => "   "}) =~ ~s(class="omni-results")

      # A payload with no value at all is ignored outright.
      refute render_hook(view, "omni_search", %{}) =~ ~s(class="omni-results")
    end

    test "opening a class row points the System Browser at it", %{conn: conn} do
      view = mount_cockpit(conn)
      render_hook(view, "omni_search", %{"value" => "counter"})

      html = render_hook(view, "omni_open", %{"kind" => "class", "class" => "Counter"})

      refute html =~ ~s(class="omni-results")
      assert html =~ "Counter"
      assert Process.alive?(view.pid)
    end

    test "opening a selector row opens its method tab", %{conn: conn} do
      view = mount_cockpit(conn)
      render_hook(view, "omni_search", %{"value" => "increment"})

      html =
        render_hook(view, "omni_open", %{
          "kind" => "selector",
          "class" => "Counter",
          "side" => "instance",
          "selector" => "increment"
        })

      refute html =~ ~s(class="omni-results")
      assert html =~ "increment"
      assert Process.alive?(view.pid)
    end

    test "an unrecognised open payload and an explicit close both just dismiss", %{conn: conn} do
      view = mount_cockpit(conn)

      render_hook(view, "omni_search", %{"value" => "counter"})
      refute render_hook(view, "omni_open", %{"kind" => "mystery"}) =~ ~s(class="omni-results")

      render_hook(view, "omni_search", %{"value" => "counter"})
      refute render_hook(view, "omni_close", %{}) =~ ~s(class="omni-results")
      assert Process.alive?(view.pid)
    end
  end

  # ── panel visibility ────────────────────────────────────────────────────────

  describe "panel visibility toggles" do
    test "the Inspector and dock panels toggle independently", %{conn: conn} do
      view = mount_cockpit(conn)

      hidden_inspector = render_click(view, "toggle_inspector", %{})
      assert render_click(view, "toggle_inspector", %{}) != hidden_inspector

      hidden_dock = render_click(view, "toggle_dock", %{})
      assert render_click(view, "toggle_dock", %{}) != hidden_dock
      assert Process.alive?(view.pid)
    end
  end

  # ── push streams + async plumbing ───────────────────────────────────────────

  describe "push handling" do
    test "a bindings push refreshes only this session's pane", %{conn: conn} do
      StubWorkspaceClient.put_bindings([{"x", 41}])
      view = mount_cockpit(conn)
      session_id = SessionProbe.session_id()

      # Another session's eval must not force this pane to re-read.
      StubWorkspaceClient.put_bindings([{"x", 42}])
      send(view.pid, binding_changed(%{sessionId: "phoenix-someone-else"}))
      assert render(view) =~ "41"

      # This session's own change does.
      send(view.pid, binding_changed(%{sessionId: session_id}))
      assert render(view) =~ "42"
    end

    test "an unrecognised message is ignored", %{conn: conn} do
      view = mount_cockpit(conn)

      send(view.pid, :something_no_clause_matches)

      assert render(view) =~ ~s(id="system-browser")
      assert Process.alive?(view.pid)
    end

    test "inspector pushes are forwarded without a docked watch", %{conn: conn} do
      view = mount_cockpit(conn)

      # Nothing is being inspected, so each push is a no-op forward to the
      # Inspector pane rather than a crash on an absent watch.
      send(view.pid, {:object_changed, self(), %{}})
      send(view.pid, :do_object_refresh)
      send(view.pid, {:do_window_refresh, self()})

      assert render(view) =~ ~s(id="system-browser")
      assert Process.alive?(view.pid)
    end

    test "a class-lifecycle burst coalesces into a single source refresh", %{conn: conn} do
      view = mount_cockpit(conn)

      # The first push arms the deferred refresh; the second collapses into it.
      send(view.pid, class_loaded())
      send(view.pid, class_loaded())
      assert render(view) =~ ~s(id="system-browser")

      # Let the coalescing window elapse so the single refresh actually runs.
      Process.sleep(150)
      assert render(view) =~ ~s(id="system-browser")
      assert Process.alive?(view.pid)
    end

    test "a crashed mount load leaves the panes in their empty state", %{conn: conn} do
      Application.put_env(:bt_attach, :workspace_client, CrashingMountClient)

      {:ok, view, _html} = live(conn, "/")
      render_async(view, 5_000)

      assert render(view) =~ "No classes in the image yet."
      assert Process.alive?(view.pid)
    end

    test "a cancelled mount load is a no-op" do
      socket = %Phoenix.LiveView.Socket{}

      assert {:noreply, ^socket} =
               WorkspaceLive.handle_async(:mount_load, {:exit, :cancelled}, socket)
    end
  end

  # ── degraded reads ──────────────────────────────────────────────────────────

  describe "degraded workspace reads" do
    test "unreachable class/changes reads render errors, not a crash", %{conn: conn} do
      Application.put_env(:bt_attach, :workspace_client, DegradedReadsClient)

      view = mount_cockpit(conn)
      html = render_hook(view, "dock_tab", %{"tab" => "changes"})

      # The class tree shows its error rather than an empty-image claim, and the
      # Changes pane carries the rendered reason.
      assert html =~ ~s(id="system-browser")
      assert html =~ ":nodedown"

      # A later class-lifecycle push must not mark the errored surfaces loaded.
      send(view.pid, class_loaded())
      Process.sleep(150)
      assert Process.alive?(view.pid)
    end

    test "nonsense reply shapes degrade each pane to an error", %{conn: conn} do
      Application.put_env(:bt_attach, :workspace_client, NonsenseReadsClient)

      view = mount_cockpit(conn)
      html = render_hook(view, "dock_tab", %{"tab" => "changes"})

      assert html =~ "No pending changes."
      assert html =~ ":unexpected_response"
      assert Process.alive?(view.pid)
    end
  end

  # ── render-only helpers ─────────────────────────────────────────────────────

  describe "git pane" do
    test "every porcelain state, the upstream counts, diffs and log render", %{conn: conn} do
      StubWorkspaceClient.set_git_status(
        {:ok,
         %{
           branch: "main",
           upstream: "origin/main",
           ahead: 2,
           behind: 1,
           files: [
             %{path: "src/a.bt", index: :added, worktree: :deleted},
             %{path: "src/b.bt", index: :renamed, worktree: :copied},
             %{path: "src/c.bt", index: :unmerged, worktree: :untracked},
             %{path: "src/d.bt", index: :ignored, worktree: :type_changed},
             %{path: "src/e.bt", index: :some_future_state, worktree: :modified}
           ]
         }}
      )

      StubWorkspaceClient.set_git_log(
        {:ok, [%{short_sha: "abc1234", subject: "Add Counter", author: "alice"}]}
      )

      StubWorkspaceClient.set_git_diff(
        {:ok,
         %{
           staged: "diff --git a/src/a.bt b/src/a.bt\n@@ -1 +1 @@\n-old\n+new\n context\n",
           worktree: ""
         }}
      )

      view = mount_cockpit(conn)
      render_hook(view, "dock_tab", %{"tab" => "git"})
      html = render_async(view, 5_000)

      assert html =~ "↑2 ↓1"

      # One Staged/Working cell per porcelain state `beamtalk_git` classifies.
      for label <- ~w(added deleted renamed copied unmerged untracked ignored type-changed) do
        assert html =~ "<td>#{label}</td>"
      end

      # An unrecognised future state falls back to its raw name.
      assert html =~ "<td>some_future_state</td>"
      assert html =~ "abc1234"
      assert html =~ "Add Counter"

      # The structured diff classifies hunk headers and file metadata lines
      # distinctly from the +/- body.
      diffed = render_click(view, "git_diff", %{"path" => "src/a.bt"})
      assert diffed =~ "diff-hunk"
      assert diffed =~ "diff-meta"
      assert diffed =~ "diff-add"
    end
  end

  describe "changes pane" do
    test "each ChangeLog kind gets its own label and destructive affordance", %{conn: conn} do
      for row <- [
            change_row(%{class: "Counter", selector: nil, kind: "class-def"}),
            change_row(%{class: "Ledger", selector: nil, kind: "remove-class"}),
            change_row(%{class: "Ledger", selector: nil, kind: "rename-class"}),
            change_row(%{
              class: "Counter",
              selector: "bump",
              kind: "rename-method",
              side: "class"
            }),
            change_row(%{class: "Counter", selector: "tick", kind: "rename-method", side: nil}),
            change_row(%{class: "Counter", selector: "sum", kind: "some-future-kind"})
          ] do
        StubWorkspaceClient.seed_change_row(row)
      end

      view = mount_cockpit(conn)
      html = render_hook(view, "dock_tab", %{"tab" => "changes"})

      assert html =~ "class definition"
      assert html =~ "remove class"
      assert html =~ "rename class"
      assert html =~ "rename (class)"
      assert html =~ "rename (?)"
      assert html =~ "some-future-kind"

      # The Tier-2 rows carry their own independently-confirmed gestures.
      assert html =~ "delete file"
      assert html =~ "apply rename"
      assert html =~ "Rename to Ledger on disk?"
      assert html =~ "Rename bump on Counter class on disk?"
    end

    test "a row's net-vs-disk diff expands into structured lines", %{conn: conn} do
      StubWorkspaceClient.seed_change_row(
        change_row(%{
          class: "Counter",
          selector: "increment",
          kind: "instance",
          side: "instance",
          diff: "--- a/src/counter.bt\n+++ b/src/counter.bt\n@@ -1 +1 @@\n-old\n+new\n same\n"
        })
      )

      view = mount_cockpit(conn)
      render_hook(view, "dock_tab", %{"tab" => "changes"})

      expanded =
        render_click(view, "toggle_change_diff", %{
          "class" => "Counter",
          "selector" => "increment",
          "entry-side" => "instance"
        })

      assert expanded =~ "diff-meta"
      assert expanded =~ "diff-hunk"
      assert expanded =~ "diff-ctx"
    end

    test "a finding with no recorded call sites still renders a row", %{conn: conn} do
      StubWorkspaceClient.put_reload_findings([
        %{
          owner: "Ledger",
          changed_class: "Counter",
          selector: "increment",
          classification: :signature_change,
          severity: "warning",
          category: "reload",
          message: "Counter>>increment changed arity",
          note: "re-check after fixing the caller",
          sites: []
        },
        %{
          owner: "Report",
          changed_class: "Counter",
          selector: "increment",
          classification: :signature_change,
          severity: "error",
          category: "reload",
          message: "Counter>>increment is gone",
          note: nil,
          sites: [%{method: "render", line: 12}]
        }
      ])

      view = mount_cockpit(conn)
      html = render_hook(view, "dock_tab", %{"tab" => "changes"})

      assert html =~ "Counter&gt;&gt;increment changed arity"
      assert html =~ "re-check after fixing the caller"
      assert html =~ "Report&gt;&gt;render (line 12)"
    end
  end

  describe "rename modal" do
    test "a definition tab renames its class, a method tab its selector", %{conn: conn} do
      view = mount_cockpit(conn)

      # A method tab: the modal is worded for a selector rename and pre-fills
      # the current selector.
      render_click(view, "browser_select_class", %{"class" => "Counter"})

      render_click(view, "browser_select_method", %{
        "class" => "Counter",
        "side" => "instance",
        "selector" => "increment"
      })

      method_modal = render_click(view, "open_rename", %{})
      assert method_modal =~ ~s(id="rename-modal")
      assert method_modal =~ "Rename Method"
      assert method_modal =~ "New selector for Counter increment"
      assert method_modal =~ ~s(placeholder="incrementBy")
      assert method_modal =~ ~s(aria-invalid="false")

      render_click(view, "close_rename", %{})

      # A definition tab: same modal, class wording, pre-filled with the class.
      render_click(view, "browser_open_definition", %{"class" => "Counter"})
      class_modal = render_click(view, "open_rename", %{})

      assert class_modal =~ "Rename Class"
      assert class_modal =~ "New name for Counter"
      assert class_modal =~ ~s(placeholder="Accumulator")
    end

    test "a rejected new name keeps the modal open with an inline error", %{conn: conn} do
      view = mount_cockpit(conn)
      render_click(view, "browser_open_definition", %{"class" => "Counter"})
      render_click(view, "open_rename", %{})

      html =
        view
        |> form("#rename-form", %{"new_name" => "not a class name"})
        |> render_submit()

      assert html =~ ~s(id="rename-error")
      assert html =~ ~s(aria-invalid="true")
      assert html =~ ~s(id="rename-modal")
    end
  end

  describe "REPL scrollback" do
    setup do
      Application.put_env(:bt_attach, :workspace_client, ReplClient)
      :ok
    end

    test "ok, error and info entries each render their own row shape", %{conn: conn} do
      view = mount_cockpit(conn)

      ok = render_hook(view, "repl_eval", %{"expr" => "3 + 4"})
      assert ok =~ ~s(class="repl-expr")
      # An ok entry stashes its term, so it offers the Inspect affordance.
      assert ok =~ ~s(phx-click="repl_inspect")

      errored = render_hook(view, "repl_eval", %{"expr" => "boom"})
      assert errored =~ "repl-entry err"

      # A `:`-prefixed meta-command never reaches eval: it appends an info
      # entry, and the (multi-line) help text collapses behind a summary.
      helped = render_hook(view, "repl_eval", %{"expr" => ":help"})
      assert helped =~ "repl-entry meta"
      assert helped =~ ~s(class="repl-collapse")
      assert helped =~ ~s(class="repl-summary")
    end
  end

  describe "docked inspector" do
    test "inspecting a live object renders its pid-stat chips", %{conn: conn} do
      StubWorkspaceClient.put_bindings([
        {"counter", {:beamtalk_object, :Counter, :"Elixir.Counter", self()}}
      ])

      StubWorkspaceClient.seed_pid_stats("Counter", %{
        "status" => "waiting",
        "queue_depth" => 0,
        "reductions" => 1_234_567
      })

      view = mount_cockpit(conn)
      html = render_click(view, "inspect", %{"name" => "counter"})

      assert html =~ "waiting"
      # A drained mailbox (0) is the most reassuring reading and must still show.
      assert html =~ "mailbox <b>0</b>"
      assert html =~ "1,234,567"
    end
  end

  describe "navigation popover" do
    test "the Senders query renders its own heading", %{conn: conn} do
      view = mount_cockpit(conn)
      render_click(view, "browser_select_class", %{"class" => "Counter"})

      render_click(view, "browser_select_method", %{
        "class" => "Counter",
        "side" => "instance",
        "selector" => "increment"
      })

      assert render_click(view, "senders", %{}) =~ "Senders"
    end
  end

  # ── helpers ─────────────────────────────────────────────────────────────────

  defp binding_changed(event),
    do: {:beamtalk_announcement, make_ref(), :BindingChanged, :handler, event}

  defp class_loaded,
    do: {:beamtalk_announcement, make_ref(), :ClassLoaded, :handler, %{}}

  # One ChangeLog row in the shape `entry_to_row/1` produces for a real
  # workspace (see `StubWorkspaceClient.seed_change_row/1`).
  defp change_row(overrides) do
    Map.merge(
      %{
        class: "Counter",
        selector: "increment",
        kind: "instance",
        intent: "durable",
        flushable: true,
        flushed: false,
        author_kind: "liveview",
        diff: nil
      },
      overrides
    )
  end
end

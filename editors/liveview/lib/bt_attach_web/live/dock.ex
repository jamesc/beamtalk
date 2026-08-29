# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.Dock do
  @moduledoc """
  The tabbed "Workspace dock" (BT-2490, epic BT-2482 Phase 1) — Workspace
  (eval), REPL, Transcript, and Changes/Git — extracted out of
  `BtAttachWeb.WorkspaceLive` (BT-3295, epic BT-3290) so its `handle_event/3`
  / `handle_async/3` clauses and their supporting helpers are directly
  unit-testable instead of only reachable through a full-LiveView integration
  test. Follows the same extraction shape `BtAttachWeb.Live.Inspector`
  (BT-3291) established.

  This module owns:

    * **Workspace** — the eval `<form>`'s doIt/printIt/inspectIt actions
      (`"eval"`), evaluating the tracked selection or the whole buffer via the
      SAME `eval` facade op the REPL uses, and selection tracking
      (`"select_workspace"`).
    * **REPL** (BT-2543) — `"repl_eval"`, history recall
      (`"repl_history_prev"`/`"repl_history_next"`), inspecting a `→ result`
      term (`"repl_inspect"`), and the `:`-prefixed meta-command dispatch.
    * **Changes** — the workspace ChangeLog viewer: `"revert"`, `"flush"`,
      `"flush_destructive"`, `"toggle_change_diff"`.
    * **Git** (ADR 0082 Amendment 1, BT-2586) — `"git_refresh"`, `"git_diff"`,
      `"git_stage"`, `"git_unstage"`, `"git_revert"`, `"git_commit"`, plus the
      off-socket `:git_load` async panel refresh (`handle_async/3`).
    * **`"dock_tab"`** — switches the dock's active tab, including the lazy
      first-open refreshes for the Git/Changes/Tests tabs.

  Every workspace read/write goes through `BtAttach.Facade.dispatch/3` (ADR
  0091 Decision 3) with `BtAttachWeb.Live.RequestContext` — never a raw
  `BtAttach.Workspace`/`:rpc` call — so this module never reimplements the
  `eval`/`changes`/`git_*` ops or the RBAC gates they ride (CLAUDE.md
  no-duplicate-implementations).

  **Transcript** has no events of its own (a push-only stream driven by
  `Transcript show:` subscriptions) and stays entirely in `WorkspaceLive`,
  untouched by this extraction.

  State (`:dock_tab`, `:ws_selection`, `:result`/`:output`/`:error`/`:expr`,
  `:eval_seq`, `:repl_seq`, `:repl_terms`, `:repl_history`,
  `:repl_history_pos`, `:changes`, `:expanded_changes`, `:git_status`,
  `:git_log`, `:git_diff`, `:git_diff_path`, `:git_error`, `:flush_result`,
  `:flush_error`, `:save_result`, `:save_error`) stays on the LiveView's own
  socket — initialised in `WorkspaceLive.bind_session/3` and mount, same as
  the Inspector's assigns. `WorkspaceLive` still owns `handle_event/3` /
  `handle_async/3` (`Phoenix.LiveView` callback contracts), but delegates
  every dock event/async tag to the functions here by name — see the
  `@dock_events` guard clause and the `:git_load` forward in `WorkspaceLive`.

  Several dock branches reach into state other panes own: the git-revert path
  calls `BtAttachWeb.Live.MethodEditor`'s tab-refresh helpers (extracted
  BT-3296) directly, `BtAttachWeb.Live.SystemBrowser`'s `open_class/2`
  (extracted BT-3297) focuses the System Browser on a class from the REPL
  `:help` meta-command, the Tests pane's `discover_test_classes/1` (extracted
  BT-3298, `BtAttachWeb.Live.TestRunner`) backs the `dock_tab`/`:test` lazy
  first-open, and `flush_destructive/3`'s class-name check reuses
  `BtAttachWeb.Live.ClassModals.valid_class_name?/1` (also BT-3298) — this
  module calls those public functions rather than duplicating their logic,
  exactly the temporary cross-call shape a sequential decomposition
  produces. The top-bar omni search's `symbol_rows/1` is still
  `WorkspaceLive`-owned.
  """

  use BtAttachWeb, :html

  import Phoenix.LiveView,
    only: [
      push_event: 3,
      stream_insert: 4,
      start_async: 3,
      cancel_async: 3
    ]

  require Logger

  alias BtAttach.Facade
  alias BtAttach.Workspace
  alias BtAttachWeb.Live.ClassModals
  alias BtAttachWeb.Live.FacadeError
  alias BtAttachWeb.Live.Inspector
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.RequestContext
  alias BtAttachWeb.Live.SystemBrowser
  alias BtAttachWeb.Live.TestRunner
  alias BtAttachWeb.WorkspaceLive

  # ── handle_event dispatch ────────────────────────────────────────────────
  #
  # `WorkspaceLive.handle_event/3` forwards every event whose name is in
  # `@dock_events` here unchanged (same event name, params, socket), so each
  # clause below is exactly the body the LiveView used to run directly.

  # The Workspace dock's three actions (BT-2490) all evaluate the entered code
  # (or the tracked selection, the spike's "evaluates selection vs buffer")
  # and differ only in what they do with the result term — so they ride the
  # SAME `eval` facade op and the existing `render_term` formatting rather
  # than inventing new server ops:
  #
  #   * doIt      (⌘D) — evaluate for side effects; show a terse "✓ evaluated".
  #   * printIt   (⌘P) — evaluate and show the result term (the classic eval).
  #   * inspectIt (⌘I) — evaluate, then inspect the *result term* in the
  #     Inspector (reuses `Inspector.inspect_term/4`, the same read-surface
  #     path bindings drill into).
  #
  # The clicked action rides the eval `<form>` submit as the `action` field; a
  # plain submit (or the e2e test's `render_submit(%{expr: …})`) carries no
  # action and defaults to printIt — the historical eval behaviour the tests
  # assert on.
  def handle_event("eval", %{"expr" => expr} = params, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    action = eval_action(params)
    target = eval_target(expr, socket)

    case Facade.dispatch(:eval, %{session_pid: pid, code: target}, RequestContext.build(socket)) do
      {:ok, term, output, _warnings} ->
        # eval returns the live term; rendering is display-only and reuses the
        # workspace's own formatter for surface-consistency with the browser.
        {:noreply, eval_success(socket, action, term, output, expr)}

      {:error, reason, output, _warnings} ->
        {:noreply,
         assign(socket,
           result: nil,
           output: present(output),
           error: Workspace.render_error(reason),
           expr: expr
         )}

      # The facade (BT-2420/2421) can short-circuit BEFORE dispatching to the
      # workspace — an RBAC denial (`:unauthorized`, e.g. an Observer) or an
      # off-vocabulary op (`:forbidden_op`) — returning a 2-tuple the
      # workspace eval contract never produces. Render it as an actionable
      # message rather than crashing the LiveView on an unmatched case clause.
      {:error, reason} ->
        {:noreply,
         assign(socket, result: nil, output: nil, error: FacadeError.render(reason), expr: expr)}
    end
  end

  # No session (attach failed) — don't crash on a missing assign.
  def handle_event("eval", %{"expr" => expr}, socket) do
    {:noreply,
     assign(socket, result: nil, output: nil, error: "not attached to workspace", expr: expr)}
  end

  # Switch the Workspace dock's active tab (Workspace / REPL / Transcript /
  # Changes / Tests, BT-2490, REPL added BT-2543, Tests BT-2557). Pure view
  # state — no workspace round-trip — EXCEPT the Tests tab lazily loads its
  # test catalogue the first time it is opened (discovery is a cheap `:read`
  # op, but there is no point running it for users who never open the tab).
  # An unknown tab is ignored rather than rendered, so a crafted value can't
  # blank the dock.
  def handle_event("dock_tab", %{"tab" => "tests"}, socket) do
    {:noreply, socket |> assign(dock_tab: "tests") |> ensure_test_classes()}
  end

  # ADR 0082 Amendment 1 (BT-2586): opening the Git tab refreshes the panel
  # from real git on the workspace project root (lazy first-open, like Tests).
  def handle_event("dock_tab", %{"tab" => "git"}, socket) do
    {:noreply, socket |> assign(dock_tab: "git") |> assign_git()}
  end

  # ADR 0113 (BT-3210): every *dedicated* Changes-affecting action (save,
  # revert, flush, remove class/method, new class) already refreshes
  # `@changes` itself via `assign_changes/1` — but a ChangeLog-mutating op run
  # through raw eval (`#eval-form`), e.g. `SomeClass removeFromSystem` typed
  # directly, has no such hook to call it. Refresh on open too (lazy
  # first-open, like Tests/Git above) so opening the tab is always a reliable
  # way to see current state, not just an implicit side effect of which
  # action you last took.
  def handle_event("dock_tab", %{"tab" => "changes"}, socket) do
    {:noreply, socket |> assign(dock_tab: "changes") |> WorkspaceLive.assign_changes()}
  end

  def handle_event("dock_tab", %{"tab" => tab}, socket)
      when tab in ~w(workspace repl transcript) do
    {:noreply, assign(socket, dock_tab: tab)}
  end

  def handle_event("dock_tab", _params, socket), do: {:noreply, socket}

  # ── Git panel events (ADR 0082 Amendment 1, BT-2586) ─────────────────────────
  # Read events (refresh, diff) are available to every role; the mutating
  # events (stage/unstage/commit/revert) are gated by the Facade `:execute`
  # capability *and* hidden in the template for the Observer role.

  def handle_event("git_refresh", _params, socket) do
    {:noreply, assign_git(socket)}
  end

  # Toggle the inline diff for one path: a second click on the same path
  # closes it.
  def handle_event("git_diff", %{"path" => path}, socket) when is_binary(path) do
    if socket.assigns.git_diff_path == path do
      {:noreply, assign(socket, git_diff_path: nil, git_diff: nil)}
    else
      case Facade.dispatch(:git_diff, %{path: path}, RequestContext.build(socket)) do
        {:ok, diff} when is_map(diff) ->
          {:noreply, assign(socket, git_diff_path: path, git_diff: diff, git_error: nil)}

        {:error, reason} ->
          {:noreply, assign(socket, git_error: FacadeError.render(reason))}
      end
    end
  end

  def handle_event("git_stage", %{"path" => path}, socket) when is_binary(path) do
    {:noreply, git_mutate_event(socket, :git_stage, %{path: path})}
  end

  def handle_event("git_unstage", %{"path" => path}, socket) when is_binary(path) do
    {:noreply, git_mutate_event(socket, :git_unstage, %{path: path})}
  end

  def handle_event("git_revert", %{"path" => path}, socket) when is_binary(path) do
    {:noreply, git_mutate_event(socket, :git_revert_file, %{path: path})}
  end

  def handle_event("git_commit", %{"message" => message}, socket) when is_binary(message) do
    if String.trim(message) == "" do
      {:noreply, assign(socket, git_error: "Enter a commit message.")}
    else
      {:noreply, git_mutate_event(socket, :git_commit, %{message: message})}
    end
  end

  # Malformed git payloads (missing key / non-binary value): surface a
  # git-panel validation error rather than letting a crafted WebSocket event
  # crash the LiveView, consistent with the `new_class` / `revert` fallbacks
  # in `WorkspaceLive`.
  def handle_event("git_diff", _params, socket) do
    {:noreply, assign(socket, git_error: "Invalid diff request.")}
  end

  def handle_event("git_stage", _params, socket) do
    {:noreply, assign(socket, git_error: "Invalid stage request.")}
  end

  def handle_event("git_unstage", _params, socket) do
    {:noreply, assign(socket, git_error: "Invalid unstage request.")}
  end

  def handle_event("git_revert", _params, socket) do
    {:noreply, assign(socket, git_error: "Invalid revert request.")}
  end

  def handle_event("git_commit", _params, socket) do
    {:noreply, assign(socket, git_error: "Invalid commit request.")}
  end

  # ── REPL tab (BT-2543) ───────────────────────────────────────────────────────
  #
  # The REPL is the *conversational, line-at-a-time* idiom (distinct from the
  # editor-primary Workspace and the ambient-log Transcript): a
  # request→response scrollback with the input pinned at the bottom.
  # Submitting shares the SAME `eval` facade op + session as the Workspace —
  # same structured result, same surface-shared `render_term` display rules —
  # and only differs in presentation: each submit appends a `› request` /
  # `→ response` pair to the `:repl` stream instead of inserting inline.
  # Ambient `Transcript show:` output keeps streaming to the Transcript tab
  # over the existing subscription; it is NOT duplicated into the scrollback
  # here. Per the BT-2543 confirmation, Enter submits in the REPL (terminal
  # convention) while Shift/⌘-Enter inserts a newline — the divergence is
  # enforced by the ReplInput hook, not here.
  def handle_event("repl_eval", %{"expr" => expr}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    trimmed = String.trim(expr)

    cond do
      trimmed == "" ->
        # Empty submit (bare Enter) is a no-op — never append a blank entry or
        # disturb the history cursor.
        {:noreply, socket}

      meta = repl_meta_command(trimmed) ->
        # A `:`-prefixed meta-command (BT-2543 follow-up). The IDE handles
        # these itself — driving the matching pane or pointing at it — and
        # NEVER sends them to `eval`, which would choke trying to compile
        # `:h` as Beamtalk.
        {:noreply,
         socket
         |> handle_repl_meta(meta, expr)
         |> repl_record_history(expr)
         |> repl_clear_input()}

      true ->
        socket =
          case Facade.dispatch(
                 :eval,
                 %{session_pid: pid, code: expr},
                 RequestContext.build(socket)
               ) do
            {:ok, term, _output, _warnings} ->
              socket |> repl_append_ok(expr, term) |> repl_help_followup(expr)

            {:error, reason, _output, _warnings} ->
              repl_append_error(socket, expr, Workspace.render_error(reason))

            # Facade short-circuit (RBAC denial / off-vocabulary op) — a
            # 2-tuple the eval contract never produces; render it as the
            # entry's response rather than crashing the LiveView.
            {:error, reason} ->
              repl_append_error(socket, expr, FacadeError.render(reason))
          end

        {:noreply, socket |> repl_record_history(expr) |> repl_clear_input()}
    end
  end

  # No session — the first clause's `is_pid(pid)` guard only fails when
  # bind_session never ran, so the REPL stream + assigns (`:repl_seq`,
  # `:repl_terms`, `:repl_history`, `:repl_history_pos`) don't exist and the
  # pane isn't rendered. A crafted `repl_eval` during the attach-failure
  # window must NOT touch the REPL helpers (which read those assigns) — that
  # would KeyError / crash the LiveView. Guard on the assigns' presence and
  # no-op when absent, otherwise surface the "not attached" entry (the
  # defensive path for any future state where the assigns exist but the
  # session pid is gone).
  def handle_event("repl_eval", %{"expr" => expr}, socket) do
    cond do
      not Map.has_key?(socket.assigns, :repl_seq) ->
        {:noreply, socket}

      String.trim(expr) == "" ->
        {:noreply, socket}

      true ->
        {:noreply,
         socket
         |> repl_append_error(expr, "not attached to workspace")
         |> repl_record_history(expr)
         |> repl_clear_input()}
    end
  end

  def handle_event("repl_eval", _params, socket), do: {:noreply, socket}

  # ↑/↓ history recall (BT-2543). The ReplInput hook only fires these at the
  # composer's edges (↑ on the first line, ↓ on the last), so mid-buffer
  # cursor navigation is untouched. `repl_history_pos` walks the
  # most-recent-first ring: ↑ moves further back (toward older entries), ↓
  # moves toward the present and past the newest restores the empty live
  # input. The recalled text is pushed to the hook (the input is hook-owned /
  # phx-update=ignore, so the server can't set it through morphdom).
  def handle_event("repl_history_prev", _params, socket) do
    {:noreply, repl_recall(socket, :prev)}
  end

  def handle_event("repl_history_next", _params, socket) do
    {:noreply, repl_recall(socket, :next)}
  end

  # Inspect a `→ result` term in the Inspector (BT-2543): results stay live
  # objects even in the terminal idiom. The term was stashed server-side
  # under the entry id at append time (the scrollback DOM is display-only);
  # look it up and drive the same `inspect_term` path bindings/Print-it use.
  # In `"float"` mode (BT-2493) it opens a floating window instead of the
  # docked pane. An unknown id (a stale entry after a reconnect dropped the
  # term map) is ignored.
  def handle_event("repl_inspect", %{"id" => id}, %{assigns: %{repl_terms: terms}} = socket) do
    case Map.fetch(terms, id) do
      {:ok, term} ->
        label = "REPL result"

        socket =
          if socket.assigns.inspector_mode == "float" do
            Inspector.open_window_for_term(socket, label, term)
          else
            Inspector.inspect_term(socket, label, term, [%{label: label, term: term}])
          end

        {:noreply, socket}

      :error ->
        {:noreply, socket}
    end
  end

  def handle_event("repl_inspect", _params, socket), do: {:noreply, socket}

  # Selection tracking for the Workspace dock's editor (BT-2490). Tracked in a
  # SEPARATE assign (`ws_selection`) from the method editor's `edit_selection`
  # so the dock's doIt/printIt/inspectIt evaluate *this* editor's selection —
  # a selection left in the method editor must not leak into a Workspace
  # eval. Same defensive shape as `WorkspaceLive`'s `select_source`.
  def handle_event("select_workspace", %{"text" => text} = params, socket)
      when is_binary(text) do
    selection = %{
      text: text,
      start: WorkspaceLive.clamp_offset(params["start"]),
      end: WorkspaceLive.clamp_offset(params["end"])
    }

    {:noreply, assign(socket, ws_selection: selection)}
  end

  def handle_event("select_workspace", _params, socket), do: {:noreply, socket}

  # Flush all pending durable changes to disk ("Save All to Disk", ADR 0082
  # `Workspace flush`). The summary's conflicts/skipped lists carry
  # recoverable conditions; a hard runtime failure renders as a structured
  # error.
  #
  # Deliberately Tier-1-only, unchanged by ADR 0113: a pending `remove-class`
  # entry is reported in the summary as `skipped: destructive`
  # (`beamtalk_workspace_flush`'s own vocabulary, unmodified by this
  # surface — see `Workspace.format_flush_summary/1`) rather than silently
  # deleted, so this one button staying textually the same `Workspace flush`
  # call it always was is itself the safety property ADR 0113 relies on for
  # every unmodified caller. Reaching Tier 2 is the `flush_destructive`
  # handler below (per-row "Delete file" button, ADR 0113 Surface: browser
  # row).
  def handle_event("flush", _params, socket) do
    {:noreply, flush_changes(socket)}
  end

  # "Delete file" / "Apply rename" — the browser's second, independently-
  # confirmed gesture for one pending `remove-class`/`rename-class`/
  # `rename-method` ChangeLog row (ADR 0113 Phase 4 BT-3210, extended by ADR
  # 0114 Phase 5 BT-3277's Surface table). Scoped to the one class the row
  # names, mirroring the REPL's `:flush-destructive <Class>` and the MCP
  # `flush` tool's scoped `confirm_destructive: true` form — never the
  # unscoped `flushIncludingDestructive`, since a browser click always
  # targets one named row, not "every destructive entry in the workspace".
  # `kind` rides its own `phx-value-*` attribute (defaulting to
  # `"remove-class"`, this button's original — and still most common — row)
  # only to pick the right status message on success; it plays no role in
  # which entry actually gets flushed (the `class` Symbol alone scopes that).
  def handle_event(
        "flush_destructive",
        %{"class" => class} = params,
        %{assigns: %{role: :owner}} = socket
      )
      when is_binary(class) and class != "" do
    kind = Map.get(params, "kind", "remove-class")
    {:noreply, flush_destructive(socket, class, kind)}
  end

  # Non-owner (Observer), or a crafted event with a missing/blank class: a
  # no-op — the button is rendered only for `:owner` against a real row's
  # class, mirroring `remove_class`/`revert`.
  def handle_event("flush_destructive", _params, socket), do: {:noreply, socket}

  # Revert one pending in-memory method patch (ADR 0082 Phase 5 `Workspace
  # changes revert:`, BT-2293), keyed by the `(class, selector)` carried on
  # the ChangeLog row's revert button. `side` (ADR 0112, BT-3187) is the same
  # row's `side` — required to disambiguate a same-selector instance-side
  # entry from a class-side one, which are otherwise indistinguishable by
  # `(class, selector)` alone. Refreshes the Changes pane so the fresh revert
  # entry is visible.
  def handle_event("revert", %{"class" => class, "selector" => selector} = params, socket)
      when is_binary(class) and is_binary(selector) do
    side = present_side_param(Map.get(params, "entry-side"))
    {:noreply, revert_change(socket, class, selector, side)}
  end

  # Malformed payload (missing keys / non-binary values): surface a
  # validation error rather than silently no-op'ing, consistent with
  # `new_class`/`save_method`.
  def handle_event("revert", _params, socket) do
    {:noreply, WorkspaceLive.status_error(socket, "Invalid revert request.")}
  end

  # Toggle the structured diff disclosure for one Changes-pane row (BT-2636),
  # keyed by the row's `{class, selector, side}` carried on the leading caret
  # (`phx-value-entry-side`, BT-3195 — reuses `present_side_param/1`'s `""` →
  # `nil` normalisation, the same one the revert button's
  # `phx-value-entry-side` already relies on, so both controls agree on what
  # "no side" means). Named `entry-side`, not `side`, for the same reason the
  # revert button is: it doesn't collide with the browser's `phx-value-side`
  # instance/class toggle (BT-2491), so a `button[phx-value-side=...]`
  # selector in a test doesn't match this caret too. Pure view state — no
  # workspace round-trip; flips the key in/out of `:expanded_changes`,
  # showing/hiding that row's `unified_diff` body. Before BT-3195 this keyed
  # on `{class, selector}` alone, so a same-selector instance-side and
  # class-side row (possible since BT-3187's shadow-key fix) shared one
  # toggle — expanding one flipped the other too.
  def handle_event(
        "toggle_change_diff",
        %{"class" => class, "selector" => selector} = params,
        socket
      )
      when is_binary(class) and is_binary(selector) do
    key = {class, selector, present_side_param(Map.get(params, "entry-side"))}

    expanded =
      if MapSet.member?(socket.assigns.expanded_changes, key) do
        MapSet.delete(socket.assigns.expanded_changes, key)
      else
        MapSet.put(socket.assigns.expanded_changes, key)
      end

    {:noreply, assign(socket, expanded_changes: expanded)}
  end

  def handle_event("toggle_change_diff", _params, socket), do: {:noreply, socket}

  # ── handle_async dispatch (:git_load) ────────────────────────────────────
  #
  # `WorkspaceLive.handle_async/3` forwards every `:git_load` tag here
  # unchanged, mirroring the `handle_event` dispatch above.

  # BT-2590 (S1): the off-socket git load (`assign_git/1`'s `start_async`)
  # completed. The task returned `{status_result, log_result}` — the raw
  # `Facade.dispatch` outcomes — which we fold into the panel assigns here, on
  # the LiveView process, so the render is driven from a single coherent
  # update.
  def handle_async(:git_load, {:ok, {status_result, log_result}}, socket) do
    socket =
      socket
      |> apply_git_status(status_result)
      |> apply_git_log(log_result)

    {:noreply, socket}
  end

  # The git-load task exited. A real crash surfaces as a git-panel error
  # rather than taking down the LiveView. The `:cancelled` clause is a
  # defensive no-op: `assign_git/1` `cancel_async`-es the prior load before
  # starting a new one, and while LiveView normally prunes that stale ref
  # before its exit is delivered, we guard the atom explicitly so a
  # cancellation can never render a spurious error. On a genuine failure we
  # reset BOTH status and log so the panel can't show a stale commit list
  # beside the error banner (a torn read).
  def handle_async(:git_load, {:exit, :cancelled}, socket), do: {:noreply, socket}

  def handle_async(:git_load, {:exit, reason}, socket) do
    Logger.error("git panel load crashed: #{inspect(reason)}", domain: [:beamtalk, :liveview])

    {:noreply,
     assign(socket,
       git_status: nil,
       git_log: [],
       git_error: "Couldn't load git status — the load failed unexpectedly."
     )}
  end

  # ── Workspace dock actions (BT-2490) ────────────────────────────────────────

  # Which dock action the eval submit carried. The clicked action button rides
  # the form as `action`; a plain submit (the e2e `render_submit(%{expr: …})`)
  # carries none and defaults to printIt — the historical eval behaviour.
  defp eval_action(%{"action" => action}) when action in ~w(do_it print_it inspect_it),
    do: action

  defp eval_action(_params), do: "print_it"

  # The code an action evaluates: the Workspace editor's tracked selection if
  # there is one (the spike's "evaluates selection"), else the whole entered
  # buffer ("evaluates buffer"). The Workspace editor's CmEditor hook keeps
  # `ws_selection` current (distinct from the method editor's
  # `edit_selection`); an empty or whitespace-only selection falls back to
  # the buffer.
  defp eval_target(expr, socket) do
    if ws_selection?(socket.assigns), do: socket.assigns.ws_selection.text, else: expr
  end

  # Whether the Workspace editor has a non-blank selection to evaluate. Takes
  # the bare `assigns` so `WorkspaceLive`'s render template can call it
  # directly too (public for that reason — mirrors `Inspector`'s
  # template-facing helpers).
  def ws_selection?(assigns) do
    case assigns[:ws_selection] do
      %{text: text} when is_binary(text) -> String.trim(text) != ""
      _ -> false
    end
  end

  # Render an eval success according to the chosen action (BT-2542 Workspace
  # rebuild). The growing below-editor `.ws-result` success bubble is gone;
  # the Workspace is now editor-primary, so feedback is split:
  #
  #   * print_it   — the classic Workspace "Print it": the result is inserted
  #     INLINE into the CodeMirror buffer after the evaluated region (pushed
  #     to the `CmEditor` hook as `ws_insert_result`, rendered as a
  #     collapsible block widget — NOT doc text, so "evaluate buffer" never
  #     re-runs it). A terse `→ result` also flashes in the transient status
  #     line.
  #   * do_it      — evaluate for side effects; a subtle, self-clearing
  #     `✓ evaluated` status only (no buffer insert; ambient output →
  #     Transcript).
  #   * inspect_it — show the term in the status AND open it in the
  #     Inspector.
  #
  # All three carry the result/confirmation in `result` (rendered as the thin
  # `.eval-status` line) and bump `eval_seq` to restart its fade.
  defp eval_success(socket, "do_it", _term, output, expr) do
    eval_status(socket, "✓ evaluated", output, expr)
  end

  defp eval_success(socket, "inspect_it", term, output, expr) do
    socket = eval_status(socket, "→ " <> Workspace.render_term(term), output, expr)

    # In `"float"` mode (BT-2493) Inspect-it opens a floating window on the
    # `→ result` term rather than driving the docked pane; docked mode keeps
    # the original single-pane behaviour.
    if socket.assigns.inspector_mode == "float" do
      Inspector.open_window_for_term(socket, "→ result", term)
    else
      Inspector.inspect_term(socket, "→ result", term, [%{label: "→ result", term: term}])
    end
  end

  # print_it (and the historical default): insert the result inline in the
  # buffer AND flash it in the status line.
  defp eval_success(socket, _print_it, term, output, expr) do
    rendered = Workspace.render_term(term)
    # Capture the anchor from the ORIGINAL socket before the pipe:
    # `eval_status` doesn't touch `ws_selection`, but binding it here makes
    # that independence explicit and survives a future pipe stage that might
    # clear the selection.
    anchor = ws_anchor(socket)

    # `push_event` is page-wide — every CmEditor hook that registered the
    # handler receives it. Scope it to the Workspace editor by element id
    # (the client drops a mismatched target) so a second inline editor
    # (BT-2543) can't also insert this result. The id is the shared
    # `workspace_editor_id/0` so the target and the template host can't
    # drift apart.
    socket
    |> eval_status("→ " <> rendered, output, expr)
    |> push_event("ws_insert_result", %{
      text: rendered,
      anchor: anchor,
      target: workspace_editor_id()
    })
  end

  # DOM id of the Workspace CodeMirror editor host — the single source of
  # truth shared by the template element and the `ws_insert_result` push
  # target, so a rename can't silently break inline results (the client
  # guard discards a push whose target doesn't match `this.el.id`). Public:
  # `WorkspaceLive`'s render template calls it directly.
  def workspace_editor_id, do: "workspace-editor-overlay"

  # The doc offset to anchor an inline result after: the end of the tracked
  # selection (the evaluated region) when evaluating a selection, else nil so
  # the client falls back to the live buffer end. Echoed in the
  # `ws_insert_result` push so a cursor move during the eval round-trip
  # (wider over a remote distribution node) can't drop the widget on the
  # wrong line.
  defp ws_anchor(socket) do
    if ws_selection?(socket.assigns) do
      case socket.assigns.ws_selection[:end] do
        offset when is_integer(offset) -> offset
        _ -> nil
      end
    end
  end

  # Assign the transient eval-status line + bump its re-key sequence. Shared
  # by every success branch so the status fade restarts consistently per
  # eval.
  defp eval_status(socket, status, output, expr) do
    assign(socket,
      result: status,
      output: present(output),
      error: nil,
      expr: expr,
      eval_seq: socket.assigns.eval_seq + 1
    )
  end

  # Blank captured output is not worth rendering a pane for.
  defp present(""), do: nil
  defp present(nil), do: nil
  defp present(output) when is_binary(output), do: output

  # ── REPL helpers (BT-2543) ───────────────────────────────────────────────────

  # Cap on the REPL scrollback depth: the client keeps the most recent N
  # entries (via `stream_insert(:repl, …, limit: -N)`) and `repl_terms` is
  # evicted in lockstep, so a long session can't grow the DOM or the assigns
  # map unbounded.
  @repl_scrollback_limit 200

  # Append a successful `› request` / `→ response` pair to the scrollback and
  # stash the live result term under the entry id so a later Inspect click
  # can re-open it. The response is the surface-shared `render_term`
  # rendering — the SAME string the Workspace `→ result` shows — so the two
  # surfaces stay display-consistent. Long responses are marked so the
  # template can collapse them within the entry rather than letting one
  # result flood the scrollback.
  defp repl_append_ok(socket, request, term) do
    seq = socket.assigns.repl_seq + 1
    id = repl_entry_id(seq)
    response = Workspace.render_term(term)

    entry = %{
      id: id,
      request: request,
      kind: :ok,
      response: response,
      inspectable: true,
      long: repl_long?(response)
    }

    socket
    |> assign(:repl_seq, seq)
    # `repl_terms` is bounded in step with the scrollback (below): stash this
    # term, then evict the one that just scrolled past the depth cap. Each
    # entry is a small reference (the object lives in the workspace
    # process), but a long session would still grow the map unboundedly
    # without this.
    |> update(:repl_terms, fn terms -> terms |> Map.put(id, term) |> repl_evict(seq) end)
    |> stream_insert(:repl, entry, limit: -@repl_scrollback_limit)
    |> repl_scroll_to_bottom()
  end

  # Append an error entry: the `→ response` carries the rendered error and
  # there is no live term to inspect, so no Inspect affordance and nothing
  # stashed.
  defp repl_append_error(socket, request, message) do
    seq = socket.assigns.repl_seq + 1
    id = repl_entry_id(seq)

    entry = %{
      id: id,
      request: request,
      kind: :error,
      response: message,
      inspectable: false,
      long: repl_long?(message)
    }

    socket
    |> assign(:repl_seq, seq)
    # An error entry stashes no term, but still bump the cap so an OK term
    # that scrolled past the depth limit (counting errors too) is evicted in
    # step.
    |> update(:repl_terms, &repl_evict(&1, seq))
    |> stream_insert(:repl, entry, limit: -@repl_scrollback_limit)
    |> repl_scroll_to_bottom()
  end

  # Cap the scrollback depth (client DOM via `stream_insert(limit:)`) and the
  # `repl_terms` map in lockstep. Entry ids are monotonic (`repl-entry-N`, N
  # = seq), so the entry now `@repl_scrollback_limit` positions back is
  # exactly the one the client dropped — evict its term (a no-op for an
  # error entry, which was never stashed).
  defp repl_evict(terms, seq) do
    evicted = seq - @repl_scrollback_limit
    if evicted > 0, do: Map.delete(terms, repl_entry_id(evicted)), else: terms
  end

  # Scroll the scrollback to the newest entry on each append (classic
  # terminal behaviour): a new submission should reveal its result even if
  # the user had scrolled up to read older output. The scroll is a pure
  # client effect, so it rides a push to the ReplInput hook rather than a
  # re-render.
  defp repl_scroll_to_bottom(socket) do
    push_event(socket, "repl_scroll_bottom", %{})
  end

  defp repl_entry_id(seq), do: "repl-entry-#{seq}"

  # DOM id of the REPL input editor host — the single source of truth shared
  # by the template element and the `repl_set_input` push target, so a
  # rename can't silently break history recall / submit-clear. (There is a
  # single ReplInput instance on the page; `push_event/3` reaches every hook
  # registered for the event, so this id is for template/push-target
  # consistency, not filtering.) Public: `WorkspaceLive`'s render template
  # calls it directly.
  def repl_input_id, do: "repl-input"

  # First-line (capped) preview shown in a collapsed long response's
  # `<summary>`: enough to recognise the result without expanding, with an
  # ellipsis when the full text is longer. Public: `WorkspaceLive`'s render
  # template calls it directly.
  def repl_preview(text) when is_binary(text) do
    first = text |> String.split("\n", parts: 2) |> hd()

    cond do
      String.length(first) > 80 -> String.slice(first, 0, 80) <> "…"
      first != text -> first <> " …"
      true -> first
    end
  end

  # A response is "long" (worth collapsing within its entry) when it spills
  # past a handful of lines or a few hundred chars — the threshold that
  # keeps a single verbose result from pushing the rest of the scrollback
  # off-screen.
  defp repl_long?(text) when is_binary(text) do
    # `parts: 7` short-circuits the split after 6 newlines instead of
    # materialising every line just to count past six.
    String.length(text) > 320 or length(String.split(text, "\n", parts: 7)) > 6
  end

  # Record a submitted expression at the head of the recall ring and reset
  # the ↑/↓ cursor to the live input. Every prior occurrence of the
  # expression is dropped first so re-running the same expression doesn't
  # bloat the ring (all earlier copies collapse onto the new head, shell
  # `HISTCONTROL=erasedups` style — not just consecutive runs); the ring is
  # capped so a long session can't grow the assigns unbounded.
  @repl_history_limit 100
  defp repl_record_history(socket, expr) do
    history =
      [expr | Enum.reject(socket.assigns.repl_history, &(&1 == expr))]
      |> Enum.take(@repl_history_limit)

    assign(socket, repl_history: history, repl_history_pos: nil)
  end

  # Walk the recall ring and push the recalled text to the input. `:prev` (↑)
  # moves toward older entries; `:next` (↓) moves toward the present, and
  # stepping past the newest restores the empty live input (pos = nil). An
  # empty ring or a ↓ while already at the live input is a no-op (no push,
  # so the hook keeps the in-progress text the user was typing).
  #
  # The first clause also covers the attach-failure window: when
  # bind_session never ran, `:repl_history` is absent, so a crafted ↑/↓ must
  # NOT fall through to `socket.assigns.repl_history` (a KeyError that would
  # crash the LiveView) — `is_map_key/2` guards it to a no-op.
  defp repl_recall(socket, _dir) when not is_map_key(socket.assigns, :repl_history),
    do: socket

  defp repl_recall(%{assigns: %{repl_history: []}} = socket, _dir), do: socket

  defp repl_recall(socket, dir) do
    history = socket.assigns.repl_history
    pos = socket.assigns.repl_history_pos
    last = length(history) - 1

    new_pos =
      case {dir, pos} do
        {:prev, nil} -> 0
        {:prev, p} -> min(p + 1, last)
        {:next, nil} -> :live
        {:next, 0} -> nil
        {:next, p} -> p - 1
      end

    case new_pos do
      :live ->
        # ↓ at the live input: nothing to recall, leave the user's draft
        # alone.
        socket

      nil ->
        socket
        |> assign(:repl_history_pos, nil)
        |> push_event("repl_set_input", %{text: ""})

      p ->
        socket
        |> assign(:repl_history_pos, p)
        |> push_event("repl_set_input", %{text: Enum.at(history, p)})
    end
  end

  # Clear the REPL input after a submit (REPL convention: submit empties the
  # composer). The input is hook-owned (phx-update=ignore), so the server
  # can only set it by pushing to the ReplInput hook.
  defp repl_clear_input(socket) do
    push_event(socket, "repl_set_input", %{text: ""})
  end

  # ── REPL meta-commands (BT-2543 follow-up) ──────────────────────────────────
  #
  # The CLI REPL parses `:`-prefixed meta-commands client-side (see
  # crates/beamtalk-cli/src/commands/repl/mod.rs); the LiveView REPL
  # historically forwarded them straight to `eval`, which choked trying to
  # compile `:h` as a Beamtalk expression. In a graphical IDE most of those
  # commands map onto a pane that already exists (the System Browser, the
  # Bindings pane, the Changes tab), so rather than re-implement the CLI's
  # command DSL we recognise the leading colon and either DRIVE the matching
  # pane (`:help X` focuses the System Browser) or POINT the user at it.
  # Input without a leading colon is real code and falls through to `eval`
  # untouched.
  #
  # Returns `nil` for non-meta input (the overwhelmingly common path) so the
  # caller's `cond` falls through to eval; otherwise a parsed `{kind, …}`
  # tuple `handle_repl_meta/3` routes.
  defp repl_meta_command(input) do
    if String.starts_with?(input, ":") do
      # `input` starts with ":", so splitting on whitespace always yields at
      # least the command token (a bare ":" splits to `[":"]`, routed to the
      # catch-all).
      [cmd | rest] = String.split(input, ~r/\s+/, parts: 2, trim: true)
      repl_meta_dispatch(cmd, meta_arg(rest))
    else
      nil
    end
  end

  # First whitespace-delimited token of a meta-command's argument, with a
  # leading `#` stripped so `:help #Counter` and `:help Counter` agree. `nil`
  # when the command had no argument.
  defp meta_arg([]), do: nil

  defp meta_arg([arg]) do
    # `arg` is the non-empty second part of the outer `parts: 2, trim: true`
    # split, so this inner split always yields at least the first token.
    [token | _] = String.split(arg, ~r/\s+/, parts: 2, trim: true)

    case String.trim_leading(token, "#") do
      "" -> nil
      stripped -> stripped
    end
  end

  defp repl_meta_dispatch(cmd, arg) when cmd in [":help", ":h", ":?"], do: {:help, arg}

  defp repl_meta_dispatch(cmd, _) when cmd in [":bindings", ":b"],
    do:
      {:point,
       "Bindings are listed live in the Bindings pane on the right — click one to inspect it."}

  defp repl_meta_dispatch(cmd, _) when cmd in [":changes", ":dirty"],
    do: {:point, "Pending changes are shown in the Changes tab of this dock."}

  defp repl_meta_dispatch(":flush", _),
    do: {:point, "Use the Flush control in the Changes tab to write pending changes to disk."}

  defp repl_meta_dispatch(cmd, _) when cmd in [":sync", ":s"],
    do:
      {:point,
       "The IDE tracks the live image as you work, so there is no manual sync step — project files from `beamtalk.toml` load when you connect."}

  defp repl_meta_dispatch(cmd, _) when cmd in [":test", ":t"],
    do: {:tab, "tests", "Opened the Tests pane in this dock — Run all, or run a single class. ◂"}

  defp repl_meta_dispatch(":clear", _),
    do:
      {:point,
       "Session bindings clear with the workspace. To clear them now, evaluate: Session current clear"}

  defp repl_meta_dispatch(cmd, _) when cmd in [":show-codegen", ":sc"],
    do:
      {:point,
       "Generated-code inspection (:show-codegen) is CLI-only for now — run it from `beamtalk repl`."}

  defp repl_meta_dispatch(cmd, _) when cmd in [":exit", ":quit", ":q"],
    do:
      {:point,
       "Close the browser tab to disconnect — there is no REPL process to exit in the IDE."}

  defp repl_meta_dispatch(cmd, _), do: {:unknown, cmd}

  # Route a parsed meta-command. `:help X` drives the System Browser;
  # everything else appends an informational scrollback entry (a third
  # `kind`, `:info`, the template styles muted rather than as an error).
  defp handle_repl_meta(socket, {:help, nil}, expr),
    do: repl_append_info(socket, expr, repl_help_text())

  defp handle_repl_meta(socket, {:help, class}, expr),
    do: repl_focus_class(socket, expr, class)

  defp handle_repl_meta(socket, {:point, message}, expr),
    do: repl_append_info(socket, expr, message)

  # Route a meta-command to a dock tab (BT-2557: `:test` → Tests pane).
  # Switches the dock to `tab`, lazily loads the Tests catalogue when that is
  # the target, and appends a confirming info entry — the GUI equivalent of
  # the CLI command.
  defp handle_repl_meta(socket, {:tab, tab, message}, expr) do
    socket
    |> assign(dock_tab: tab)
    |> then(fn s -> if tab == "tests", do: ensure_test_classes(s), else: s end)
    |> repl_append_info(expr, message)
  end

  defp handle_repl_meta(socket, {:unknown, cmd}, expr) do
    repl_append_info(
      socket,
      expr,
      "Unknown command #{cmd}. This is the IDE REPL — type :help for what's available, " <>
        ":help <Class> to open a class in the System Browser, or just evaluate an expression."
    )
  end

  # Focus the System Browser on `class` (the GUI equivalent of the CLI's
  # `:help Class` → `Beamtalk help: Class`). We validate against the live
  # symbol index first so an unknown name gives a clean message instead of
  # pointing the browser at a class that doesn't exist.
  defp repl_focus_class(socket, expr, class) do
    if class in browser_class_names(socket) do
      socket
      |> SystemBrowser.open_class(class)
      |> repl_append_info(expr, "Opened #{class} in the System Browser ◂")
    else
      # "No class named X" is feedback about a meta-command, not a
      # code-evaluation failure, so it uses the muted `:info` styling (like
      # an unknown `:cmd`) rather than the red error arrow reserved for eval
      # errors.
      repl_append_info(
        socket,
        expr,
        "No class named #{class}. Browse classes in the System Browser, or search with the omni bar (top)."
      )
    end
  end

  # The class names known to the live image, from the same symbol index the
  # omni search uses, as a MapSet so the `class in browser_class_names(socket)`
  # membership check in `repl_focus_class/3` is O(1). An empty index
  # (dispatch failure / RBAC denial) just means every `:help X` reports "no
  # such class" rather than crashing.
  defp browser_class_names(socket) do
    socket
    |> WorkspaceLive.symbol_rows()
    |> Enum.filter(&(&1.kind == "class"))
    |> MapSet.new(& &1.class)
  end

  # `:help` with no argument: a short tour of where the CLI REPL's commands
  # live in the IDE, so a muscle-memory `:h` lands somewhere useful instead
  # of erroring.
  defp repl_help_text do
    """
    IDE REPL — evaluate any expression (Enter runs it, ↑/↓ recall history).
    :help / :h / :?        show this help
    :help <Class>          open a class in the System Browser (left)
    :bindings / :b         → Bindings pane (right)
    :changes / :dirty      → Changes tab (this dock)
    :flush                 → Flush control (Changes tab)
    :sync / :s             tracks the live image (loads beamtalk.toml on connect)
    :clear                 evaluates `Session current clear`
    :show-codegen / :sc    CLI-only — run from `beamtalk repl`
    :test / :t             → Tests pane (this dock) — run all or a class
    :exit / :quit / :q     close the browser tab to disconnect
    Inspect results with the Inspect button; browse classes/methods on the left.\
    """
  end

  # After a successful eval, if the expression was a `Beamtalk help: Class`
  # send (the CLI's `:help` desugaring, and a natural thing to type
  # directly), focus the System Browser on that class too — the help text
  # stays in the scrollback AND the browser navigates to the subject.
  # Non-help evals pass through untouched.
  #
  # Unlike `repl_focus_class/3`, this skips the `browser_class_names/1`
  # validation and calls `SystemBrowser.open_class/2` directly: the `eval`
  # already succeeded, which proves the class exists in the runtime, so a
  # symbol-index lookup would be redundant (and would falsely reject a class
  # defined moments earlier if the index is briefly stale). `load_protocols`
  # handles an empty result gracefully regardless.
  defp repl_help_followup(socket, expr) do
    case Regex.run(~r/^\s*Beamtalk\s+help:\s+#?([A-Z]\w*)/, expr) do
      [_, class] -> SystemBrowser.open_class(socket, class)
      _ -> socket
    end
  end

  # Append an informational meta-command response (`kind: :info`): no live
  # term, so no Inspect affordance and nothing stashed. Mirrors
  # `repl_append_error/3`'s bookkeeping (seq bump + term-map eviction in
  # lockstep with the scrollback cap).
  defp repl_append_info(socket, request, message) do
    seq = socket.assigns.repl_seq + 1
    id = repl_entry_id(seq)

    entry = %{
      id: id,
      request: request,
      kind: :info,
      response: message,
      inspectable: false,
      long: repl_long?(message)
    }

    socket
    |> assign(:repl_seq, seq)
    |> update(:repl_terms, &repl_evict(&1, seq))
    |> stream_insert(:repl, entry, limit: -@repl_scrollback_limit)
    |> repl_scroll_to_bottom()
  end

  # ── Test-runner pane hand-off (BT-2557) ─────────────────────────────────────
  #
  # `dock_tab`'s "tests" clause and the `:test` REPL meta-command both lazily
  # load the test catalogue on first open — the Tests pane's own events
  # (`tests_refresh`/`run_tests`/`load_tests`/`run_test_class`/
  # `open_test_method`) are NOT part of this extraction (BT-3295's "Events to
  # move" list); they belong to `BtAttachWeb.Live.TestRunner` (extracted
  # BT-3298), so the actual discovery RPC (`TestRunner.discover_test_classes/1`)
  # lives there too — this is a thin call-through, not a reimplementation.

  # Load the test catalogue once (lazy first open of the Tests tab).
  # Re-opening the tab keeps the already-loaded list — use `tests_refresh` to
  # re-discover.
  defp ensure_test_classes(socket) do
    if is_nil(socket.assigns.test_classes),
      do: TestRunner.discover_test_classes(socket),
      else: socket
  end

  # ── Git panel (ADR 0082 Amendment 1, BT-2586) ────────────────────────────────

  # Run a mutating git op through the Facade (Owner-gated) and refresh the
  # panel so the new status/log is reflected. An :unauthorized/error result
  # surfaces in the panel rather than crashing it.
  # A content-mutating git op (revert / `git restore -- <path>`) needs more
  # than a git-panel refresh: it changes the on-disk working tree, so it must
  # (1) not silently clobber unflushed in-memory edits for that path, and (2)
  # reload the affected module(s) into the live image so image == disk and
  # open windows reflect the reverted code (BT-2598). Routed through
  # `git_revert_event/2`.
  defp git_mutate_event(socket, :git_revert_file, %{path: path} = params) do
    if path_has_pending_edits?(socket, path) do
      # Decision 2: do not revert under unflushed in-memory ChangeLog edits —
      # live work would be silently lost. Block and tell the user to flush or
      # discard the pending entry first. No git call is made.
      assign(socket, git_error: pending_revert_warning(path))
    else
      git_revert_event(socket, params)
    end
  end

  # Stage / unstage / commit do not change working-tree *content* (the file
  # the user is editing), so they keep the original behaviour: dispatch and
  # refresh the git panel only.
  defp git_mutate_event(socket, op, params) do
    case Facade.dispatch(op, params, RequestContext.build(socket)) do
      {:ok, _} -> assign_git(socket)
      {:error, reason} -> assign(socket, git_error: FacadeError.render(reason))
    end
  end

  # Revert the working-tree change, then reload the reverted file into the
  # live image and refresh every source-dependent surface (browser,
  # ChangeLog, open editor tabs) plus the git panel. The reload's
  # `ClassLoaded` push *also* drives the refresh for other connected
  # sessions; reloading + refreshing here makes the acting session update
  # synchronously rather than waiting on its own push.
  defp git_revert_event(socket, %{path: path} = params) do
    case Facade.dispatch(:git_revert_file, params, RequestContext.build(socket)) do
      {:ok, _} ->
        {reloaded, reload_note} = reload_reverted_path(socket, path)

        socket
        # The reload reconciled the image to the reverted (disk) body, so the
        # reloaded class' open tabs are no longer divergent — clear their
        # `unflushed` badge before the re-read (which would otherwise
        # preserve the already-set divergence, mirroring
        # `clear_disk_differs/2` after a flush).
        |> assign(:tabs, MethodEditor.clear_disk_differs(socket.assigns.tabs, reloaded))
        # BT-2655: re-read the reverted `:def` tabs' *editable* definition
        # buffer so the visible editor shows the reverted header without a
        # close/reopen. `MethodEditor.refresh_after_source_change/1` below
        # re-reads `:method` tab bodies on its own (and bumps `editor_rev` to
        # remount the editor), but it deliberately leaves a `:def` tab's
        # editable definition buffer untouched (a generic push must not
        # clobber a concurrent edit). A revert is the safe exception: it is
        # blocked for this path under pending edits
        # (`path_has_pending_edits?/2`, BT-2598 d2), so overwriting the
        # editable buffer for exactly the reloaded `:def` set is both safe
        # and expected. Method tabs are intentionally left to the refresh
        # below to avoid a redundant second `browse_method_source`
        # round-trip per tab.
        |> MethodEditor.reload_reverted_def_buffers(reloaded)
        |> MethodEditor.refresh_after_source_change()
        |> assign_git()
        # A clean revert whose reload failed surfaces its note in the shared
        # status area (the same slot revert / new-class outcomes use), NOT
        # `git_error` — `assign_git/1` and the async git load both clear
        # `git_error`, so the note would not survive there. The working tree
        # was reverted, but the image may not have reloaded.
        |> maybe_status_error(reload_note)

      {:error, reason} ->
        assign(socket, git_error: FacadeError.render(reason))
    end
  end

  defp maybe_status_error(socket, nil), do: socket
  defp maybe_status_error(socket, note), do: WorkspaceLive.status_error(socket, note)

  # Reload the reverted file from disk into the live image (image == disk,
  # BT-2585), returning the `{class, selector}` set of the reloaded class'
  # currently-open `:method` tabs (so their `unflushed` badge can be
  # cleared) and a reload-failure note (or nil). A reload failure (a deleted
  # file, a file with a compile error at HEAD) is non-fatal: the working
  # tree was still reverted, and the subsequent refresh re-reads what the
  # image can serve; the note is surfaced afterwards.
  defp reload_reverted_path(socket, path) do
    case Facade.dispatch(:reload, %{path: path}, RequestContext.build(socket)) do
      {:ok, class_names} when is_list(class_names) ->
        {reloaded_tab_keys(socket.assigns.tabs, class_names), nil}

      {:error, reason} ->
        {MapSet.new(), "Reverted #{path}, but reload failed: #{FacadeError.render(reason)}"}
    end
  end

  # The disk keys of open `:method` and `:def` tabs whose class is among the
  # just-reloaded class names — the tabs whose `unflushed` badge a revert
  # should clear (their image now matches the reverted disk body). Method
  # tabs key on `{class, selector}`; a `:def` tab keys on `{class, :def}` so
  # a revert that changed a class header (state, superclass) also clears the
  # open def tab's badge without needing a re-open (BT-2600).
  defp reloaded_tab_keys(tabs, class_names) do
    reloaded = MapSet.new(class_names)

    for tab <- tabs,
        key = MethodEditor.tab_disk_key(tab),
        key != nil,
        MapSet.member?(reloaded, elem(key, 0)),
        into: MapSet.new(),
        do: key
  end

  # Whether the file `path` git is about to revert has unflushed in-memory
  # ChangeLog edits (BT-2598 decision 2). The cockpit `changes` rows carry
  # only `class`/`selector`, so map each pending class to its source file
  # via the browser class list (which carries `source_file`) and compare
  # against `path`. Path comparison is by trailing-segment match so a
  # project-relative `path` (`src/Foo.bt`) matches an absolute or
  # differently-rooted `source_file`.
  defp path_has_pending_edits?(socket, path) do
    pending_classes =
      for %{class: class} <- socket.assigns[:changes] || [],
          is_binary(class),
          into: MapSet.new(),
          do: class

    if MapSet.size(pending_classes) == 0 do
      false
    else
      Enum.any?(socket.assigns[:browser_classes] || [], fn row ->
        is_map(row) and MapSet.member?(pending_classes, row["name"]) and
          paths_match?(row["source_file"], path)
      end)
    end
  end

  # Two paths refer to the same file when one is a trailing-segment suffix of
  # the other (so `src/Foo.bt` matches `/abs/project/src/Foo.bt`). Both
  # nil/non-binary → no match.
  defp paths_match?(a, b) when is_binary(a) and is_binary(b) do
    a == b or String.ends_with?(a, "/" <> b) or String.ends_with?(b, "/" <> a)
  end

  defp paths_match?(_a, _b), do: false

  # The git-panel message shown when a revert is blocked because the file has
  # unflushed in-memory edits (BT-2598 decision 2) — names the path and the
  # two ways forward (flush to keep the work on disk, or discard the pending
  # change in the Changes pane first).
  defp pending_revert_warning(path) do
    "Cannot revert #{path}: it has unflushed in-memory edits. " <>
      "Flush (Save All to Disk) to keep them, or discard the pending change in the Changes pane first."
  end

  # BT-2590 (S1): read the git status + recent log off-socket so a slow
  # `git`/large-history repo never blocks the LiveView call — blocking tab
  # switches, saves, and clicks. We therefore run the two reads off-socket in
  # a single `start_async` task: the socket stays responsive while git runs,
  # the panel shows its "Loading git status…" placeholder, and the results
  # land in `handle_async(:git_load, …)`. A rapid second Refresh first
  # `cancel_async`-es the in-flight load (killing the LiveView-side Task so
  # only the latest load can update the panel — the workspace-side RPC may
  # still complete, but its response is discarded) and then starts the fresh
  # one — only the latest load wins the panel, and the LiveView is never
  # blocked.
  #
  # A workspace that is unreachable, not a git repo, or missing `git`
  # renders an error rather than crashing the pane — the graceful-
  # degradation requirement. We clear any stale per-file diff and reset to
  # the loading state on each refresh. Public: `WorkspaceLive.maybe_refresh_git/1`
  # (used by the not-yet-extracted save/flush paths) calls it directly.
  def assign_git(socket) do
    ctx = RequestContext.build(socket)

    socket
    |> assign(git_diff_path: nil, git_diff: nil, git_status: nil, git_log: [], git_error: nil)
    |> cancel_async(:git_load, :cancelled)
    |> start_async(:git_load, fn ->
      # Runs in a Task off the LiveView process — never touch `socket` here,
      # only the captured `ctx`. Both reads are gathered so the panel
      # updates atomically.
      {Facade.dispatch(:git_status, %{}, ctx), Facade.dispatch(:git_log, %{count: 20}, ctx)}
    end)
  end

  # Apply a completed git status read to the socket. Pure (no dispatch);
  # shared by `handle_async/3` so the async result path and any future sync
  # caller agree. Kept total — an unexpected shape (an off-vocabulary facade
  # reply, a malformed status) degrades to a panel error rather than
  # crashing the LiveView.
  defp apply_git_status(socket, {:ok, status}) when is_map(status),
    do: assign(socket, git_status: status, git_error: nil)

  defp apply_git_status(socket, {:error, reason}),
    do: assign(socket, git_status: nil, git_error: FacadeError.render(reason))

  defp apply_git_status(socket, _other),
    do: assign(socket, git_status: nil, git_error: FacadeError.render(:unexpected_git_status))

  # Apply a completed git log read.
  defp apply_git_log(socket, {:ok, commits}) when is_list(commits),
    do: assign(socket, git_log: commits)

  defp apply_git_log(socket, {:error, reason}),
    do: log_failed(socket, FacadeError.render(reason))

  defp apply_git_log(socket, _other),
    do: log_failed(socket, FacadeError.render(:unexpected_git_log))

  # A git-log read failed. Clear the list, and surface the error only if the
  # status read hasn't already reported one — when both fail together the
  # status pane already shows the degraded state, but a fast status beside
  # an independently-failed log (e.g. a large-history timeout) would
  # otherwise leave a valid branch next to a mysteriously empty commit list
  # with no explanation.
  defp log_failed(socket, error) do
    socket = assign(socket, git_log: [])

    if socket.assigns.git_error,
      do: socket,
      else: assign(socket, git_error: error)
  end

  # ── Changes pane (ADR 0082) ──────────────────────────────────────────────────

  # `present_side_param/1` normalises a client-supplied `entry-side` payload
  # value (`""` from an absent hidden field) to `nil` so `revert`/
  # `toggle_change_diff` agree on what "no side" means.
  defp present_side_param(side) when is_binary(side) and side != "", do: side
  defp present_side_param(_), do: nil

  # Revert one pending method patch (BT-2293). On success the prior body is
  # re-installed (a fresh durable entry) and the Changes pane refreshes; a
  # non-revertable entry (new-class, class-side, no prior body) renders the
  # structured error the workspace returns. `side` (ADR 0112, BT-3187)
  # disambiguates a same-selector instance-side entry from a class-side one.
  defp revert_change(socket, class, selector, side) do
    case Facade.dispatch(
           :revert,
           %{class: class, selector: selector, side: side},
           RequestContext.build(socket)
         ) do
      {:ok, reverted_class} ->
        socket
        |> assign(
          save_result: "Reverted #{selector} on #{reverted_class}",
          save_error: nil,
          flush_result: nil,
          flush_error: nil
        )
        |> WorkspaceLive.assign_changes()

      {:error, reason} ->
        WorkspaceLive.status_error(socket, FacadeError.render(reason))
    end
  end

  # Drive the write-surface flush and render its summary, then refresh
  # changes so the (now-flushed) entries drop out of the active view.
  #
  # The flush reconciles every pending live `>>` patch with its on-disk
  # body, so an open `:method` tab whose patch was just written is no longer
  # divergent — clear its `unflushed` (`disk_differs`) breadcrumb badge
  # (BT-2545). We diff the pending method set captured *before* the flush
  # against the set still pending *after* (`WorkspaceLive.assign_changes/1`
  # already refreshed it): the difference is exactly what this flush wrote,
  # so a conflicted / non-flushable method keeps its badge and a method
  # untouched by this flush is never cleared.
  defp flush_changes(socket) do
    was_pending = WorkspaceLive.pending_method_keys(socket.assigns.changes)

    case Facade.dispatch(:flush, %{}, RequestContext.build(socket)) do
      {:ok, summary} ->
        socket
        |> assign(flush_result: Workspace.format_flush_summary(summary), flush_error: nil)
        |> WorkspaceLive.assign_changes()
        |> clear_flushed_badges(was_pending)
        # BT-2586: a flush writes pending edits to disk, so the post-flush
        # git panel must reflect them immediately (the pre→post-flush
        # handoff).
        |> WorkspaceLive.maybe_refresh_git()

      {:error, reason} ->
        assign(socket, flush_result: nil, flush_error: FacadeError.render(reason))
    end
  end

  # "Delete file" / "Apply rename" (ADR 0113 Phase 4 BT-3210, extended by ADR
  # 0114 Phase 5 BT-3277) — the scoped Tier-2 flush for one `remove-class`/
  # `rename-class`/`rename-method` row, submitted as `Workspace flush:
  # #Class confirmDestructive: true` through the generic `evaluate` op (no
  # dedicated workspace-side op, matching `remove_class`/`remove_method` and
  # mirroring the REPL's `:flush-destructive #Class` / MCP `flush`'s scoped
  # `confirm_destructive: true` form). All three kinds join the same Tier 2
  # (ADR 0114 "Flush" reuses ADR 0113's tier verbatim), so one handler drives
  # all three rows; only the confirmation prompt and the resulting status
  # message differ by `kind`.
  #
  # Scoped by *Symbol* (`#Class`), not the bare class name: a `remove-class`
  # row's class name is already unbound by the time this fires
  # (`removeFromSystem` ran first) — a bare identifier would fail to
  # *evaluate* before the `flush:` send ever runs. `beamtalk_workspace_flush`'s
  # filter normalisation matches a Symbol against the ChangeLog entry's
  # recorded `class` field by name, needing no live class to resolve, so the
  # same Symbol form works uniformly for a `rename-class`/`rename-method` row
  # too (both still-bound, but simpler to share one code path than
  # special-case which kind can take a bare identifier).
  #
  # `class` rides a `phx-value-*` attribute, so — unlike `remove_class`'s
  # `class` (read from server-tracked active-tab state) — it is
  # client-controlled input reaching a raw, textually-interpolated Beamtalk
  # expression: validated against `ClassModals.valid_class_name?/1`, the
  # same bare-PascalCase-identifier shape the New Class modal enforces, so a
  # crafted event cannot inject arbitrary source into the `evaluate` call.
  defp flush_destructive(socket, class, kind) do
    if ClassModals.valid_class_name?(class) do
      expr = "Workspace flush: ##{class} confirmDestructive: true"
      pid = socket.assigns[:session_pid]

      if not is_pid(pid) do
        WorkspaceLive.status_error(socket, "not attached to workspace")
      else
        flush_destructive_eval(socket, class, kind, expr, pid)
      end
    else
      WorkspaceLive.status_error(socket, "Invalid class name.")
    end
  end

  # The past-tense status line for a completed Tier-2 flush, by `kind` — an
  # unrecognised future kind falls back to a generic phrase rather than
  # crashing, matching `WorkspaceLive`'s own `change_kind_label/1` fallback.
  defp rename_flush_message("rename-class", class), do: "Flushed the pending rename to #{class}"

  defp rename_flush_message("rename-method", class),
    do: "Flushed the pending method rename on #{class}"

  defp rename_flush_message(_kind, class), do: "Flushed the pending removal for #{class}"

  defp flush_destructive_eval(socket, class, kind, expr, pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, RequestContext.build(socket)) do
      {:ok, _term, _output, _warnings} ->
        socket
        |> assign(
          save_result: nil,
          save_error: nil,
          flush_result: rename_flush_message(kind, class),
          flush_error: nil
        )
        |> WorkspaceLive.assign_changes()
        # BT-2586: a successful destructive flush can delete a tracked file,
        # so the git panel must reflect it immediately, same as an ordinary
        # "Save All to Disk" flush.
        |> WorkspaceLive.maybe_refresh_git()

      {:error, reason, _output, _warnings} ->
        WorkspaceLive.status_error(socket, Workspace.render_error(reason))

      {:error, reason} ->
        WorkspaceLive.status_error(socket, FacadeError.render(reason))
    end
  end

  # After a flush, clear the `unflushed` badge on the `:method` tabs this
  # flush wrote to disk (BT-2545), scoped by `WorkspaceLive.flushed_method_keys/3`
  # so conflicts / skips keep their badge and methods untouched by this
  # flush are never cleared.
  defp clear_flushed_badges(socket, was_pending) do
    flushed =
      WorkspaceLive.flushed_method_keys(
        was_pending,
        socket.assigns.changes,
        socket.assigns[:changes_error]
      )

    if MapSet.size(flushed) == 0 do
      socket
    else
      assign(socket, :tabs, MethodEditor.clear_disk_differs(socket.assigns.tabs, flushed))
    end
  end
end

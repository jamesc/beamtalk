# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.Dock.GitChanges do
  @moduledoc """
  The Dock's Git panel and Changes pane: git status/log loading, revert and
  stage actions, and the ChangeLog revert/flush actions. Extracted from
  `BtAttachWeb.Live.Dock` (BT-5112) with no behaviour change; `Dock`'s
  `handle_event/3` clauses call into it.
  """

  use BtAttachWeb, :html

  import Phoenix.LiveView, only: [cancel_async: 3, start_async: 3]

  alias BtAttach.Facade
  alias BtAttach.Workspace
  alias BtAttachWeb.Live.ClassModals
  alias BtAttachWeb.Live.FacadeError
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.RequestContext
  alias BtAttachWeb.WorkspaceLive

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
  @doc false
  def git_mutate_event(socket, :git_revert_file, %{path: path} = params) do
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
  def git_mutate_event(socket, op, params) do
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
  @doc false
  def git_revert_event(socket, %{path: path} = params) do
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
  @doc false
  def apply_git_status(socket, {:ok, status}) when is_map(status),
    do: assign(socket, git_status: status, git_error: nil)

  def apply_git_status(socket, {:error, reason}),
    do: assign(socket, git_status: nil, git_error: FacadeError.render(reason))

  def apply_git_status(socket, _other),
    do: assign(socket, git_status: nil, git_error: FacadeError.render(:unexpected_git_status))

  # Apply a completed git log read.
  @doc false
  def apply_git_log(socket, {:ok, commits}) when is_list(commits),
    do: assign(socket, git_log: commits)

  def apply_git_log(socket, {:error, reason}),
    do: log_failed(socket, FacadeError.render(reason))

  def apply_git_log(socket, _other),
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
  @doc false
  def present_side_param(side) when is_binary(side) and side != "", do: side
  def present_side_param(_), do: nil

  # Revert one pending method patch (BT-2293). On success the prior body is
  # re-installed (a fresh durable entry) and the Changes pane refreshes; a
  # non-revertable entry (new-class, class-side, no prior body) renders the
  # structured error the workspace returns. `side` (ADR 0112, BT-3187)
  # disambiguates a same-selector instance-side entry from a class-side one.
  @doc false
  def revert_change(socket, class, selector, side) do
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
  @doc false
  def flush_changes(socket) do
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
  # `rename-class`/`rename-method` row: the `flush` operation with `class` and
  # `confirm_destructive: true` (BT-5204), the same operation and confirmation
  # rule as the MCP `flush` tool's scoped `confirm_destructive: true` form and
  # the REPL's `:flush-destructive #Class`. All three kinds join the same Tier 2
  # (ADR 0114 "Flush" reuses ADR 0113's tier verbatim), so one handler drives
  # all three rows; only the confirmation prompt and the resulting status
  # message differ by `kind`.
  #
  # Scoped by class *name*, not a live class: a `remove-class` row's class is
  # already unbound by the time this fires (`removeFromSystem` ran first), and
  # `beamtalk_workspace_flush`'s filter matches the name against the ChangeLog
  # entry's recorded `class` field, so the same scope works uniformly for a
  # `rename-class`/`rename-method` row too.
  #
  # `class` rides a `phx-value-*` attribute, so it is client-controlled input:
  # validated against `ClassModals.valid_class_name?/1`, the same bare-PascalCase-
  # identifier shape the New Class modal enforces. It travels as an operation
  # param, never spliced into source text, so this is defence in depth.
  @doc false
  def flush_destructive(socket, class, kind) do
    if ClassModals.valid_class_name?(class) do
      pid = socket.assigns[:session_pid]

      if not is_pid(pid) do
        WorkspaceLive.status_error(socket, "not attached to workspace")
      else
        flush_destructive_dispatch(socket, class, kind, pid)
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

  defp flush_destructive_dispatch(socket, class, kind, pid) do
    BtAttachWeb.Live.Dock.dispatch_async(
      socket,
      pid,
      :flush,
      %{class: class, confirm_destructive: true},
      fn socket, result -> destructive_flush_done(socket, result, class, kind) end,
      &flush_destructive_failed(&1, class, &2),
      key: flush_destructive_key(class, kind)
    )
  end

  # Fold the destructive flush's result into the socket. A flush that returned
  # conflicts did not write those files, so the conflicts are shown, not the
  # success message (BT-5206).
  @doc false
  def destructive_flush_done(socket, {:ok, summary}, class, kind) do
    conflicts = Workspace.format_flush_conflicts(summary)

    socket
    |> assign(
      save_result: nil,
      save_error: nil,
      flush_result: if(conflicts, do: nil, else: rename_flush_message(kind, class)),
      flush_error: conflicts
    )
    |> WorkspaceLive.assign_changes()
    # BT-2586: a successful destructive flush can delete a tracked file, so the
    # git panel must reflect it immediately, same as an ordinary "Save All to
    # Disk" flush.
    |> WorkspaceLive.maybe_refresh_git()
  end

  def destructive_flush_done(socket, {:error, reason}, _class, _kind),
    do: WorkspaceLive.status_error(socket, FacadeError.render(reason))

  # BT-5189: the pending key of a destructive flush of `class`'s `kind` entry —
  # its Changes-pane button stays disabled while the flush is queued or running.
  @doc false
  def flush_destructive_key(class, kind), do: {:flush_destructive, class, kind}

  # The flush never ran (refused, dropped from the queue) or its task died:
  # report it in the Changes pane's flush status, where its success would land.
  defp flush_destructive_failed(socket, class, message) do
    assign(socket,
      save_result: nil,
      save_error: nil,
      flush_result: nil,
      flush_error: "Flushing #{class} did not complete: #{message}"
    )
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

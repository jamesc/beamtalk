# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.SessionLifecycle do
  @moduledoc """
  Workspace session lifecycle for `BtAttachWeb.WorkspaceLive`, extracted from it
  (BT-5092): the connected-mount attach path, resume-or-start of the per-tab
  session through `BtAttach.SessionRegistry`, `bind_session/3` (the cockpit's
  initial socket assigns), the monitored-session reconnect and the off-socket
  mount-time reads. Behaviour is unchanged; `WorkspaceLive.mount/3` and its
  `workspace_reconnect` event call `attach/1` and `reconnect_workspace/1`.
  """

  use BtAttachWeb, :html

  import Phoenix.LiveView, only: [start_async: 3, stream: 4]
  require Logger

  alias BtAttach.Facade
  alias BtAttach.SessionRegistry
  alias BtAttach.Workspace
  alias BtAttachWeb.Live.Inspector
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.RequestContext
  alias BtAttachWeb.Live.SystemBrowser

  defp ws_client, do: Application.get_env(:bt_attach, :workspace_client, Workspace)

  @doc false
  def attach(%{assigns: %{token: token}} = socket) do
    socket =
      case ws_client().connect() do
        :ok ->
          # Resume the tab's existing session if the registry still holds a live
          # one (reconnect within the grace window); otherwise start fresh.
          case resume_or_start(token, session_meta(socket)) do
            {:ok, session_id, pid, origin} ->
              socket
              |> bind_session(session_id, pid)
              |> Inspector.restore_windows(token, origin)
              |> MethodEditor.restore_doc(token, origin)

            {:error, reason} ->
              assign(socket,
                connected: false,
                error: "session start failed: #{inspect(reason)}"
              )
          end

        {:error, reason} ->
          assign(socket, connected: false, error: "attach failed: #{inspect(reason)}")
      end

    {:ok, socket}
  end

  # Resume the tab's session if the registry has a *live* one for this token,
  # else start a brand-new workspace-supervised session and register it.
  #
  # A registry hit is only trusted if the remote session pid is still reachable:
  # the workspace could have died/restarted between the disconnect and this
  # reconnect, leaving a stale entry. We probe with a cheap `is_process_alive/1`
  # on the workspace node (`Workspace.session_alive?/1`) and fall back to a fresh
  # session on a dead pid, so resume never claims success on a session that is
  # already gone.
  defp resume_or_start(token, meta) do
    case token && SessionRegistry.checkout(token) do
      {:resumed, session_id, pid} ->
        if Workspace.session_alive?(pid) and attach_resumed(pid) do
          {:ok, session_id, pid, :resumed}
        else
          # Stale entry (workspace restarted, or the session idled out while the
          # tab was away): discard it and start fresh.
          SessionRegistry.discard(token)
          start_fresh(token, meta)
        end

      _miss ->
        start_fresh(token, meta)
    end
  end

  # BT-5084: resuming attaches this LiveView to the session. `attach` answers
  # `gone` when the session stopped (a detached session whose idle period ran out)
  # between the liveness probe and now; the caller then starts a fresh one instead
  # of binding to a dead pid. Any other outcome (including an older workspace
  # without the call) keeps the resume, as before; `bind_session/3` attaches again
  # and logs a failure.
  defp attach_resumed(pid) do
    ws_client().attach_session(pid, self()) != {:error, :gone}
  end

  # Origin/debug metadata for a freshly-created workspace session: always the
  # `liveview` kind and Phoenix `node`, plus the authenticated `user` when one is
  # assigned (a plain binary, or extracted from a user struct's id/username).
  # Surfaced by `Workspace sessions` / `Session info` on the workspace side.
  defp session_meta(socket) do
    base = %{kind: "liveview", node: node(), connected_at: System.system_time(:microsecond)}

    case socket.assigns[:current_user] do
      user when is_binary(user) -> Map.put(base, :user, user)
      %{username: u} when is_binary(u) -> Map.put(base, :user, u)
      %{id: id} when not is_nil(id) -> Map.put(base, :user, to_string(id))
      _ -> base
    end
  end

  defp start_fresh(token, meta) do
    session_id = "phoenix-#{System.unique_integer([:positive])}"

    case ws_client().start_session(session_id, meta) do
      pid when is_pid(pid) ->
        # Register before binding so a crash mid-bind can't leak: the registry
        # owns the close, keyed by the tab token (a nil token simply skips
        # registration — that session is non-resumable and closed in terminate/2).
        SessionRegistry.register(token, session_id, pid)
        {:ok, session_id, pid, :fresh}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Subscribe THIS LiveView pid (location-transparent over dist) to the Transcript
  # AND bindings streams through the BT-2399 facade — no direct gen_server cast.
  # The facade's cast returns `:ok`; any non-ok reply (`{:badrpc, _}`) means a
  # stream is NOT live, so we must not render the pane as connected and claim a
  # working stream. Pattern-match both subscribe results (Wave-1 review rule) and,
  # on failure, release the session through the registry so it can't leak.
  #
  # Shared by the fresh-start and resume paths: on resume the same subscribe +
  # bindings re-read re-establishes the live streams for the new LiveView pid
  # (the old pid's subscriptions died with it), so the resumed tab gets its
  # Transcript and bindings flowing again with its accumulated state intact.
  defp bind_session(socket, session_id, pid) do
    with :ok <-
           Facade.dispatch(:subscribe_transcript, %{pid: self()}, RequestContext.build(socket)),
         :ok <- Facade.dispatch(:subscribe_bindings, %{pid: self()}, RequestContext.build(socket)) do
      # BT-2598: subscribe to the class-lifecycle push stream so any source change
      # — a git revert's disk→image reload, another session's flush, an external
      # edit reloaded into the image — pushes a `ClassLoaded`/`ClassRemoved`
      # refresh trigger to this LiveView. Best-effort: unlike the transcript /
      # bindings streams the cockpit can still function without it (open windows
      # simply fall back to the re-activation re-read), so a subscribe failure is
      # logged but does not tear down the session.
      subscribe_classes_best_effort(socket)
      # ADR 0105 Phase 1 (BT-2779): subscribe to the reload-check push stream
      # so a `ReloadCheckCompleted` announcement (a re-check ran, or a
      # caller's stale findings were cleared) updates the reload-findings
      # panel live. Best-effort, same rationale as `subscribe_classes`.
      subscribe_reload_check_best_effort(socket)

      attach_session_best_effort(pid)

      socket
      |> assign(:connected, true)
      |> assign(:workspace_lost, nil)
      |> assign(:reconnect_error, nil)
      |> monitor_session(pid)
      |> assign(:node, Workspace.node_name())
      # ADR 0105 Phase 1 (BT-2779): the initial live snapshot of
      # reload-induced findings. A plain synchronous dispatch (unlike
      # `changes`/`browser_classes`, which fold through the async
      # `start_mount_load` task) — the read is a single in-memory
      # `gen_server:call` with no git/file I/O, so blocking `bind_session`
      # briefly for it is the same trade-off `subscribe_transcript`/
      # `subscribe_bindings` already make just above.
      |> assign(:reload_findings, initial_reload_findings(socket))
      |> assign(:session_id, session_id)
      |> assign(:session_pid, pid)
      |> assign(:result, nil)
      # `eval_seq` re-keys the transient `.eval-status` line (BT-2542) so its
      # fade animation restarts on every eval — the status self-clears visually
      # while staying in the DOM (the value is still assertable in tests).
      |> assign(:eval_seq, 0)
      # BT-5081: true while a Workspace/REPL eval runs in its `start_async` task.
      # The tab stays interactive; the dock shows a Stop control meanwhile.
      |> assign(:eval_running, false)
      |> assign(:eval_queue, [])
      # BT-5189: the job whose task is running (so a crash or `DOWN` can tell
      # its own UI) and the keys of `Dock.eval_async/6` operations still queued
      # or running (so their submit controls stay disabled meanwhile).
      |> assign(:eval_current, nil)
      |> assign(:eval_pending, MapSet.new())
      # BT-5189: what the last Stop cancelled ("Stopped. N waiting evaluations
      # were cancelled."), shown beside the cockpit-wide Stop.
      |> assign(:eval_notice, nil)
      |> assign(:output, nil)
      |> assign(:eval_warnings, nil)
      |> assign(:error, nil)
      |> assign(:expr, "3 + 4")
      # Workspace dock active tab (BT-2490): Workspace | REPL | Transcript |
      # Changes | Tests (REPL added in BT-2543, Tests in BT-2557).
      |> assign(:dock_tab, "workspace")
      # Test-runner pane (BT-2557): `:test_classes` is the discovered catalogue
      # (`[%{"class","selectors"}]`), loaded lazily the first time the Tests tab
      # is opened (nil = not loaded yet); `:test_results` is the last run's
      # normalised result map (nil = nothing run yet); `:tests_error` carries a
      # discovery/run failure string for the pane.
      |> assign(:test_classes, nil)
      |> assign(:test_results, nil)
      |> assign(:tests_error, nil)
      # BT-3358: which op last set `:tests_error` — `:run_load` (a `run_tests`/
      # `load_tests` result) or `nil` (discovery, or nothing yet). Lets a
      # `:test_discover` fold that lands after a `:test_op` result (the tab-open
      # discovery was never cancelled and completes later — see
      # `TestRunner.apply_test_classes/3`) tell "my own stale error/success"
      # apart from "a fresher run/load error I must not clobber".
      |> assign(:tests_error_owner, nil)
      # BT-2597: a run/load is in flight on the workspace node. Set while the
      # `:test_op` async task runs so the run/load controls disable themselves —
      # `phx-disable-with` alone reverts as soon as the (now-immediate) event
      # handler re-renders, since the work moved off-socket to `start_async`.
      |> assign(:tests_running, false)
      # BT-5189: the running `:test_op` task's pid (announced by the task) and
      # the token of the run it belongs to — Stop kills its workspace worker.
      |> assign(:test_op_token, nil)
      |> assign(:test_op_pid, nil)
      # BT-2599: a transient flag for the off-socket `:test_discover` catalogue
      # discovery — true only while a partial-load re-discovery is in flight so
      # `TestRunner.handle_async(:test_discover, …)` keeps the partial-load
      # `tests_error` banner across a successful re-discovery (see
      # `TestRunner`'s `apply_test_classes/3`).
      |> assign(:tests_discover_keep_error, false)
      # REPL tab (BT-2543): a classic TUI request→response scrollback with the
      # input pinned at the bottom. `:repl` is the scrollback stream (so long
      # history never bloats the assigns/diff); `:repl_seq` mints stable entry
      # ids; `:repl_terms` holds each `→ result` term server-side so a later
      # Inspect click re-opens the live object in the Inspector (the entry in the
      # DOM is display-only). `:repl_history` is the recall ring (most-recent
      # first) with `:repl_history_pos` the ↑/↓ cursor into it (nil = at the live
      # input, not recalling).
      |> assign(:repl_seq, 0)
      |> assign(:repl_terms, %{})
      |> assign(:repl_history, [])
      |> assign(:repl_history_pos, nil)
      # Docked Inspector + floating windows (BT-2486/BT-2492/BT-2493, epic
      # BT-2482 Phase 3): drill breadcrumb, live per-object tracking (change
      # subscription, pid stats, freeze, field-flash coalescing), owner poke,
      # and the floating-window desk. `Inspector.init_assigns/0` is the single
      # source of truth for this key list + its fresh-session defaults
      # (BT-3302) — see that function's doc and `Inspector`'s `@moduledoc` for
      # what each key drives; assigning the map here (rather than hand-copying
      # its keys into a local `assign/3` pipe) is the tether back to
      # `Inspector`'s own read/write sites, so a rename that misses one side
      # fails a `mix test` run instead of only a live user's click.
      |> assign(Inspector.init_assigns())
      # BT-2600: coalesce `ClassLoaded`/`ClassRemoved` refresh bursts. A project
      # sync / `Workspace load:` reloading N files fires N consecutive pushes;
      # rather than running `refresh_after_source_change` (N source re-reads ×
      # open tabs) per push, the first push schedules one deferred
      # `:do_source_refresh` and sets this flag, collapsing the burst into a
      # single refresh — mirroring the Inspector's own `:refresh_pending`
      # object-change coalescing.
      |> assign(:source_refresh_pending, false)
      # Method editor (Wave 3): the write-surface edit/save/flush pane.
      |> assign(:edit_class, "")
      |> assign(:edit_selector, "")
      |> assign(:edit_source, "")
      # Tabbed method editor (BT-2494, epic BT-2482 Phase 2): the spike's
      # write-surface tab strip (ADR 0082). `:tabs` is the ordered open-tab list;
      # `:active_tab` is the id of the focused tab. Each tab carries its own
      # source/base/dirty so switching tabs swaps the whole edit buffer, and a
      # dirty dot per tab tracks unsaved edits (cleared on a successful compile).
      |> MethodEditor.init_tabs()
      # Method-editor selection (BT-2485, BT-2539), reported by the CmEditor hook.
      |> assign(:edit_selection, nil)
      # Workspace-editor selection (BT-2490): the dock's own CmEditor hook,
      # kept separate so the method editor's selection can't leak into an eval.
      |> assign(:ws_selection, nil)
      |> assign(:save_result, nil)
      |> assign(:save_error, nil)
      |> assign(:flush_result, nil)
      |> assign(:flush_error, nil)
      # BT-2588: one-shot flag `compile_clean/3` arms on a successful save and
      # `resync_active_tab/2` consumes — see that function's docs.
      |> assign(:save_echo_pending, false)
      # System Browser (BT-2491, epic BT-2482 Phase 2): the left-column
      # class → protocol → method navigator, driven by the BT-2488 browse ops
      # (ADR 0096) through the read-only facade. `browser_view` toggles the class
      # tree between Hierarchy (indented by superclass) and Category (grouped by
      # annotation); `browser_side` is the instance/class toggle that
      # re-populates the protocol/method list; `selected_protocol` is the
      # protocol filter (`nil` = "all"). Selecting a method opens it in the method
      # editor (`open_method_tab`); the browser highlights whatever the focused tab
      # shows. All four browse ops are `:read`, so the pane works for the Observer
      # role too.
      |> assign(:browser_view, "hierarchy")
      # BT-2557: source-origin filter for the class tree (`source_origin` field on
      # each browse row). "all" shows everything; "project" / "deps" / "stdlib"
      # narrow the tree so a project's own classes aren't buried under the stdlib.
      #
      # BT-2661: the filter defaults to "project" so the tree opens scoped to the
      # project's own classes ("show me my code"). The default can't be decided
      # here — `browser_classes` is still empty at mount (it loads asynchronously,
      # BT-2591) — so it is applied once the rows arrive (`apply_browser_classes/2`
      # → `apply_default_browser_source/2`), falling back to "all" when the workspace has
      # no project-origin classes (bare/stdlib-only) so the tree isn't empty on
      # open. `:browser_source_chosen` flips `true` the moment the user picks a
      # filter (or the default is applied once), so a later async refresh never
      # resets a deliberate choice — the chosen value always wins.
      |> assign(:browser_source, "all")
      |> assign(:browser_source_chosen, false)
      |> assign(:browser_side, "instance")
      |> assign(:selected_class, nil)
      |> assign(:selected_protocol, nil)
      |> assign(:browser_protocols, [])
      |> assign(:browser_error, nil)
      # BT-3238: the divider-grouped method view alongside the protocol view.
      # `browser_group_mode` toggles which grouping the method list renders by
      # ("protocol" | "section"); "section" is only reachable when
      # `browser_categories["has_dividers"]` is true, so a divider-free class
      # renders exactly as before (no behavior change). `editing_section` is
      # the section currently being renamed/added (`nil` | a section name
      # binary | the sentinel `:new`); `section_form_error` surfaces a failed
      # save inline.
      |> assign(:browser_categories, SystemBrowser.default_categories())
      |> assign(:browser_group_mode, "protocol")
      |> assign(:editing_section, nil)
      |> assign(:section_form_error, nil)
      # BT-2578: the read-only native backing-source pane. `nil` = collapsed;
      # otherwise a map carrying the fetched Erlang source for one class
      # (lazily loaded on the def tab's "View Erlang source" toggle).
      # Single-slot: only one class's native pane is expanded at a time — opening
      # a second replaces this value, and `native_shown?/2` scopes display to the
      # active tab's class (per-tab pane state is intentionally not kept).
      |> assign(:native_view, nil)
      # BT-2656: the left browser column is a two-mode panel — a `Classes | Native`
      # toggle at the top switches the whole body between the class tree
      # (`:classes`, the default) and the separate Native browser (`:native`), a
      # loaded package's hand-written native Erlang modules that no `native:` class
      # backs (the beamtalk-http case). Native modules used to live as a collapsed
      # in-tree section that crowded the class tree and couldn't scroll (BT-2660);
      # they now have their own scrollable, filterable surface. `browser_native_modules`
      # holds the enumerated rows. Opening a module creates a first-class read-only
      # `:native` editor TAB (see `open_native_module_tab/2`), so several can be open
      # at once alongside class/method tabs.
      |> assign(:browser_mode, :classes)
      |> assign(:browser_native_modules, [])
      # BT-2903 (ADR 0108 Phase 8): the third panel mode — every loaded
      # package's declared `type` aliases (ADR 0108 Implementation: "a new
      # sibling category alongside the existing Classes/Protocols panes"),
      # mirroring the Native browser's own separate-panel treatment rather
      # than a class-tree category bucket (aliases have no live class object
      # to hang a category flag off of). No client-side source filter, unlike
      # Native's `native_source`: a dependency's internal alias is already
      # excluded server-side at the seeding boundary (`browse-type-aliases`),
      # so every row this holds is already visible-by-construction.
      |> assign(:browser_type_aliases, [])
      # BT-3314: the read-only alias source pane, mirroring `:native_view`.
      # `nil` = collapsed; otherwise a map carrying the fetched source for one
      # alias (lazily loaded on a Type Aliases row's click-to-toggle). Same
      # single-slot design as `:native_view` — opening a second alias's pane
      # replaces this value.
      |> assign(:alias_view, nil)
      # BT-2656/BT-2661: the Native browser's own source-origin filter, mirroring the
      # class tree's `browser_source`. Defaults to "project" once the rows arrive
      # (`apply_default_native_source/1`), falling back to "all" when there are no
      # project-origin native modules so the list isn't empty on open. `:native_source_chosen`
      # flips `true` the moment the user picks a filter (or the default is applied
      # once), so a later refresh never resets a deliberate choice.
      |> assign(:native_source, "all")
      |> assign(:native_source_chosen, false)
      # New Class modal (BT-2293, BT-2645): the System Browser's owner-only
      # create-a-class wizard, closed by default (it opens from the browser head's
      # ＋ button). The owner types a plain class name + picks a superclass
      # (default `Object`); the `<Superclass> subclass: <Name>` definition is
      # synthesized server-side. `new_class_error` carries an in-modal validation
      # / create error (kept off the method-editor's shared `save_error`);
      # `new_class_name` / `new_class_super` retain the in-flight field values so a
      # rejected submit re-renders the modal with what the owner typed.
      |> assign(:new_class_open, false)
      |> assign(:new_class_error, nil)
      |> assign(:new_class_name, "")
      |> assign(:new_class_super, "Object")
      # Rename modal (ADR 0114 Phase 5, BT-3277): the editor's owner-only
      # "Rename Class"/"Rename Method" affordance, closed by default. Opening
      # it reads the active tab (a `:def` tab renames the class via
      # `renameTo:`; a `:method` tab renames the selector via
      # `renameSelector:to:`) and pre-fills the field with the current name,
      # mirroring the New Class modal's shape above. `rename_kind` (`:class` |
      # `:method` | `nil`) picks which primitive `rename_submit` drives;
      # `rename_class`/`rename_side`/`rename_old_selector` are the target
      # captured when the modal opened (not re-read from the active tab on
      # submit, so the target can't drift under a mid-edit tab switch);
      # `rename_new_name` is the in-flight field value and `rename_error`
      # carries an in-modal validation/rename error, kept off the
      # method-editor's shared `save_error` just like `new_class_error`.
      |> assign(:rename_open, false)
      |> assign(:rename_kind, nil)
      |> assign(:rename_class, nil)
      |> assign(:rename_side, nil)
      |> assign(:rename_old_selector, nil)
      |> assign(:rename_new_name, "")
      |> assign(:rename_error, nil)
      # Navigation aids (BT-2495, epic BT-2482 Phase 3): the top-bar omni search
      # and the method-editor Senders/Implementors popovers. Both ride thin
      # `:read` facade ops over the navigation channel (ADR 0096), so they work
      # for the Observer role too.
      #   * `:omni_query` — the live search box text; `:omni_results` the filtered
      #     class/selector matches; `:omni_open` whether the results popover shows.
      #     The OmniSearch JS hook handles arrow/enter keyboard nav client-side
      #     (Phoenix.LiveViewTest can't see it — hence the Playwright coverage) and
      #     pushes `omni_open` on enter/click to open the chosen result.
      #   * `:nav_popover` — the Senders/Implementors result popover: `nil` when
      #     closed, else `%{kind, selector, sites}` for the open list.
      |> assign(:omni_query, "")
      |> assign(:omni_results, [])
      |> assign(:omni_open, false)
      |> assign(:nav_popover, nil)
      # Appearance/settings menu (the Tweaks panel): a top-bar gear opens it as a
      # dropdown rather than a permanent sidebar panel. The panel stays mounted
      # whenever the cockpit is (the `TweaksPanel` hook applies the saved theme on
      # mount); `:show_settings` only toggles the dropdown's visibility.
      |> assign(:show_settings, false)
      # Floating inspector windows (BT-2493, epic BT-2482 Phase 3): the spike's
      # Dock/Float toggle (spikes/cockpit-ux-spike/app.jsx). In `"float"` mode a
      # binding click / Inspect-it opens a *floating, draggable, stackable*
      # inspector window instead of driving the docked pane; `"docked"` (the
      # default) keeps the single right-column Inspector. `:inspector_mode`,
      # `:windows`, `:next_window_id` and `:window_z` are already set above by
      # `Inspector.init_assigns()` (BT-3302) — see `Inspector`'s `@moduledoc`
      # for what each drives.
      # Panel visibility toggles (BT-2559): dismissable side panels + collapsible
      # dock. Each panel can be hidden via a close button; toggle buttons in the
      # top bar re-show them. The dock can be collapsed/expanded.
      |> assign(:show_browser, true)
      |> assign(:show_inspector, true)
      |> assign(:show_dock, true)
      # Method-editor doc block (BT-2558) starts collapsed: the signature is
      # always shown as a one-line summary, but the rendered `///` doc body —
      # which is *also* present verbatim in the editable source below — is hidden
      # until the user expands it, so it no longer crowds the editor by default.
      # Server-held (not a native <details>) so the open state survives the
      # frequent phx-change re-renders morphdom would otherwise reset. This is the
      # collapsed default for a fresh session; on a reconnect (which mounts a brand-
      # new process) the prior expand state is restored from the registry stash by
      # `restore_doc/3`, so an expanded block stays expanded across a socket drop,
      # redeploy, or laptop wake (BT-2570).
      |> assign(:doc_expanded, false)
      |> SystemBrowser.assign_browser_native_modules()
      |> SystemBrowser.assign_browser_type_aliases()
      # BT-2636: the set of expanded Changes-pane rows (keyed by the row's
      # `{class, selector, side}` — BT-3195 added `side` so a same-selector
      # instance-side and class-side entry, both now possible simultaneously
      # since BT-3187's `(class, selector, side)` shadow-key fix, get
      # independent toggle state instead of sharing one), driven by the
      # leading disclosure caret. A row's structured net-vs-disk diff renders
      # beneath it only while its key is in this set; collapsed by default so
      # the table stays compact.
      |> assign(:expanded_changes, MapSet.new())
      # BT-2591: the four mount-time workspace reads (browser classes, bindings,
      # the active ChangeLog, the autoflush flag) used to run as *synchronous*
      # RPCs here, so a slow/unreachable workspace blocked the connected mount
      # (~5s each worst case) before the cockpit could render. They now start in
      # their loading/empty state so the first render is immediate, and a single
      # off-socket `start_async(:mount_load, …)` performs all four reads; the
      # results fold into these assigns in `handle_async(:mount_load, …)`. The
      # empty defaults double as the graceful-degradation fallback (lists empty,
      # autoflush false) if the load fails — no error is shown for the initial
      # mount load (the panes simply render their empty state until it resolves).
      #
      # The `autoflush` flag (ADR 0082 Phase 4, BT-2590 S2) gates the post-save
      # git refresh: a per-method save only shells out to git when autoflush is
      # on. It is read once here — the cockpit has no toggle, so a later REPL/MCP
      # `Workspace autoflush: true` leaves this cached copy stale (worst case: a
      # missed git-panel refresh, a UX miss not data loss). Defaults to `false`
      # (no extra shell-out) when the workspace is unreachable.
      |> assign(:browser_classes, [])
      |> assign(:browser_error, nil)
      |> assign(:bindings, [])
      |> assign(:bindings_error, nil)
      |> assign(:changes, [])
      |> assign(:changes_error, nil)
      |> assign(:autoflush, false)
      # BT-2619 (race): per-surface "loaded" flags gating the async mount fold so a
      # live push — or a direct synchronous write (BT-3321) — that lands BEFORE the
      # mount load resolves is not clobbered by the (staler) mount snapshot.
      # `:browser_classes_loaded` and `:changes_loaded` are independent (BT-3321:
      # split from one combined `:source_loaded` — the two surfaces do NOT always
      # refresh together, e.g. `save_method`/`ClassModals` write `changes` or
      # `browser_classes` directly, bypassing `:do_source_refresh`);
      # `:bindings_loaded` covers the bindings pane. All three start `false` and
      # are set `true` ONLY by a *successful* apply of that surface's data —
      # whether from a push refresh, a direct write, or the mount fold itself (see
      # `apply_changes/2`, `SystemBrowser.apply_browser_classes/2`,
      # `apply_bindings/2`) — never by a separate post-hoc marker; `handle_async(
      # :mount_load, …)` then applies a surface's mount read only while its flag is
      # still `false`. Success-only, monotonic (false→true, never back) gating is
      # the crux of the edge case: an early write that ERRORED leaves the flag
      # `false`, so the later-completing mount load's *successful* data still
      # folds in (no lingering error flash). A remount = new process = fresh
      # `false` flags, so reconnect/resume re-loads normally. `autoflush` has no
      # competing push, so it always folds in (no flag).
      |> assign(:browser_classes_loaded, false)
      |> assign(:changes_loaded, false)
      |> assign(:bindings_loaded, false)
      |> start_mount_load(pid)
      # Git panel (BT-2586): the post-flush VCS surface. Loaded lazily the first
      # time the Git tab is opened (and refreshed after flush/save), so a page
      # load never shells out to git for users who never open the tab. The
      # status/log loads run off-socket via `start_async` (BT-2590), so opening
      # the tab / refreshing never blocks other socket events while git runs.
      |> assign(:git_status, nil)
      |> assign(:git_log, [])
      |> assign(:git_error, nil)
      |> assign(:git_diff, nil)
      |> assign(:git_diff_path, nil)
      |> stream(:transcript, [], reset: true)
      |> stream(:repl, [], reset: true)
    else
      other ->
        Logger.error("subscribe failed: #{inspect(other)}")
        # Drop whichever subscription may have succeeded, then release the
        # session through the registry (which closes it) so we don't leave a
        # dangling subscriber or an orphaned session behind.
        ws_client().unsubscribe_all(self())
        force_close(socket.assigns[:token], pid)

        assign(socket,
          connected: false,
          error: "subscribe failed: #{inspect(other)}"
        )
    end
  end

  # BT-5081: attach THIS LiveView pid to the workspace session so the workspace
  # monitors its client. When the front dies without running `terminate/2` (the
  # desktop shell's SIGKILL, a VM crash) the session becomes *detached* and the
  # workspace's idle reaping (BT-5012) covers it, instead of the runtime treating
  # a never-attached session as permanently active. Best-effort: an older
  # workspace without the lifecycle API just keeps the previous behaviour.
  defp attach_session_best_effort(pid) do
    case ws_client().attach_session(pid, self()) do
      :ok ->
        :ok

      other ->
        Logger.warning(
          "attach_session failed (idle reaping will not see this front): #{inspect(other)}",
          domain: [:beamtalk, :liveview]
        )

        :ok
    end
  end

  # BT-5129: the session this tab's UI state belongs to is gone. Clear every
  # surface tied to it (REPL + transcript streams, the `repl-N` dom ids the new
  # session would reuse, the Inspect terms that point at dead objects) and the
  # eval state (a stale `eval_running` hides Stop and wedges the queue; a queued
  # eval would drain into dead-pid errors). Called on `DOWN`; `bind_session`
  # does the same via its fresh assigns on Reconnect.
  @doc false
  def reset_session_ui(socket) do
    # Cancel the in-flight eval task too: its late completion would otherwise
    # clear `eval_running` while a post-Reconnect eval runs and start a second
    # concurrent one.
    #
    # BT-5189: every internal operation that was running or queued is told so
    # in its own UI (a rename modal, the method editor's status, a poke bar)
    # before the eval state is cleared, rather than silently never running.
    socket
    |> Phoenix.LiveView.cancel_async(:eval)
    |> BtAttachWeb.Live.Dock.abandon_evals(
      "The workspace connection was lost before it finished; it may not have run.",
      "Cancelled: the workspace connection was lost."
    )
    |> Phoenix.LiveView.stream(:transcript, [], reset: true)
    |> Phoenix.LiveView.stream(:repl, [], reset: true)
    |> assign(
      repl_seq: 0,
      repl_terms: %{},
      repl_history_pos: nil,
      eval_running: false,
      eval_current: nil,
      eval_queue: [],
      eval_pending: MapSet.new()
    )
  end

  # The single place the "workspace lost" state is resolved. The banner's
  # Reconnect control calls it today; an automatic reconnect on `DOWN` would call
  # the same function, so changing that UX is a one-line change in
  # `handle_info({:DOWN, …})`.
  #
  # Only a successful re-attach clears the lost state. If the workspace is still
  # down, `attach/1` leaves the socket `connected: false`; keep the previous
  # socket (and its banner + Reconnect button) and add the failure to the
  # banner, so the user can retry instead of dead-ending until a page reload.
  @doc false
  def reconnect_workspace(socket) do
    {:ok, attached} = attach(assign(socket, :workspace_lost, nil))

    if attached.assigns[:connected] do
      attached
    else
      assign(socket, :reconnect_error, attached.assigns[:error])
    end
  end

  # BT-5081: monitor the session pid from the LiveView. A workspace restart (or
  # a lost distribution link) kills the monitored remote pid; the `DOWN` is
  # handled in `handle_info/2` by showing a "workspace lost" banner with a
  # Reconnect control rather than leaving a dead `session_pid` that errors
  # every op until a manual page reload. Replaces any previous monitor so a
  # re-bind (Reconnect) never accumulates stale refs.
  defp monitor_session(socket, pid) do
    case socket.assigns[:session_monitor] do
      ref when is_reference(ref) -> Process.demonitor(ref, [:flush])
      _ -> :ok
    end

    assign(socket, :session_monitor, Process.monitor(pid))
  end

  # BT-2598: best-effort subscribe to the class-lifecycle push stream. A failure
  # (an older workspace without the wiring, a transient dist hiccup) is logged but
  # does not fail the mount — the cockpit degrades to the clean-tab re-read on
  # re-activation rather than a live push, and re-subscribes on the next remount.
  defp subscribe_classes_best_effort(socket) do
    case Facade.dispatch(:subscribe_classes, %{pid: self()}, RequestContext.build(socket)) do
      :ok ->
        :ok

      other ->
        Logger.warning("subscribe_classes failed (push refresh degraded): #{inspect(other)}",
          domain: [:beamtalk, :liveview]
        )

        :ok
    end
  end

  # ADR 0105 Phase 1 (BT-2779): best-effort subscribe to the reload-check push
  # stream. Same rationale as `subscribe_classes_best_effort/1` — an older
  # workspace without the wiring, or a transient dist hiccup, degrades the
  # reload-findings panel to whatever `initial_reload_findings/1` read at
  # mount (stale until next remount) rather than failing the whole session.
  defp subscribe_reload_check_best_effort(socket) do
    case Facade.dispatch(:subscribe_reload_check, %{pid: self()}, RequestContext.build(socket)) do
      :ok ->
        :ok

      other ->
        Logger.warning(
          "subscribe_reload_check failed (reload-findings panel degraded): #{inspect(other)}",
          domain: [:beamtalk, :liveview]
        )

        :ok
    end
  end

  # ADR 0105 Phase 1 (BT-2779): the initial reload-findings snapshot for a
  # fresh mount. An unreachable workspace or an unexpected reply degrades to
  # an empty panel (consistent with `changes`/`browser_classes`'s
  # graceful-degradation default) rather than failing the mount.
  defp initial_reload_findings(socket) do
    case Facade.dispatch(:reload_findings, %{}, RequestContext.build(socket)) do
      {:ok, findings} when is_list(findings) -> findings
      _other -> []
    end
  end

  # Close a session immediately (not via the grace timer): used when binding
  # fails, so a half-started session can't linger. A registered token is
  # discarded (the registry closes + forgets it now); an unregistered (nil-token)
  # session is closed directly.
  defp force_close(token, _pid) when is_binary(token) do
    SessionRegistry.discard(token)
    :ok
  end

  defp force_close(_token, pid) when is_pid(pid) do
    Workspace.close_session(pid)
    :ok
  end

  defp force_close(_token, _pid), do: :ok

  # ── Mount-time workspace reads (BT-2591) ─────────────────────────────────────

  # BT-2591: kick the four mount-time workspace reads (browser classes, bindings,
  # the active ChangeLog, the autoflush flag) off the connected mount. Previously
  # each ran as a *synchronous* RPC in `bind_session`, so a slow/unreachable
  # workspace blocked the connected mount (~5s each worst case) before the cockpit
  # could render. We now gather all four in a single off-socket `start_async`
  # task (mirroring the git panel's `:git_load`, BT-2590): the mount returns
  # immediately with the loading/empty assigns already set, the panes render their
  # empty state, and the reads' results land in `handle_async(:mount_load, …)`.
  #
  # The task captures only `ctx` + `pid` (never `socket`) and returns the four
  # raw `Facade.dispatch` outcomes in a map so the fold applies them atomically.
  defp start_mount_load(socket, pid) do
    ctx = RequestContext.build(socket)

    start_async(socket, :mount_load, fn ->
      # Runs in a Task off the LiveView process — never touch `socket` here.
      %{
        browser_classes: Facade.dispatch(:browse_classes, %{}, ctx),
        bindings: Facade.dispatch(:bindings, %{session_pid: pid}, ctx),
        changes: Facade.dispatch(:changes, %{}, ctx),
        autoflush: Facade.dispatch(:autoflush, %{}, ctx)
      }
    end)
  end
end

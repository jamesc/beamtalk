# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.Inspector do
  @moduledoc """
  The docked Inspector + floating object-window pane, extracted out of
  `BtAttachWeb.WorkspaceLive` (BT-3291, epic BT-3290) so its
  `handle_event`/`handle_info` branches and their supporting helpers are
  directly unit-testable instead of only reachable through a full-LiveView
  integration test.

  Originally built as the "Bindings + Inspector restyle" (BT-2486, epic
  BT-2482 Phase 1) and its live-tracking follow-up "Inspector live tracking —
  field flash, freeze, pid stats chips, poke quick-actions" (BT-2492), plus
  the floating-window mode (BT-2493).

  This module owns:

    * The docked Inspector's inspect/drill/crumb navigation, live tracking
      (per-object change subscription, field-flash coalescing, freeze/
      unfreeze), pid-stats chips, and owner-only poke quick-action.
    * The floating-window overlay (BT-2493): open/drill/crumb/close/focus/
      move/freeze/poke per window, each a self-contained inspector keyed by
      id, plus the `inspector_windows/1` function component that renders the
      overlay.
    * Reconnect persistence for the open window desk (BT-2527 #3).

  State stays on the LiveView's own socket — it is threaded today as plain
  `WorkspaceLive` assigns (not a `Phoenix.LiveComponent`'s isolated
  assigns), initialised in `WorkspaceLive.bind_session/3` from this module's
  `init_assigns/0` (BT-3302 — the canonical key list + defaults live there,
  not hand-copied here as prose) and read live by object-change pushes
  delivered straight to the LiveView pid. `WorkspaceLive` still owns
  `handle_event/3`/`handle_info/2` (a `Phoenix.LiveView` callback contract),
  but delegates every inspector/window event and push to the functions here
  by name — see the `@inspector_events` guard clause in `WorkspaceLive`,
  which reads its event list from `__inspector_events__/0` below (BT-3301)
  rather than hand-maintaining a second copy.

  Every workspace read/write goes through `BtAttach.Facade.dispatch/3` (ADR
  0091 Decision 3) with the RBAC-relevant `BtAttachWeb.Live.RequestContext` —
  never a raw `BtAttach.Workspace`/`:rpc` call — so this module never
  reimplements the read-surface `inspect`/`subscribe_object`/`pid_stats` ops
  or the RBAC gate `eval` already rides for poke (AGENTS.md
  no-duplicate-implementations).
  """

  use BtAttachWeb, :html

  alias BtAttach.Facade
  alias BtAttach.SessionRegistry
  alias BtAttach.Workspace
  alias BtAttachWeb.Live.FacadeError
  alias BtAttachWeb.Live.Inspector.View
  alias BtAttachWeb.Live.RequestContext

  # Moved to `Inspector.View` in BT-5112; kept so every call site is unchanged.
  defdelegate inspector_windows(assigns), to: View
  defdelegate stat_mailbox(stats), to: View
  defdelegate stat_reductions(stats), to: View
  defdelegate stat_status(stats), to: View
  defdelegate term_kind(term), to: View
  defdelegate field_rows(fields), to: View
  defdelegate scalar_kind(term), to: View
  defdelegate supervisor_child_rows(term), to: View
  defdelegate target_info(label, term), to: View
  defdelegate children_cut_notice(target), to: View
  defdelegate supervisor_target_info(label, term, rows, truncated), to: View
  defdelegate watched_pid?(watch, pid), to: View
  defdelegate valid_receiver?(label), to: View
  defdelegate window_watched_terms(windows), to: View

  # ── canonical event list (BT-3301) ───────────────────────────────────────
  #
  # The single source of truth for which event names `handle_event/3` below
  # implements. Before BT-3301, `WorkspaceLive`'s `@inspector_events` was an
  # independently hand-maintained copy of these same 16 names — nothing tied
  # the two lists together, so a name added/renamed/removed on one side
  # without the other failed silently at runtime (a `FunctionClauseError` on
  # click, or dead code never reached), not at compile/test time. Now
  # `WorkspaceLive` reads `__inspector_events__/0` directly instead of
  # hand-copying the list, so there is only one list to keep in sync with the
  # clauses below — `inspector_test.exs`'s "@inspector_events coverage" test
  # scans this file's `handle_event/3` clause heads and asserts the two stay
  # identical, plus that every name here resolves to a working, non-crashing
  # clause.
  @inspector_events ~w(
    inspect drill crumb freeze_toggle poke close_inspector set_inspector_mode
    window_close window_crumb window_drill window_focus window_freeze
    window_moved window_poke window_reset_positions dismiss_window_error kill_actor
  )

  @doc false
  def __inspector_events__, do: @inspector_events

  # ── canonical default-assigns map (BT-3302) ─────────────────────────────
  #
  # The single source of truth for which socket-assign keys the docked
  # Inspector + floating windows own, and their fresh-session defaults.
  # Before BT-3302, `WorkspaceLive.bind_session/3` initialised these 15 keys
  # as one hand-written `assign/3` pipe with no tether back to the keys this
  # module actually reads/writes (its own `@moduledoc` carried an
  # independently hand-maintained — and, it turned out, already stale, since
  # it omitted `:inspect_error` — prose copy of the same list). A rename here
  # that missed a call site in this file (or vice versa) had no compile/test
  # signal, only a `KeyError`/nil-pattern-match crash the first time a user
  # drove the affected code path.
  #
  # `bind_session/3` now assigns this map directly instead of hand-copying
  # the keys, and `InspectorTest`'s `base_socket/1` fixture is built from it
  # too (merged with the WorkspaceLive-context keys Inspector only reads,
  # never initialises: `:session_pid`, `:current_user`, `:role`,
  # `:session_id`). With both the production init path and the test fixture
  # deriving from this one map, a key this module's functions actually
  # pattern-match/assign on but that goes missing here fails the very next
  # `mix test` run (a `KeyError` reading a socket assign that no longer
  # exists) rather than waiting for a user to hit it live.
  @default_assigns %{
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
    # BT-5199: the "Kill actor" outcome and the actor list read right after it.
    kill_notice: nil,
    actor_list: nil,
    windows: [],
    window_z: 10,
    next_window_id: 1,
    inspector_mode: "docked"
  }

  @doc """
  The default assigns for the docked Inspector + floating windows on a
  freshly-bound `WorkspaceLive` socket (BT-3302). Called from
  `WorkspaceLive.bind_session/3` via `assign/2` — see the module doc for why
  this replaced a hand-written `assign/3` pipe there.
  """
  def init_assigns, do: @default_assigns

  # ── handle_event dispatch ────────────────────────────────────────────────
  #
  # `WorkspaceLive.handle_event/3` forwards every event whose name is in
  # `__inspector_events__/0` here unchanged (same event name, params, socket),
  # so each clause below is exactly the body the LiveView used to run
  # directly.

  # Inspect a binding by name: look up its live term and drill into it via the
  # read-surface `inspect` op. Reference-following starts here. In `"float"`
  # mode (BT-2493) the click opens a *new floating window* on the binding
  # instead of driving the docked pane; in `"docked"` mode it drives the
  # docked Inspector as before. Docked is the default, so the existing
  # single-pane flow is untouched unless the user flipped the top-bar
  # Dock/Float toggle.
  def handle_event("inspect", %{"name" => name}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    if socket.assigns.inspector_mode == "float" do
      {:noreply, open_window(socket, pid, name)}
    else
      {:noreply, inspect_binding(socket, pid, name)}
    end
  end

  # Drill into an object-valued field of the currently-inspected object: the
  # field term is itself a live `{:beamtalk_object, …}` handle, so following
  # it is the same read-surface inspect call one level deeper. The clicked
  # field's term is carried back to the server by index against the current
  # rows, so we never round-trip a flattened string.
  def handle_event("drill", %{"index" => index}, %{assigns: %{inspect_rows: rows}} = socket) do
    # `index` is client-supplied; parse defensively so a malformed value can't
    # crash the LiveView (`String.to_integer/1` would raise on non-digits).
    with {i, ""} when i >= 0 <- Integer.parse(index),
         %{term: term, name: name} <- Enum.at(rows, i) do
      # Following a reference extends the drill breadcrumb one level deeper.
      crumbs = socket.assigns.inspect_crumbs ++ [%{label: to_string(name), term: term}]
      {:noreply, inspect_term(socket, name, term, crumbs)}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("drill", _params, socket), do: {:noreply, socket}

  # Jump back to an earlier level of the drill breadcrumb (BT-2486): truncate
  # the trail at the clicked crumb and re-inspect that level's live term.
  # Defensive against a client-supplied index that no longer maps to a crumb.
  def handle_event("crumb", %{"index" => index}, %{assigns: %{inspect_crumbs: crumbs}} = socket) do
    with {i, ""} when i >= 0 <- Integer.parse(index),
         %{term: term, label: label} <- Enum.at(crumbs, i) do
      {:noreply, inspect_term(socket, label, term, Enum.take(crumbs, i + 1))}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("crumb", _params, socket), do: {:noreply, socket}

  # ── live Inspector tracking events (BT-2492, epic BT-2482 Phase 3) ───────────

  # Freeze / unfreeze the Inspector's live tracking (the spike's `iw-freeze`
  # toggle, inspector.jsx). Freezing drops the per-object change subscription
  # so the pane holds the snapshot it has — no more field-flash, no re-reads.
  # Unfreezing re-subscribes the *current* head object and refreshes its
  # fields + stats immediately so the pane catches up to the live state. A
  # toggle with no object inspected (or a scalar head) just flips the flag.
  def handle_event("freeze_toggle", _params, socket) do
    {:noreply, toggle_freeze(socket)}
  end

  # Owner-only "send a message" quick action (the spike's PokeBar / quick-
  # pokes, inspector.jsx). Sends the typed Beamtalk message to the inspected
  # actor by eval'ing `<binding> <message>` against the workspace session —
  # the same `eval` facade op the Workspace dock uses, so poke invents no new
  # server op and rides the existing RBAC gate (an Observer's `eval` is
  # refused; the bar is also owner-gated in the markup). The object is
  # addressed by its *binding name* (the head crumb / target label), which is
  # the live handle's source-level name; a drilled field with no binding name
  # can't be poked, so we say so.
  def handle_event("poke", %{"message" => message}, socket) when is_binary(message) do
    {:noreply, poke_object(socket, message)}
  end

  def handle_event("poke", _params, socket), do: {:noreply, socket}

  # Owner-only "Kill actor" (BT-5199): the button carries a `data-confirm`
  # prompt, so this runs only after the user confirmed. Sends the `kill` facade
  # op (the wire `kill` operation, `:admin` capability — an Observer is refused
  # server-side even with a crafted event), then closes the pane, which would
  # otherwise show a dead actor (and any floating window on it), and reads the
  # actor list again so the user
  # sees the actor gone. A refusal (a system actor, an actor that already
  # stopped) is shown in the pane, which stays open.
  def handle_event("kill_actor", _params, socket) do
    {:noreply, kill_inspected_actor(socket)}
  end

  # ── floating inspector windows (BT-2493, epic BT-2482 Phase 3) ───────────────

  # Flip the Inspector between docked and floating ("overlay") modes (the
  # spike's Dock/Float toggle). Switching to docked leaves the open windows
  # alone — they stay in state and reappear if the user flips back — so a
  # misclick doesn't tear down a desk full of inspectors and their
  # subscriptions. Only an explicit window close releases a window's watch.
  def handle_event("set_inspector_mode", %{"mode" => mode}, socket)
      when mode in ~w(docked float) do
    {:noreply, assign(socket, inspector_mode: mode)}
  end

  def handle_event("set_inspector_mode", _params, socket), do: {:noreply, socket}

  # Close *only* the Inspector pane (BT-2611), not the whole right column. The
  # `×` lives in the Inspector sub-pane header, so it must dismiss just the
  # Inspector and leave the Bindings pane (and the column) visible — the
  # whole-column show/hide stays on `toggle_inspector`/`.panel-toggle`. We
  # reset the inspector target/rows/crumbs/error back to the empty state and
  # tear down the live subscription via `track_pane(nil)` so re-inspecting an
  # object later rebinds cleanly. Unfreeze too, so a frozen pane doesn't
  # reopen stale on the next inspect. `show_inspector` is intentionally left
  # untouched.
  def handle_event("close_inspector", _params, socket) do
    socket =
      socket
      |> assign(
        inspect_target: nil,
        inspect_rows: [],
        inspect_crumbs: [],
        inspect_error: nil,
        inspect_frozen: false,
        kill_notice: nil,
        actor_list: nil
      )

    {:noreply, update_docked_pane(socket, &track_pane(&1, socket, nil))}
  end

  # Drill into an object-valued field of a *floating window* (BT-2493): the
  # field term is carried by index against that window's current rows,
  # exactly like the docked `"drill"` event but scoped to one window.
  # Defensive against a malformed id/index so a crafted event can't crash the
  # LiveView.
  def handle_event("window_drill", %{"id" => id, "index" => index}, socket) do
    with %{} = win <- find_window(socket, id),
         {i, ""} when i >= 0 <- Integer.parse(index),
         %{term: term, name: name} <- Enum.at(win.rows, i) do
      crumbs = win.crumbs ++ [%{label: to_string(name), term: term}]

      {:noreply,
       update_window(socket, id, fn w -> inspect_pane(w, socket, name, term, crumbs) end)}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("window_drill", _params, socket), do: {:noreply, socket}

  # Walk a floating window's drill breadcrumb back to an earlier level:
  # truncate its crumb trail at the clicked index and re-inspect that level's
  # live term.
  def handle_event("window_crumb", %{"id" => id, "index" => index}, socket) do
    with %{} = win <- find_window(socket, id),
         {i, ""} when i >= 0 <- Integer.parse(index),
         %{term: term, label: label} <- Enum.at(win.crumbs, i) do
      crumbs = Enum.take(win.crumbs, i + 1)

      {:noreply,
       update_window(socket, id, fn w -> inspect_pane(w, socket, label, term, crumbs) end)}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("window_crumb", _params, socket), do: {:noreply, socket}

  # Close a floating window: drop it from the list and release its per-object
  # change subscription (BT-2493 acceptance: "Close removes the window and
  # releases any subscriptions"). Idempotent — closing an unknown id is a
  # no-op.
  def handle_event("window_close", %{"id" => id}, socket) do
    {:noreply, close_window(socket, id)}
  end

  def handle_event("window_close", _params, socket), do: {:noreply, socket}

  # Bring a floating window to the front (z-order follows focus, the spike's
  # stacking). The WindowDrag JS hook fires this on a mousedown anywhere in
  # the window; we bump the clicked window's z above the current max so it
  # overlays the others. Pure view state — no workspace round-trip.
  def handle_event("window_focus", %{"id" => id}, socket) do
    {:noreply, focus_window(socket, id)}
  end

  def handle_event("window_focus", _params, socket), do: {:noreply, socket}

  # Persist a floating window's final position after a drag (BT-2493): the
  # WindowDrag hook reports x/y ONCE on drop (no per-mousemove round-trip), so
  # the position lives in LV state and survives an unrelated re-render.
  # Coordinates are clamped to non-negative integers so a crafted payload
  # can't place a window off into NaN-land.
  def handle_event("window_moved", %{"id" => id, "x" => x, "y" => y}, socket) do
    {:noreply, update_window(socket, id, fn w -> %{w | x: clamp_coord(x), y: clamp_coord(y)} end)}
  end

  def handle_event("window_moved", _params, socket), do: {:noreply, socket}

  # Bring every floating window back onto the default on-screen ladder
  # (BT-2527 #4): recovery for a window dragged/restored outside the visible
  # viewport. Pure view state — positions only.
  def handle_event("window_reset_positions", _params, socket) do
    {:noreply, reset_window_positions(socket)}
  end

  # Freeze / unfreeze a single floating window's live tracking (per-window
  # freeze, mirroring the docked `"freeze_toggle"`). Each window watches
  # independently, so one frozen window holds its snapshot while others keep
  # tracking.
  def handle_event("window_freeze", %{"id" => id}, socket) do
    {:noreply, update_window(socket, id, fn w -> toggle_window_freeze(socket, w) end)}
  end

  def handle_event("window_freeze", _params, socket), do: {:noreply, socket}

  # Owner-only per-window poke (the docked `"poke"`, scoped to one window):
  # send the typed message to that window's inspected actor by eval'ing
  # `<binding> <message>`. Rides the same RBAC-gated eval op +
  # well-formedness gate as the docked poke; a window not at a named-binding
  # root reports it can't send.
  def handle_event("window_poke", %{"id" => id, "message" => message}, socket)
      when is_binary(message) do
    window = Enum.find(socket.assigns.windows, &(&1.id == id))
    {:noreply, if(window, do: poke_window(socket, window, message), else: socket)}
  end

  def handle_event("window_poke", _params, socket), do: {:noreply, socket}

  # Dismiss a per-window inspector error. `@windows` is a list of window
  # maps; the client sends the window `id` so we clear `:error` on the
  # matching window only.
  def handle_event("dismiss_window_error", %{"id" => id}, socket) do
    {:noreply, update_window(socket, id, fn w -> Map.put(w, :error, nil) end)}
  end

  def handle_event("dismiss_window_error", _params, socket), do: {:noreply, socket}

  # ── handle_info dispatch ─────────────────────────────────────────────────
  #
  # `WorkspaceLive.handle_info/2` forwards the object-change / coalesced-
  # refresh messages below unchanged, mirroring the `handle_event` dispatch
  # above.

  # Per-object change push (BT-2492, backend BT-2489): the watched actor
  # committed a state write. Like the bindings stream this is a *refresh
  # trigger*, not the data — re-read the object's fields + pid stats through
  # the read-surface so the Inspector shows the live snapshot, and bump
  # `:flash_gen` so the FieldFlash JS hook flashes the cells that changed.
  #
  # **Coalescing (no flash storm):** a hot actor can emit a burst of writes.
  # Rather than re-read on every push (which would re-render — and re-flash —
  # the whole table repeatedly), the first push schedules a single deferred
  # refresh via a self-send and sets `:refresh_pending`; intervening pushes
  # are dropped while the flag is set. The deferred `:do_object_refresh` then
  # performs ONE re-read for the whole burst. We ignore the push entirely
  # once frozen, once we've navigated off this object, or for a `pid` that
  # isn't the currently-watched one — the watch server warns it may deliver
  # one final push after we unsubscribe (a navigate-away/freeze race), so we
  # drop a push whose pid doesn't match the head.
  def handle_info({:object_changed, pid, _slots}, %{assigns: assigns} = socket) do
    # The push fans out to TWO independent watchers: the docked Inspector
    # (its `:inspect_watch`) and any floating windows watching this same pid
    # (BT-2493). Each coalesces its own burst, so we schedule the docked
    # refresh here and let `notify_windows_changed/2` schedule a per-pid
    # window refresh — a single push can pulse both the docked pane and a
    # float window on the same actor.
    docked =
      cond do
        assigns.inspect_frozen or not watched_pid?(assigns.inspect_watch, pid) ->
          socket

        assigns.refresh_pending ->
          # A refresh is already queued for this burst — collapse this push
          # into it.
          socket

        true ->
          Process.send_after(self(), :do_object_refresh, refresh_debounce_ms())
          assign(socket, refresh_pending: true)
      end

    {:noreply, notify_windows_changed(docked, pid)}
  end

  # The coalesced refresh fired by `{:object_changed, …}`: re-read the
  # watched object's fields + stats once for the whole burst, then clear the
  # pending flag so the next burst schedules afresh. Guarded against a stale
  # timer firing after the pane froze or navigated away (the watched term
  # went nil / changed).
  def handle_info(:do_object_refresh, %{assigns: %{inspect_watch: term}} = socket)
      when not is_nil(term) do
    socket = assign(socket, refresh_pending: false)
    {:noreply, update_docked_pane(socket, &refresh_pane(&1, socket, term))}
  end

  def handle_info(:do_object_refresh, socket) do
    {:noreply, assign(socket, refresh_pending: false)}
  end

  # The coalesced per-window refresh fired by `notify_windows_changed/2` for
  # one `pid`: re-read every floating window whose watched head is that pid,
  # clearing ONLY those windows' pending flags so the burst collapses into
  # one re-read + flash per window. A window watching a *different* pid keeps
  # its own `:refresh_pending` untouched — its own `{:do_window_refresh,
  # otherpid}` timer is still in flight and must not be pre-empted (else that
  # window's refresh would be silently dropped). A window that froze or
  # navigated off this pid since the timer armed no longer matches
  # `watched_pid?/2`, so it is left as-is.
  def handle_info({:do_window_refresh, pid}, socket) do
    windows =
      Enum.map(socket.assigns.windows, fn w ->
        cond do
          # Pending refresh for this pid: re-read + flash, clearing the flag.
          w.refresh_pending and watched_pid?(w.watch, pid) ->
            refresh_pane(%{w | refresh_pending: false}, socket, w.watch)

          # Still watching this pid but not pending (already serviced / never
          # scheduled): clear any stale flag so it can't wedge future
          # refreshes.
          watched_pid?(w.watch, pid) ->
            %{w | refresh_pending: false}

          # A different pid (or no watch): leave it untouched — its own timer
          # (if any) is still in flight and must not be pre-empted.
          true ->
            w
        end
      end)

    {:noreply, assign(socket, :windows, windows)}
  end

  # ── shared with WorkspaceLive (repl_inspect, mount, terminate) ──────────────
  #
  # Public because they're called directly from `WorkspaceLive` at sites
  # outside `@inspector_events` (the "Events to move" list in BT-3291 is
  # `handle_event`-only): the REPL's Inspect-it action, and session
  # mount/terminate's window-desk resume/stash.

  # ── shared docked/window "pane" core (BT-3319) ───────────────────────────
  #
  # The docked pane's state lives in `socket.assigns` under `inspect_`-
  # prefixed keys (plus two unprefixed assigns, `flash_gen`/`refresh_pending`);
  # a floating window's state is the same 11 fields, unprefixed, in one plain
  # map in `socket.assigns.windows`. `docked_pane/1` projects the docked
  # assigns into that same unprefixed shape (tagged `id: :docked`) so the
  # five `inspect_term/4`+`inspect_window/5`-shaped pairs below collapse into
  # one core each, operating on a plain "pane" map — a window's map already
  # has this shape, so a window call site hands its `w` straight to a core
  # with no conversion; only the docked side needs this accessor pair.
  defp docked_pane(socket) do
    %{
      id: :docked,
      target: socket.assigns.inspect_target,
      rows: socket.assigns.inspect_rows,
      crumbs: socket.assigns.inspect_crumbs,
      error: socket.assigns.inspect_error,
      watch: socket.assigns.inspect_watch,
      stats: socket.assigns.inspect_stats,
      frozen: socket.assigns.inspect_frozen,
      refresh_pending: socket.assigns.refresh_pending,
      flash_gen: socket.assigns.flash_gen,
      poke_result: socket.assigns.poke_result,
      poke_error: socket.assigns.poke_error
    }
  end

  defp assign_docked_pane(socket, pane) do
    assign(socket,
      inspect_target: pane.target,
      inspect_rows: pane.rows,
      inspect_crumbs: pane.crumbs,
      inspect_error: pane.error,
      inspect_watch: pane.watch,
      inspect_stats: pane.stats,
      inspect_frozen: pane.frozen,
      refresh_pending: pane.refresh_pending,
      flash_gen: pane.flash_gen,
      poke_result: pane.poke_result,
      poke_error: pane.poke_error
    )
  end

  # Apply a pane-core function to the docked pane and write the result back
  # onto `socket.assigns` in one step — what each remaining direct docked
  # call site needs (`close_inspector`, the coalesced `:do_object_refresh`,
  # unfreeze, the poked-inspector refresh) so it stays a one-line pipe step
  # instead of hand-rolling the project/apply/write-back dance every time.
  defp update_docked_pane(socket, fun), do: assign_docked_pane(socket, fun.(docked_pane(socket)))

  # Inspect a single live term via the read-surface `inspect` op and assign
  # the resulting structured-field rows plus the drill breadcrumb (`crumbs`).
  # Object-valued fields are flagged drillable, carrying their live term so
  # the next drill follows the reference one level deeper. Non-object terms
  # are not inspectable, so we say so rather than guess. Public API kept for
  # `WorkspaceLive`'s direct calls (REPL Inspect-it, `inspect_binding/3`);
  # delegates to the shared `inspect_pane/5` core via the docked-pane
  # accessors.
  def inspect_term(socket, label, term, crumbs) do
    assign_docked_pane(socket, inspect_pane(docked_pane(socket), socket, label, term, crumbs))
  end

  # Inspect a live `term` into a `pane` — the docked Inspector or one
  # floating window (BT-3319: formerly `inspect_term/4` + `inspect_window/5`,
  # documented there as a "parameterised twin"/"mirrors the docked" pair with
  # no shared implementation): read the object's fields via the read-surface,
  # set the pane's target/rows/crumbs/error, then (re)arm its per-object
  # watch + stats. A re-inspect (drill / crumb walk-back) rebinds the watch
  # onto the new head and releases the previous one — but only if no OTHER
  # pane (a window or the docked pane) still needs that pid.
  #
  # BT-2634: a supervisor's content is its CHILDREN / supervision tree, not
  # actor instance vars. Each child row carries a live `{:beamtalk_supervisor,
  # …}` / `{:beamtalk_object, …}` handle, so the existing "drill" event
  # follows it as its own reference (ADR 0095), and the crumb walk-back
  # re-inspects the supervisor handle (re-listing its children). Live-
  # tracking is deliberately NOT armed for a supervisor (`track_pane/3`'s
  # catch-all): no field-flash, no per-object watch, no pid-stats poll
  # against a supervisor.
  defp inspect_pane(pane, socket, label, term, crumbs) do
    if Workspace.inspectable?(term) do
      case Facade.dispatch(:inspect, %{term: term}, RequestContext.build(socket)) do
        {:ok, {:supervisor_children, child_rows, truncated}} ->
          %{
            pane
            | target: supervisor_target_info(label, term, child_rows, truncated),
              rows: supervisor_child_rows(child_rows),
              crumbs: crumbs,
              error: nil
          }
          |> track_pane(socket, term)

        {:ok, fields} when is_map(fields) ->
          %{
            pane
            | target: target_info(label, term),
              rows: field_rows(fields),
              crumbs: crumbs,
              error: nil
          }
          |> track_pane(socket, term)

        {:ok, scalar} ->
          %{
            pane
            | target: target_info(label, term),
              rows: [
                %{
                  name: "value",
                  value: Workspace.format_value(scalar),
                  term: scalar,
                  drillable: false,
                  kind: term_kind(scalar)
                }
              ],
              crumbs: crumbs,
              error: nil
          }
          |> track_pane(socket, scalar)

        {:error, reason} ->
          # A failed inspect leaves no coherent head: reset the crumbs + rows
          # so a later freeze/poke doesn't act on a stale level, and drop any
          # watch.
          %{pane | target: nil, rows: [], crumbs: [], error: Workspace.render_error(reason)}
          |> track_pane(socket, nil)
      end
    else
      %{
        pane
        | target: target_info(label, term),
          rows: [],
          crumbs: crumbs,
          error: "#{label} is a #{scalar_kind(term)} — no fields to inspect"
      }
      |> track_pane(socket, term)
    end
  end

  # Inspect a binding selected by name: resolve its live term from the
  # current binding list, then inspect that term. The term — not a string —
  # drives the op.
  defp inspect_binding(socket, pid, name) do
    case Facade.dispatch(:bindings, %{session_pid: pid}, RequestContext.build(socket)) do
      pairs when is_list(pairs) ->
        case List.keyfind(pairs, name, 0) do
          # Inspecting a binding starts a fresh drill breadcrumb at this
          # object.
          {^name, term} ->
            inspect_term(socket, name, term, [%{label: to_string(name), term: term}])

          nil ->
            assign(socket, inspect_error: "binding not found: #{name}")
        end

      {:error, reason} ->
        assign(socket, inspect_error: Workspace.render_error(reason))
    end
  end

  # ── live Inspector tracking (BT-2492, backend BT-2489 / ADR 0095 §5) ─────────

  # Arm (or tear down) the per-object change subscription + pid-stats read
  # for a `pane`'s newly-inspected `term` (BT-3319: formerly `track_object/2`
  # + `track_window/3`), called on every `inspect_pane/5` so re-inspecting a
  # different object (a drill, a crumb walk-back, a fresh binding) rebinds
  # the watch onto the *current* object and drops the previous one. The flow
  # keeps the contract honest:
  #
  #   * A pid-backed object → subscribe THIS LiveView pid (over distribution)
  #     to its `{:object_changed, …}` stream, read its pid stats now, and
  #     clear any stale poke result. A frozen pane does NOT subscribe (it
  #     holds a snapshot) but still reads stats once so the chips reflect the
  #     snapshot.
  #   * A non-pid term (a scalar field, a drilled value) has nothing to
  #     watch: drop any prior subscription and clear the stats/watch.
  #
  # The pane's previously-watched term is always unsubscribed first so the
  # workspace never keeps pushing changes for an object we navigated away
  # from.
  defp track_pane(pane, socket, {:beamtalk_object, _class, _module, pid} = term)
       when is_pid(pid) do
    # Reset any in-flight coalesced-refresh flag: a pending `:do_object_refresh`
    # / `:do_window_refresh` timer was scheduled for the *previous* object, so
    # clearing the flag lets the NEW object's first change push schedule its
    # own refresh immediately (the stale timer, if it still fires, is a
    # harmless no-op on the fresh watch).
    pane = unwatch_pane(%{pane | refresh_pending: false}, socket)

    pane =
      if pane.frozen do
        # Frozen: hold the snapshot — no live subscription, but read stats
        # once so the chips populate. `watch` stays nil (nothing to
        # unsubscribe).
        %{pane | watch: nil}
      else
        case Facade.dispatch(
               :subscribe_object,
               %{term: term, pid: self()},
               RequestContext.build(socket)
             ) do
          :ok -> %{pane | watch: term}
          # A non-:ok (term not watchable, dist hiccup) leaves the pane
          # un-watched rather than claiming a live subscription that isn't
          # there.
          _ -> %{pane | watch: nil}
        end
      end

    pane
    |> refresh_pane_stats(socket, term)
    |> Map.merge(%{poke_result: nil, poke_error: nil})
  end

  # Non-object target: nothing to track. Drop any prior watch and clear
  # stats.
  defp track_pane(pane, socket, _term) do
    pane
    |> unwatch_pane(socket)
    |> Map.merge(%{stats: nil, poke_result: nil, poke_error: nil})
  end

  # Drop a pane's per-object subscription (if any) and forget the watched
  # term. Idempotent: a nil watch unsubscribes nothing.
  #
  # Reference-aware (BT-2493): the workspace keys subscriptions by `(pid,
  # subscriber)` and our subscriber is always this LiveView, so one
  # unsubscribe would silence EVERY this-pid watcher in this process. If
  # another pane (the docked Inspector, or a floating window) still watches
  # the same actor, we must NOT unsubscribe here — that pane still needs the
  # push.
  defp unwatch_pane(%{watch: nil} = pane, _socket), do: pane

  defp unwatch_pane(%{watch: term} = pane, socket) do
    unless pane_watched_elsewhere?(socket, term, pane.id) do
      Facade.dispatch(
        :unsubscribe_object,
        %{term: term, pid: self()},
        RequestContext.build(socket)
      )
    end

    %{pane | watch: nil}
  end

  # True when the pid backing `term` is still watched by some pane OTHER
  # than `except_id` (`:docked` for the docked Inspector, a window's `id`
  # otherwise) — used to avoid unsubscribing a pid a sibling pane still
  # depends on. This is the one place the docked pane and a window genuinely
  # differ (which sibling panes to scan), so `except_id` is threaded
  # explicitly rather than hidden: the docked pane never compares itself
  # against its own watch (there's only one docked pane), so it's excluded
  # by construction whenever `except_id` is `:docked`.
  defp pane_watched_elsewhere?(socket, {:beamtalk_object, _c, _m, pid}, except_id)
       when is_pid(pid) do
    docked = except_id != :docked and watched_pid?(socket.assigns[:inspect_watch], pid)

    windowed =
      Enum.any?(socket.assigns.windows, fn w ->
        w.id != except_id and watched_pid?(w.watch, pid)
      end)

    docked or windowed
  end

  defp pane_watched_elsewhere?(_socket, _term, _except_id), do: false

  # Read the inspected actor's live process metrics (mailbox/reductions/
  # status/…) and set the snapshot on the pane's head chips (BT-3319:
  # formerly `refresh_stats/2` + `refresh_window_stats/3`). A read failure
  # clears the chips rather than rendering stale numbers — the change stream
  # still drives the field flash, so the pane stays useful even when stats
  # are momentarily unavailable.
  defp refresh_pane_stats(pane, socket, term) do
    case Facade.dispatch(:pid_stats, %{term: term}, RequestContext.build(socket)) do
      {:ok, stats} when is_map(stats) -> %{pane | stats: stats}
      _ -> %{pane | stats: nil}
    end
  end

  # Re-read a `pane`'s *already-watched* object's fields + stats after a
  # change push (BT-2492) WITHOUT re-arming the subscription — the watch is
  # still live, so `track_pane/3` would needlessly unsubscribe + resubscribe
  # (BT-3319: formerly `refresh_inspector/2` + `refresh_window/3`). The drill
  # breadcrumb is preserved (same level); only the field values + stats
  # refresh. `flash_gen` bumps so the FieldFlash hook flashes the changed
  # cells. The target label/crumbs come from the pane's current head (the
  # last crumb's label).
  defp refresh_pane(pane, socket, {:beamtalk_object, _class, _module, pid} = term)
       when is_pid(pid) do
    label = pane_label(pane)

    case Facade.dispatch(:inspect, %{term: term}, RequestContext.build(socket)) do
      {:ok, fields} when is_map(fields) ->
        %{
          pane
          | target: target_info(label, term),
            rows: field_rows(fields),
            error: nil
        }
        |> refresh_pane_stats(socket, term)
        |> bump_pane_flash()

      {:ok, _scalar} ->
        # The object resolved to a scalar (no fields) — refresh stats +
        # flash; the row already reflects the value the next render reads.
        pane |> refresh_pane_stats(socket, term) |> bump_pane_flash()

      {:error, _reason} ->
        # A transient read failure on a live refresh: keep the existing rows
        # rather than blanking the pane mid-track; the next push retries.
        pane
    end
  end

  defp refresh_pane(pane, _socket, _term), do: pane

  # The label of a pane's head right now — the last drill crumb, falling
  # back to the target label. Used so a live refresh re-renders the head
  # with the same label the user navigated to (BT-3319: formerly
  # `current_inspect_label/1` + `window_label/1`).
  defp pane_label(pane) do
    case List.last(pane.crumbs) do
      %{label: label} -> label
      _ -> (pane.target || %{})[:label] || "value"
    end
  end

  defp bump_pane_flash(pane), do: %{pane | flash_gen: pane.flash_gen + 1}

  # The coalescing window for a burst of `{:object_changed, …}` pushes. A
  # small delay collapses a flurry of rapid writes into a single re-read +
  # flash. Kept as a function so a test can drive it deterministically if
  # needed.
  defp refresh_debounce_ms, do: 60

  # ── floating inspector windows (BT-2493, epic BT-2482 Phase 3) ───────────────
  #
  # Each floating window is a self-contained inspector: its own drill
  # `crumbs`, `rows`, `target`, live `watch`/`stats`/`frozen` and a
  # `flash_gen`, plus its `x`/`y`/`z` placement (client-reported by the
  # WindowDrag hook on drop / focus). The window-list lives in `:windows`;
  # helpers below open / drill / close / focus / freeze a window by id and
  # route change pushes to the right window. They reuse the same
  # read-surface ops (`:inspect`, `:subscribe_object`, `:pid_stats`) and the
  # same `target_info` / `field_rows` builders as the docked pane (BT-2486),
  # so a window's content is the docked Inspector parameterised by window id.

  # The window placement step (px): each new window is offset down-right from
  # the last so a burst of opens cascades rather than stacking dead-on (the
  # spike's `+24` cascade). The first window opens near the top-left of the
  # overlay.
  defp window_origin_x, do: 120
  defp window_origin_y, do: 96
  defp window_cascade, do: 28

  # Open a floating window on a session binding selected by name: resolve its
  # live term (same path as the docked `inspect_binding/3`), then build a
  # window on it. A binding that no longer resolves opens a window showing
  # the error rather than silently doing nothing.
  defp open_window(socket, pid, name) do
    case Facade.dispatch(:bindings, %{session_pid: pid}, RequestContext.build(socket)) do
      pairs when is_list(pairs) ->
        case List.keyfind(pairs, name, 0) do
          {^name, term} -> open_window_for_term(socket, to_string(name), term)
          nil -> open_window_error(socket, to_string(name), "binding not found: #{name}")
        end

      {:error, reason} ->
        open_window_error(socket, to_string(name), Workspace.render_error(reason))
    end
  end

  # Open a floating window on an already-resolved live `term` (used by
  # Inspect-it, whose head is the `→ result`, and by `open_window/3`). Mints
  # a fresh id + cascade position, fills its first inspector level, and arms
  # its watch. Public: called directly by `WorkspaceLive`'s `repl_inspect`
  # event (not in the `@inspector_events` delegation list).
  def open_window_for_term(socket, label, term) do
    {id, socket} = mint_window_id(socket)
    {x, y, socket} = next_window_pos(socket)
    z = socket.assigns.window_z + 1

    win = %{
      id: id,
      label: to_string(label),
      crumbs: [%{label: to_string(label), term: term}],
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
      x: x,
      y: y,
      z: z
    }

    win = inspect_pane(win, socket, label, term, win.crumbs)

    socket
    |> assign(:windows, socket.assigns.windows ++ [win])
    |> assign(:window_z, z)
  end

  # Open a window that carries only an error head (a binding that vanished):
  # still gives the user a closable window explaining what happened rather
  # than a no-op.
  defp open_window_error(socket, label, message) do
    {id, socket} = mint_window_id(socket)
    {x, y, socket} = next_window_pos(socket)
    z = socket.assigns.window_z + 1

    win = %{
      id: id,
      label: to_string(label),
      crumbs: [],
      target: nil,
      rows: [],
      error: message,
      watch: nil,
      stats: nil,
      frozen: false,
      refresh_pending: false,
      flash_gen: 0,
      poke_result: nil,
      poke_error: nil,
      x: x,
      y: y,
      z: z
    }

    socket
    |> assign(:windows, socket.assigns.windows ++ [win])
    |> assign(:window_z, z)
  end

  # ── reconnect persistence (BT-2527 #3) ──────────────────────────────────────
  #
  # A LiveView reconnect (transient socket drop / page reload) mounts a
  # brand-new process whose assigns — including the open floating-window
  # list — start empty, even though the underlying workspace session resumes
  # with its bindings intact. Without help, a desk full of inspector windows
  # silently vanishes on a blip. So `WorkspaceLive.terminate/2` stashes the
  # open windows' ROOTS in the registry (Phoenix-node memory that outlives
  # the reconnect) via `build_window_stash/1`, and the resuming mount
  # rebuilds them via `restore_windows/3`.

  # Snapshot the open floating windows into a resume stash: each window's
  # root (its first crumb — label + live term) plus placement and freeze,
  # alongside the inspector mode. Drilled levels are intentionally dropped —
  # a resume restores each window to its root (the issue's "or at least the
  # roots"). Error-only windows carry no root term, so there's nothing live
  # to reopen — they're skipped by the crumb pattern. Public: called
  # directly by `WorkspaceLive.terminate/2`.
  def build_window_stash(socket) do
    roots =
      for %{crumbs: [%{label: label, term: term} | _]} = w <- socket.assigns[:windows] || [] do
        %{label: to_string(label), term: term, x: w.x, y: w.y, z: w.z, frozen: w.frozen}
      end

    %{windows: roots, mode: socket.assigns[:inspector_mode] || "docked"}
  end

  # Rebuild the stashed desk on a genuine session resume. A fresh session or
  # a failed bind (not connected) leaves the empty desk untouched. Each
  # stashed root is reopened from its still-live term — the inspected actor
  # lives on the workspace node and survives the LiveView reconnect — which
  # re-reads its fields and re-arms its per-object watch for the NEW
  # LiveView pid, then it is placed at its saved position and re-frozen. The
  # mode is restored too so a resumed Float desk comes back in Float.
  # Public: called directly by `WorkspaceLive.attach/1` (mount).
  def restore_windows(socket, _token, :fresh), do: socket

  def restore_windows(socket, token, :resumed) do
    with true <- socket.assigns[:connected],
         %{windows: [_ | _] = roots, mode: mode} <- SessionRegistry.window_stash(token) do
      socket = assign(socket, :inspector_mode, mode)
      Enum.reduce(roots, socket, &restore_window/2)
    else
      _ -> socket
    end
  end

  # Reopen one stashed root, then override the cascade position
  # `open_window_for_term` assigned with the stashed placement and re-apply
  # its freeze.
  defp restore_window(root, socket) do
    socket = open_window_for_term(socket, root.label, root.term)

    case List.last(socket.assigns.windows) do
      %{id: id} ->
        socket
        |> update_window(id, fn w -> %{w | x: root.x, y: root.y, z: root.z} end)
        |> assign(:window_z, max(socket.assigns.window_z, root.z))
        |> restore_window_freeze(id, root.frozen)

      _ ->
        socket
    end
  end

  defp restore_window_freeze(socket, _id, false), do: socket

  defp restore_window_freeze(socket, id, true) do
    update_window(socket, id, fn w -> toggle_window_freeze(socket, w) end)
  end

  # Re-cascade every open window back onto the default on-screen ladder
  # (BT-2527 #4): a recovery affordance for a window dragged to (or restored
  # at) a spot outside the visible viewport. Positions only — drill state,
  # watches, stats and freeze are untouched.
  defp reset_window_positions(socket) do
    {windows, _} =
      Enum.map_reduce(socket.assigns.windows, 0, fn w, i ->
        step = rem(i, 8)
        x = window_origin_x() + step * window_cascade()
        y = window_origin_y() + step * window_cascade()
        {%{w | x: x, y: y}, i + 1}
      end)

    assign(socket, :windows, windows)
  end

  # Per-window freeze toggle (the docked `toggle_freeze/1`, scoped to one
  # window). Unfreeze re-arms the window's watch on its current head and
  # catches up; freeze drops its subscription (reference-aware) and holds
  # the snapshot.
  defp toggle_window_freeze(socket, %{frozen: true} = w) do
    w = %{w | frozen: false, refresh_pending: false}

    case window_head_term(w) do
      {:beamtalk_object, _c, _m, pid} = term when is_pid(pid) ->
        w
        |> rearm_window_watch(socket, term)
        |> refresh_pane(socket, term)

      _ ->
        w
    end
  end

  defp toggle_window_freeze(socket, w) do
    w
    |> unwatch_pane(socket)
    |> Map.put(:frozen, true)
  end

  defp rearm_window_watch(w, socket, term) do
    case Facade.dispatch(
           :subscribe_object,
           %{term: term, pid: self()},
           RequestContext.build(socket)
         ) do
      :ok -> %{w | watch: term}
      _ -> %{w | watch: nil}
    end
  end

  # The live term at a window's current head (its last crumb), or nil.
  defp window_head_term(w) do
    case List.last(w.crumbs) do
      %{term: term} -> term
      _ -> nil
    end
  end

  # Send `message` to a window's inspected actor by eval'ing `<binding>
  # <message>` against the session (the docked `poke_object/2`, scoped to one
  # window). The actor is addressed by its window's single-crumb binding
  # label; a window not at a named-binding root can't be poked. On success
  # the window re-reads so its fields reflect the write synchronously (the
  # docked `refresh_poked_inspector`).
  defp poke_window(socket, w, message) do
    message = String.trim(message)
    pid = socket.assigns[:session_pid]
    label = window_poke_label(w)

    cond do
      not is_pid(pid) ->
        put_window(socket, %{w | poke_result: nil, poke_error: "not attached to workspace"})

      message == "" ->
        put_window(socket, %{w | poke_result: nil, poke_error: "Enter a message to send."})

      is_nil(label) ->
        put_window(socket, %{
          w
          | poke_result: nil,
            poke_error: "Can only send to a bound object — inspect a binding to poke it."
        })

      true ->
        send_window_poke(socket, w, pid, label, message)
    end
  end

  defp send_window_poke(socket, w, pid, label, message) do
    code = "#{label} #{message}"
    crumbs = w.crumbs

    # BT-5189: the result lands only on the window it was sent from, and only
    # while that window still shows the poked object — a window closed or
    # navigated elsewhere meanwhile must not show it against another object.
    BtAttachWeb.Live.Dock.eval_async(
      socket,
      pid,
      code,
      fn socket, result ->
        update_window_at(socket, w.id, crumbs, &fold_window_poke(socket, &1, result))
      end,
      fn socket, reason ->
        update_window_at(socket, w.id, crumbs, &%{&1 | poke_result: nil, poke_error: reason})
      end
    )
  end

  defp update_window_at(socket, id, crumbs, fun) do
    update_window(socket, id, fn w -> if w.crumbs == crumbs, do: fun.(w), else: w end)
  end

  defp fold_window_poke(socket, w, result) do
    case result do
      {:ok, term, _output, _warnings} ->
        w = %{w | poke_result: "→ #{Workspace.render_term(term)}", poke_error: nil}

        # Re-read only when this window is live (`watch` is a pid). A FROZEN
        # window shows the poke result but deliberately does NOT re-read its
        # field rows (BT-2527 #6, reviewed): frozen means "snapshot", and
        # overriding it here would diverge from the docked poke, which is
        # governed by the same rule — surface parity over a one-off
        # exception. Unfreeze to see the new state.
        case w.watch do
          {:beamtalk_object, _c, _m, p} = watched when is_pid(p) ->
            refresh_pane(w, socket, watched)

          _ ->
            w
        end

      {:error, _, _, _} = err ->
        %{w | poke_result: nil, poke_error: FacadeError.render_eval_error(err)}

      {:error, _} = err ->
        %{w | poke_result: nil, poke_error: FacadeError.render_eval_error(err)}
    end
  end

  # The binding name to address a window poke to (its single-crumb root
  # label), or nil — the same well-formedness gate as the docked
  # `poke_label/1`.
  defp window_poke_label(%{crumbs: [%{label: label}]}) when is_binary(label) do
    if valid_receiver?(label), do: label, else: nil
  end

  defp window_poke_label(_w), do: nil

  # ── window-list manipulation (pure view state, no workspace round-trip) ──────

  # Mint the next unique window id (a string so it rides DOM ids + phx-value
  # cleanly) and advance the counter.
  defp mint_window_id(socket) do
    n = socket.assigns.next_window_id
    {"win-#{n}", assign(socket, :next_window_id, n + 1)}
  end

  # The cascade position for the next window: offset down-right by one step
  # per already-open window, wrapping after a few so a long-lived session
  # doesn't march windows off-screen. Returns {x, y, socket} (socket
  # unchanged — kept symmetric with `mint_window_id/1` for a tidy call site).
  defp next_window_pos(socket) do
    step = rem(length(socket.assigns.windows), 8)
    x = window_origin_x() + step * window_cascade()
    y = window_origin_y() + step * window_cascade()
    {x, y, socket}
  end

  # Look up a window by id, or nil.
  defp find_window(socket, id), do: Enum.find(socket.assigns.windows, &(&1.id == id))

  # Replace the window `id` with `fun.(window)`, leaving the list order (and
  # so the DOM order) stable — z-order is carried by each window's `:z`, not
  # list position, so an update never reshuffles the windows and resets
  # their drag positions.
  defp put_window(socket, w), do: update_window(socket, w.id, fn _ -> w end)

  defp update_window(socket, id, fun) do
    windows =
      Enum.map(socket.assigns.windows, fn w ->
        if w.id == id, do: fun.(w), else: w
      end)

    assign(socket, :windows, windows)
  end

  # Close a window: release its watch (reference-aware) and drop it from the
  # list.
  defp close_window(socket, id) do
    case find_window(socket, id) do
      nil ->
        socket

      w ->
        _ = unwatch_pane(w, socket)
        assign(socket, :windows, Enum.reject(socket.assigns.windows, &(&1.id == id)))
    end
  end

  # Bring window `id` to the front: bump its z above the current max so it
  # overlays the others. No-op for an unknown id.
  defp focus_window(socket, id) do
    case find_window(socket, id) do
      nil ->
        socket

      _w ->
        z = socket.assigns.window_z + 1

        socket
        |> assign(:window_z, z)
        |> update_window(id, fn w -> %{w | z: z} end)
    end
  end

  # Clamp a client-reported drag coordinate to a non-negative integer (a
  # crafted payload could send a float/NaN/negative).
  defp clamp_coord(n) when is_integer(n) and n >= 0, do: n
  defp clamp_coord(n) when is_float(n) and n >= 0.0, do: trunc(n)

  defp clamp_coord(n) when is_binary(n) do
    case Integer.parse(n) do
      {i, _} when i >= 0 -> i
      _ -> 0
    end
  end

  defp clamp_coord(_), do: 0

  # Route an `{:object_changed, pid, …}` push to every open floating window
  # whose watched head is that pid (a non-frozen, currently-watching
  # window). Each such window coalesces its own burst via its
  # `:refresh_pending` flag and schedules a per-window deferred refresh.
  # Windows watching a different pid (or frozen) are untouched. Returns the
  # updated socket.
  defp notify_windows_changed(socket, pid) do
    {windows, scheduled} =
      Enum.map_reduce(socket.assigns.windows, false, fn w, sched ->
        cond do
          w.frozen or not watched_pid?(w.watch, pid) ->
            {w, sched}

          w.refresh_pending ->
            {w, sched}

          true ->
            {%{w | refresh_pending: true}, true}
        end
      end)

    if scheduled do
      Process.send_after(self(), {:do_window_refresh, pid}, refresh_debounce_ms())
    end

    assign(socket, :windows, windows)
  end

  # Flip the freeze flag and (un)arm tracking accordingly. Freezing
  # unsubscribes the live object change stream (the pane now holds a
  # snapshot). Unfreezing re-subscribes the current head object and
  # refreshes it so it catches up.
  defp toggle_freeze(%{assigns: %{inspect_frozen: true}} = socket) do
    # Unfreeze: re-arm tracking on the current head term (if any) and catch
    # up. Clear any stale `refresh_pending` too: a timer scheduled before
    # the freeze could otherwise fire a redundant second refresh (double
    # flash) right after this catch-up re-read.
    socket = assign(socket, inspect_frozen: false, refresh_pending: false)

    case head_term(socket) do
      {:beamtalk_object, _c, _m, pid} = term when is_pid(pid) ->
        socket = rearm_watch(socket, term)
        update_docked_pane(socket, &refresh_pane(&1, socket, term))

      _ ->
        socket
    end
  end

  defp toggle_freeze(socket) do
    # Freeze: drop the subscription, keep the current rows/stats as the
    # snapshot.
    socket
    |> update_docked_pane(&unwatch_pane(&1, socket))
    |> assign(inspect_frozen: true)
  end

  # Subscribe the current head object for change pushes without touching the
  # rows (used by unfreeze, which re-reads separately). A non-:ok result
  # leaves the watch nil rather than claiming a live subscription.
  defp rearm_watch(socket, term) do
    case Facade.dispatch(
           :subscribe_object,
           %{term: term, pid: self()},
           RequestContext.build(socket)
         ) do
      :ok -> assign(socket, inspect_watch: term)
      _ -> assign(socket, inspect_watch: nil)
    end
  end

  @doc """
  True when the docked Inspector's head is a live actor handle — the only
  target "Kill actor" applies to (a supervisor, a future or a scalar is not
  killed from here).
  """
  def killable?(assigns) do
    match?(
      %{term: {:beamtalk_object, _, _, pid}} when is_pid(pid),
      List.last(assigns.inspect_crumbs)
    )
  end

  @doc """
  One row of the post-kill actor list: class and pid as the workspace prints
  it, e.g. `Counter <0.123.0>` (the pid term itself if that text is missing).
  """
  def actor_row_label(%{class: class, pid_text: pid_text}), do: "#{class} #{pid_text}"
  def actor_row_label(%{class: class, pid: pid}), do: "#{class} #{inspect(pid)}"
  def actor_row_label(other), do: inspect(other)

  defp kill_inspected_actor(socket) do
    term = head_term(socket)
    label = socket.assigns.inspect_target && socket.assigns.inspect_target.label

    if killable?(socket.assigns) do
      case Facade.dispatch(:kill, %{term: term}, RequestContext.build(socket)) do
        :ok ->
          {:noreply, socket} = handle_event("close_inspector", %{}, socket)
          {:beamtalk_object, _, _, killed_pid} = term

          socket
          |> close_windows_on(killed_pid)
          |> assign(kill_notice: "Killed #{label}.")
          |> refresh_actor_list()

        {:error, reason} ->
          assign(socket, inspect_error: FacadeError.render(reason))
      end
    else
      assign(socket, inspect_error: "Only a live actor can be killed.")
    end
  end

  # Close every floating window on the killed actor — its head, or its root
  # (a window drilled into one of the actor's fields; the reconnect stash keys
  # windows by root, so it would come back as an error window). Like the
  # docked pane, it would otherwise keep showing a dead actor. A window rooted
  # on another, live actor stays open.
  defp close_windows_on(socket, pid) do
    socket.assigns.windows
    |> Enum.filter(fn w ->
      Enum.any?(
        [List.first(w.crumbs), List.last(w.crumbs)],
        &match?(%{term: {:beamtalk_object, _, _, ^pid}}, &1)
      )
    end)
    |> Enum.reduce(socket, fn w, acc -> close_window(acc, w.id) end)
  end

  # Read the running actors (the `actors` facade op) after a kill, so the
  # empty Inspector lists what is still running. A failed read leaves no list
  # rather than a stale one.
  defp refresh_actor_list(socket) do
    case Facade.dispatch(:actors, %{}, RequestContext.build(socket)) do
      {:ok, actors} when is_list(actors) -> assign(socket, actor_list: actors)
      _ -> assign(socket, actor_list: nil)
    end
  end

  # The live term at the current inspector head: the last drill crumb
  # carries it. nil when nothing is inspected.
  defp head_term(socket) do
    case List.last(socket.assigns.inspect_crumbs) do
      %{term: term} -> term
      _ -> nil
    end
  end

  # Send `message` to the inspected actor by eval'ing `<binding> <message>`
  # against the session (the spike's poke). The actor is addressed by its
  # binding name — the head crumb's label — so a poke only makes sense when
  # the head IS a named binding (not a drilled field or the `→ result` of an
  # inspectIt). A successful send renders a terse confirmation and lets the
  # change stream flash the updated field; a failure renders the structured
  # error.
  defp poke_object(socket, message) do
    message = String.trim(message)
    pid = socket.assigns[:session_pid]
    label = poke_target_label(socket)

    cond do
      not is_pid(pid) ->
        assign(socket, poke_result: nil, poke_error: "not attached to workspace")

      message == "" ->
        assign(socket, poke_result: nil, poke_error: "Enter a message to send.")

      is_nil(label) ->
        assign(socket,
          poke_result: nil,
          poke_error: "Can only send to a bound object — inspect a binding to poke it."
        )

      true ->
        send_poke(socket, pid, label, message)
    end
  end

  defp send_poke(socket, pid, label, message) do
    code = "#{label} #{message}"
    crumbs = socket.assigns.inspect_crumbs

    # BT-5189: fold the (possibly queued) result only while the docked
    # Inspector still shows the object it was sent to — navigation meanwhile
    # resets the pane, and the result must not appear against another object.
    BtAttachWeb.Live.Dock.eval_async(
      socket,
      pid,
      code,
      fn socket, result ->
        if socket.assigns.inspect_crumbs == crumbs,
          do: fold_poke(socket, result),
          else: socket
      end,
      fn socket, reason ->
        if socket.assigns.inspect_crumbs == crumbs,
          do: assign(socket, poke_result: nil, poke_error: reason),
          else: socket
      end
    )
  end

  defp fold_poke(socket, result) do
    case result do
      {:ok, term, _output, _warnings} ->
        socket
        |> assign(poke_result: "→ #{Workspace.render_term(term)}", poke_error: nil)
        # A poke is a user-initiated mutation — re-read the inspected object
        # now so its fields reflect the write immediately, rather than
        # waiting on the async `{:object_changed, …}` change-stream push
        # (which is coalesced/delayed, and is the live-tracking path tracked
        # separately by BT-2524). No-op when the pane is frozen or nothing is
        # watched.
        |> refresh_poked_inspector()

      {:error, _, _, _} = err ->
        assign(socket, poke_result: nil, poke_error: FacadeError.render_eval_error(err))

      {:error, _} = err ->
        assign(socket, poke_result: nil, poke_error: FacadeError.render_eval_error(err))
    end
  end

  # Re-read the inspected object after a successful poke so the pane
  # reflects the mutation synchronously. Only when an object is actively
  # watched (a live, non-frozen pid-backed head) — a frozen pane holds its
  # snapshot, and a non-object head has nothing to re-read.
  defp refresh_poked_inspector(%{assigns: %{inspect_watch: term}} = socket)
       when not is_nil(term) do
    update_docked_pane(socket, &refresh_pane(&1, socket, term))
  end

  defp refresh_poked_inspector(socket), do: socket

  # The binding name to address a poke to. A poke eval's `<receiver>
  # <message>` against the session, so the receiver must be a
  # *source-addressable* name — a session binding. That holds only at the
  # inspection root (a single crumb whose label IS the binding the
  # inspection started from): once you drill into a field the head is a
  # referenced object with no session binding to name, and an inspectIt `→
  # result` has no name at all. So poke is offered only when there's a
  # single crumb with a valid-identifier label; otherwise nil (the bar
  # reports it can't send, and the markup can hide it). This keeps the eval
  # well-formed and honest rather than sending to a name that isn't bound.
  defp poke_target_label(socket), do: poke_label(socket.assigns)

  # Whether the current Inspector head can be poked: a pid-backed object at
  # the inspection root with a valid-identifier binding name. Takes the bare
  # `assigns` so the render template can gate the poke bar with it directly.
  # Public: called from `WorkspaceLive`'s docked-panel template.
  def pokeable?(assigns), do: poke_label(assigns) != nil

  # The session-binding name to address a poke to, or nil (see
  # `poke_target_label`).
  defp poke_label(assigns) do
    case assigns[:inspect_crumbs] do
      [%{label: label}] when is_binary(label) ->
        if valid_receiver?(label), do: label, else: nil

      _ ->
        nil
    end
  end
end

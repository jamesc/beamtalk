# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLive do
  @moduledoc """
  LiveView IDE (BT-2407 Wave 1, BT-2408 Wave 2): the four-pane workspace core on
  the Attach topology (ADR 0017 Phase 3, validated by the BT-2394 spike).

    * Workspace pane   — input evaluates Beamtalk source against the attached
      workspace node via the BT-2399 term-returning op layer, rendering the
      result term (surface-consistent with the Phase-1 browser workspace).
    * Transcript pane  — subscribes via the BT-2399 subscription facade and
      renders live `Transcript show:` output pushed over distribution.
    * Bindings pane (Wave 2) — lists the session's current bindings via the
      read-surface (`Workspace.list_bindings/1`, ADR 0085) and refreshes live on
      the BT-2399 `bindings` push stream (a `{:bindings_changed, _}` signal),
      never by polling or a direct gen_server cast.
    * Inspector pane (Wave 2) — inspects a selected binding/value through the
      read-surface `inspect` op (ADR 0085), rendering the **live term's**
      structured fields. Object-valued slots are themselves live
      `{:beamtalk_object, …}` handles, so the Inspector can *follow references* —
      drill from a binding into a referenced object and into its object fields,
      the whole point of carrying terms (not JSON) to the LiveView.
    * Session lifecycle — each LiveView mount gets its own workspace-supervised
      session; eval state (bindings, loaded classes) persists across evals
      within the session and is released when the LiveView process terminates.

  All workspace interaction goes through `BtAttach.Workspace`, which talks to
  the workspace node via Erlang distribution + `:rpc`. The data path carries
  live Erlang terms; JSON lives only at the browser WebSocket edge.

  ## Cockpit shell (BT-2484, epic BT-2482 Phase 1)

  `render/1` is the cockpit shell: a 46px top bar over a three-column grid —
  System Browser (286px) | editor + Workspace dock | Bindings + Inspector
  (348px) — ported from `spikes/cockpit-ux-spike/`. Theme/accent/syntax are all
  driven by CSS variables (`assets/css/app.css`) so a later Tweaks panel can
  re-skin the IDE by toggling `data-theme` on the document. Phase 1 lays the
  shell + theming foundation; the System Browser, Workspace, Bindings, and
  Inspector panes build on the placeholder regions (`#system-browser`,
  `#workspace-dock`, `#bindings-panel`, `#inspector-panel`, `#method-editor`)
  in later Phase 1 issues. The behaviour (events, assigns, term rendering) is
  unchanged — this issue re-skins markup. (The standalone `#changes-panel` /
  `#transcript-panel` footer regions were folded into the tabbed Workspace dock
  in BT-2490 — see below.)

  ## Bindings + Inspector restyle (BT-2486, epic BT-2482 Phase 1)

  `#bindings-panel` and `#inspector-panel` are restyled to the spike design
  (`spikes/cockpit-ux-spike/inspector.jsx`), preserving the existing
  `inspect`/`drill` behaviour and `render_term`-based value rendering:

    * Bindings render as a spike `obj-list` — one `obj-row` per binding showing
      `name := printString` with a type/kind chip (`term_kind/1`). Object-valued
      rows are clickable (the existing `inspect` event by name) and keep the
      explicit "Inspect →" affordance.
    * The Inspector head shows the live `printString`, class/pid type chips
      (`proc-chips`), and a reference-following drill breadcrumb (`insp-crumbs`,
      assign `:inspect_crumbs`) — clicking an earlier crumb (`crumb` event)
      re-inspects that level via the same read-surface path. Fields render in the
      spike `ivar-table`; object-valued slots are `drillable` rows carrying a
      `follow →` link that fires the existing `drill` event.

  Phase-1 scope deliberately excluded the spike's live-tracking affordances
  (field-flash, freeze/snapshot, pid stats, message poke); those land in
  **Phase 3** below (BT-2489 backend + BT-2492 wiring).

  ## Inspector live tracking (BT-2492, epic BT-2482 Phase 3)

  The docked Inspector follows the inspected actor *live*, wiring the BT-2489
  backend (ADR 0095 §5) onto the BT-2486 pane:

    * **Field flash** — inspecting a pid-backed object subscribes this LiveView to
      its per-object change stream (`subscribe_object`); each committed state
      write pushes `{:object_changed, …}`, on which the pane re-reads the object's
      fields + pid stats and bumps `:flash_gen`. The `FieldFlash` JS hook
      (`assets/js/hooks/field_flash.js`) pulses only the value cells that changed.
      A burst of writes is **coalesced** server-side (a single deferred re-read per
      burst via `:refresh_pending`) so a hot actor pulses once per refresh — no
      flash storm.
    * **Pid-stats chips** — the head carries live process metrics (status, mailbox
      depth, reductions) from the `pid_stats` read op, refreshed on every change
      push: the spike's process-health line.
    * **Freeze toggle** — the `insp-freeze` head button drops the change
      subscription (holding the current snapshot) and re-subscribes + catches up on
      unfreeze, the spike's live/frozen tell.
    * **Owner poke** — an owner-only "send a message" bar eval's `<binding>
      <message>` against the inspected actor (the existing `eval` facade op, so no
      new server op and the existing RBAC gate applies — an Observer's poke is
      refused and the bar is hidden for them). The change stream then flashes the
      field the message mutated.

  The watch is dropped on navigate-away (re-inspect rebinds onto the new term),
  on freeze, and in `terminate/2`, so the workspace never pushes to a stale pane.

  ## JS hook foundation (BT-2485, epic BT-2482 Phase 1)

  The cockpit's client-side behaviour rides these LiveView JS hooks, registered
  on the `LiveSocket` in `assets/js/app.js` and referenced via `phx-hook`:

    * `CmEditor` — the cockpit's code editor (BT-2538, BT-2539): a CodeMirror 6
      editor mounted into an ignored host, highlighting Beamtalk with a regex
      tokenizer (`assets/js/hooks/bt_highlight.js`). It powers BOTH the Workspace
      eval input (`#workspace-editor-overlay`, field `expr`) and the tabbed
      method editor (`#method-editor-overlay-<tab>`, field `source`). CodeMirror
      owns the text/selection/history; the hook mirrors its doc into a hidden
      `<textarea>` (the posted form field, kept `phx-update="ignore"` so a
      re-render can't revert it) and, where `data-select-event` is set, reports
      the selection (`{text, start, end}`) — `select_workspace` →
      `:ws_selection` for the eval input, `select_source` → `:edit_selection`
      for the method editor. Token spans reuse the `.tok-*` classes below, so the
      themed `--t-*` CSS variables still drive the colours. This retired the old
      transparent-textarea-over-`<pre>` overlay (CodeEditor) and the separate
      SelectionTracker hook, both folded into CmEditor.
    * `KeyboardShortcuts` — maps Cmd/Ctrl chords to actions from a
      `data-shortcuts` JSON map. The method-editor form binds ⌘S → `submit`
      (request-submits the form so class/selector/source ride the normal
      `save_method` `phx-submit`); the Workspace dock binds ⌘D/⌘P/⌘I →
      `submit:<action>` (BT-2490), riding the eval form's hidden `action` field.

  ## Tweaks panel (BT-2487, epic BT-2482 Phase 1)

  A top-bar gear opens the **Tweaks** dropdown (`tweaks_panel/1`, toggled by
  `:show_settings`) — the cockpit's appearance controls, ported from the spike
  (`spikes/cockpit-ux-spike`): a
  theme picker (paper/squeak/dusk), accent swatches, syntax-palette mode
  (warm/mono/vivid), density (cozy/compact), and UI-font + code-font dropdowns.
  It is **pure presentation** — the `TweaksPanel` JS hook
  (`assets/js/hooks/tweaks_panel.js`) reads each control's `data-tweak`, flips
  the matching `:root` CSS variable that BT-2484 defined (`data-theme`,
  `data-density`, `--ui-font`, `--code-font`, `--accent`, the syntax `--t-*`
  palette), and persists to `localStorage`, so no change round-trips to the
  server and the panel carries no socket state beyond the static defaults.

  ## Workspace dock (BT-2490, epic BT-2482 Phase 1)

  The center-bottom region (`#workspace-dock`) is the spike's **tabbed dock**
  (`spikes/cockpit-ux-spike/app.jsx`), merging the previously-separate eval area,
  REPL, Transcript, and Changes panes into one panel switched by `dock_tab` (held
  in `:dock_tab`, default `"workspace"`):

    * **Workspace** — a CodeMirror code editor (the BT-2538 `CmEditor` hook,
      which also reports selection) wrapped in the eval `<form>` (`#eval-form`,
      `phx-submit="eval"`, field `expr`), with three actions that ride the same
      submit via an `action` field: **Do it** (⌘D — evaluate for side effects),
      **Print it** (⌘P — evaluate and show the result term), and **Inspect it**
      (⌘I — evaluate and open the result in the Inspector). All three reuse the
      existing `eval` op + `render_term`; inspectIt reuses `inspect_term/4`. They
      evaluate the editor's tracked selection if there is one (the `CmEditor`
      hook's selection report → `select_workspace` → `:ws_selection`, kept
      separate from the method editor's `:edit_selection`), else the whole buffer.
    * **REPL** (BT-2543) — the conversational, line-at-a-time sibling of the
      Workspace: a classic TUI request→response scrollback (the `:repl` stream)
      above a bottom-pinned CodeMirror composer (the `ReplInput` hook, `#repl-form`,
      `phx-submit="repl_eval"`, field `expr`). Submitting shares the SAME `eval`
      op + session + `render_term` as the Workspace — it only differs in
      presentation: each submit appends a `› request` / `→ response` pair rather
      than inserting inline. Enter submits (terminal convention, confirmed BT-2543
      divergence from the Workspace newline); ↑/↓ recall the `:repl_history` ring
      at the composer's edges; each `→ response` keeps an Inspect affordance into
      the Inspector (the term is stashed in `:repl_terms`). Ambient `Transcript
      show:` output streams to the Transcript pane, never duplicated here.
    * **Transcript** — the live `Transcript show:` stream (`#transcript`,
      `phx-update="stream"`), wired via the BT-2399 subscription facade, unchanged.
    * **Changes** — the workspace ChangeLog viewer (`Workspace changes`, ADR 0082).

  The eval form stays the FIRST `<form>` on the page so the e2e tests'
  `form("#eval-form")` (and `form("form")`) resolve to it; the Method Editor form
  follows. A plain submit (no `action`) defaults to printIt — the historical eval
  behaviour the BT-2407/2408/2410 tests assert on.

  ## Tabbed method editor (BT-2494, epic BT-2482 Phase 2)

  The center `#method-editor` panel is the spike's **tabbed write-surface**
  (`spikes/cockpit-ux-spike/app.jsx`, ADR 0082): a tab strip over a breadcrumb
  over the CmEditor CodeMirror editor (BT-2539). The open-tab list is `:tabs` (each a map
  with `id`, `kind` (`:method | :def`), `class`, `side`, `selector`, `source`,
  `base`, `dirty`) and the focused id is `:active_tab`:

    * **Tab strip** — one tab per open method *or* class definition, each with a
      `.modot` dirty dot (unsaved edits) and a close `×` (the strip always keeps
      ≥1 tab). A `+ def` affordance opens (or re-focuses) the active class's
      definition tab. `tab_select` / `tab_close` / `open_definition` are pure
      view state — no workspace round-trip.
    * **Compile (⌘S)** — the single `save_method` form is preserved verbatim
      (`id`, `phx-submit="save_method"`, ⌘S via `KeyboardShortcuts`, the
      `class`/`selector`/`source` fields) so the BT-2409 e2e flows keep working.
      The active tab id rides as a hidden `tab` field; `save_method/5` reads the
      tab's *kind* (not the payload shape) to route: a method tab drives the
      write-surface `save` op (compile + flush, ADR 0082), a class-definition tab
      `eval`s its whole definition (compiling the class) — neither invents a new
      server op. The historical no-tab payload (`tab` absent) takes the method
      path unchanged.
    * **Dirty tracking** — the CmEditor hook mirrors its doc into the hidden
      `source` textarea and fires `input`, driving the form's `phx-debounce`d
      `phx-change="edit_source"`; `track_edit/2` flips the active tab's `dirty`
      when its `source` diverges from the last-compiled `base`, and a successful
      compile (`compile_clean/3`) clears the dot + re-bases. We do NOT echo the
      live value back (the textarea is `phx-update="ignore"`, so CodeMirror owns
      the text); switching tabs re-keys the element on `@active_tab`, remounting
      the editor with the new tab's source.
    * **Breadcrumb** — `Class › side › selector` for the active tab (a class
      definition shows `Class › class definition`).

  ## System Browser pane (BT-2491, epic BT-2482 Phase 2)

  The left column is the spike's **System Browser** (`spikes/cockpit-ux-spike`),
  replacing the BT-2484 placeholder. It is the four-pane Smalltalk navigator —
  *classes → protocols → selectors → method source* — driven entirely by the
  BT-2488 browse ops (ADR 0096) through the read-only facade (`browse_classes` /
  `browse_protocols` / `browse_method_source`, all `:read` capability), so the
  pane works for the Observer role too:

    * **Class tree** — two toggleable views (`browser_view`): **Hierarchy**
      (indented by superclass depth, `hierarchy_rows/1`) and **Category**
      (grouped by the class annotation, `category_groups/1`). Selecting a class
      (`browser_select_class`) fetches its protocols for the current side.
    * **Instance / class side** toggle (`browser_side`) at the pane footer
      re-populates the protocol + method list (a class's instance methods differ
      from its class methods — a fresh `browse-protocols` fetch per side).
    * **Protocol + method list** — selectors grouped by protocol, with a filter
      row (`browser_select_protocol`; `nil` = "all" shows every selector,
      `filtered_methods/2`). Runtime-only (image-diverged) classes and methods
      carry a `runtime` badge (origin = `runtime`, ADR 0096 / BT-2483).
    * **Method source** — selecting a method (`browser_select_method`) opens it
      as an editable tab in the centre method editor (`open_method_tab`), seeded
      with its image-accurate source. Browsing is editing (the Smalltalk idiom):
      there is no separate read-only display, and the browser highlights whichever
      method the focused editor tab is showing (`selected_method_ref`).

  The browse data path carries the wire-shaped live term verbatim over
  distribution (`{:value, json_value}`, BT-2399) — JSON only at the WebSocket
  edge, never re-serialised here. A dispatch failure / RBAC denial renders a
  `browser_error` rather than crashing the pane.
  """
  use BtAttachWeb, :live_view

  require Logger

  alias BtAttach.Facade
  alias BtAttach.SessionRegistry
  alias BtAttach.Workspace

  # All RBAC-relevant workspace ops go through the curated facade (ADR 0091
  # Decision 3) — never a raw Workspace/:rpc call from an event handler. Pure
  # transport/display/lifecycle helpers (connect, render_term, session start)
  # go through the injectable workspace client so the mount and session-start
  # paths are testable without a live node (BT-2554). `ctx/1` carries the
  # request identity the facade audits / RBAC gates on (BT-2421).
  defp ctx(socket),
    do: %{user: socket.assigns[:current_user], role: socket.assigns[:role] || :owner}

  defp ws_client, do: Application.get_env(:bt_attach, :workspace_client, Workspace)

  @impl true
  def mount(_params, _session, socket) do
    # LiveView `mount/3` runs TWICE: first on the disconnected HTTP render, then
    # on the connected WebSocket mount (and again on every reconnect). Only the
    # *connected* mount attaches over distribution — the disconnected render must
    # NOT create a workspace session, or every page load would leak an orphaned
    # one. The per-tab resume token is only present on the connected mount (it
    # rides the LiveSocket `params`), so `get_connect_params/1` is the right read.
    # The page title flows into the root layout `<.live_title>` and is the
    # canonical "Beamtalk Workspace" string the HTTP-render tests assert on
    # (rbac_web/oidc_flow/session_lifecycle). Set it on BOTH mounts so the
    # disconnected render carries it too.
    socket = assign(socket, :page_title, "Beamtalk Workspace")

    if connected?(socket) do
      token = connect_token(socket)
      attach(assign(socket, :token, token))
    else
      {:ok, assign(socket, connected: false, error: nil, token: nil)}
    end
  end

  # The per-tab token minted in `sessionStorage` (assets/js/app.js) and replayed
  # on every (re)connect. `get_connect_params/1` is only available on the
  # connected mount; a non-binary/absent value just disables resume (each connect
  # gets a fresh, non-resumable session) rather than crashing.
  defp connect_token(socket) do
    case get_connect_params(socket) do
      %{"workspace_token" => token} when is_binary(token) -> token
      _ -> nil
    end
  end

  defp attach(%{assigns: %{token: token}} = socket) do
    socket =
      case ws_client().connect() do
        :ok ->
          # Resume the tab's existing session if the registry still holds a live
          # one (reconnect within the grace window); otherwise start fresh.
          case resume_or_start(token, session_meta(socket)) do
            {:ok, session_id, pid, origin} ->
              socket
              |> bind_session(session_id, pid)
              |> restore_windows(token, origin)
              |> restore_doc(token, origin)

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
        if Workspace.session_alive?(pid) do
          {:ok, session_id, pid, :resumed}
        else
          # Stale entry (workspace restarted): discard it and start fresh.
          SessionRegistry.discard(token)
          start_fresh(token, meta)
        end

      _miss ->
        start_fresh(token, meta)
    end
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
    with :ok <- Facade.dispatch(:subscribe_transcript, %{pid: self()}, ctx(socket)),
         :ok <- Facade.dispatch(:subscribe_bindings, %{pid: self()}, ctx(socket)) do
      # BT-2598: subscribe to the class-lifecycle push stream so any source change
      # — a git revert's disk→image reload, another session's flush, an external
      # edit reloaded into the image — pushes a `ClassLoaded`/`ClassRemoved`
      # refresh trigger to this LiveView. Best-effort: unlike the transcript /
      # bindings streams the cockpit can still function without it (open windows
      # simply fall back to the re-activation re-read), so a subscribe failure is
      # logged but does not tear down the session.
      subscribe_classes_best_effort(socket)

      socket
      |> assign(:connected, true)
      |> assign(:node, Workspace.node_name())
      |> assign(:session_id, session_id)
      |> assign(:session_pid, pid)
      |> assign(:result, nil)
      # `eval_seq` re-keys the transient `.eval-status` line (BT-2542) so its
      # fade animation restarts on every eval — the status self-clears visually
      # while staying in the DOM (the value is still assertable in tests).
      |> assign(:eval_seq, 0)
      |> assign(:output, nil)
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
      # BT-2597: a run/load is in flight on the workspace node. Set while the
      # `:test_op` async task runs so the run/load controls disable themselves —
      # `phx-disable-with` alone reverts as soon as the (now-immediate) event
      # handler re-renders, since the work moved off-socket to `start_async`.
      |> assign(:tests_running, false)
      # BT-2599: a transient flag for the off-socket `:test_discover` catalogue
      # discovery — true only while a partial-load re-discovery is in flight so
      # `handle_async(:test_discover, …)` keeps the partial-load `tests_error`
      # banner across a successful re-discovery (see `apply_test_classes/3`).
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
      |> assign(:inspect_target, nil)
      |> assign(:inspect_rows, [])
      |> assign(:inspect_error, nil)
      # Drill breadcrumb (BT-2486): the trail of references followed so far, each
      # carrying the live term so a crumb click re-inspects that level. Reset when
      # inspection starts from a binding, appended to when a field is drilled.
      |> assign(:inspect_crumbs, [])
      # Live Inspector tracking (BT-2492, epic BT-2482 Phase 3): the per-object
      # change subscription + pid stats + freeze toggle + owner poke wired onto
      # the docked Inspector (backend BT-2489, ADR 0095 §5).
      #
      #   * `:inspect_watch` — the live `{:beamtalk_object, …}` term currently
      #     subscribed for `{:object_changed, …}` pushes (nil = nothing watched,
      #     e.g. a scalar target or a frozen pane). Held so we can unsubscribe it
      #     exactly when the target changes / the pane freezes / the LiveView dies.
      #   * `:inspect_stats` — the last `pid_stats` snapshot (binary-keyed metrics)
      #     driving the mailbox/reductions/status chips; nil when none read yet.
      #   * `:inspect_frozen` — when true the pane holds a snapshot: we drop the
      #     change subscription so pushes stop re-reading, and the freeze toggle
      #     re-subscribes + refreshes on unfreeze.
      #   * `:flash_gen` — a monotonic counter bumped on each *live refresh*; it
      #     rides the ivar table as `data-flash-gen` so the FieldFlash JS hook
      #     flashes only the value cells that *changed* this refresh (debounced
      #     client-side, no flash storm). Server-side the `{:object_changed, …}`
      #     push is itself coalesced via `:refresh_pending` so a burst of writes
      #     collapses into one re-read.
      #   * `:poke_result` / `:poke_error` — the owner-only "send a message" quick
      #     action's outcome (an `eval` of `<binding> <message>` against the
      #     inspected actor); rendered under the poke bar.
      |> assign(:inspect_watch, nil)
      |> assign(:inspect_stats, nil)
      |> assign(:inspect_frozen, false)
      |> assign(:flash_gen, 0)
      |> assign(:refresh_pending, false)
      # BT-2600: coalesce `ClassLoaded`/`ClassRemoved` refresh bursts. A project
      # sync / `Workspace load:` reloading N files fires N consecutive pushes;
      # rather than running `refresh_after_source_change` (N source re-reads ×
      # open tabs) per push, the first push schedules one deferred
      # `:do_source_refresh` and sets this flag, collapsing the burst into a
      # single refresh — mirroring the `:refresh_pending` object-change coalescing.
      |> assign(:source_refresh_pending, false)
      |> assign(:poke_result, nil)
      |> assign(:poke_error, nil)
      # Method editor (Wave 3): the write-surface edit/save/flush pane.
      |> assign(:edit_class, "")
      |> assign(:edit_selector, "")
      |> assign(:edit_source, "")
      # Tabbed method editor (BT-2494, epic BT-2482 Phase 2): the spike's
      # write-surface tab strip (ADR 0082). `:tabs` is the ordered open-tab list;
      # `:active_tab` is the id of the focused tab. Each tab carries its own
      # source/base/dirty so switching tabs swaps the whole edit buffer, and a
      # dirty dot per tab tracks unsaved edits (cleared on a successful compile).
      |> init_tabs()
      # Method-editor selection (BT-2485, BT-2539), reported by the CmEditor hook.
      |> assign(:edit_selection, nil)
      # Workspace-editor selection (BT-2490): the dock's own CmEditor hook,
      # kept separate so the method editor's selection can't leak into an eval.
      |> assign(:ws_selection, nil)
      |> assign(:save_result, nil)
      |> assign(:save_error, nil)
      |> assign(:flush_result, nil)
      |> assign(:flush_error, nil)
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
      # default) keeps the single right-column Inspector.
      #
      #   * `:inspector_mode` — `"docked"` | `"float"`. Persisted in LV state so it
      #     survives re-render; the top-bar toggle flips it.
      #   * `:windows` — the open floating windows, each a self-contained inspector
      #     (its own drill `crumbs`, `rows`, `target`, live `watch`/`stats`/`frozen`
      #     + `flash_gen`, and `x`/`y`/`z` placement). The window list (id + drill
      #     stack + content) lives server-side; only x/y/z are client-reported by
      #     the WindowDrag hook on drop / focus, so there is no per-mousemove
      #     round-trip and positions survive an unrelated re-render.
      #   * `:next_window_id` — a monotonic counter minting unique window ids.
      #   * `:window_z` — the running max z-index; a click bumps the focused window
      #     to `window_z + 1` so z-order follows focus (the spike's stacking).
      |> assign(:inspector_mode, "docked")
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
      |> assign(:windows, [])
      |> assign(:next_window_id, 1)
      |> assign(:window_z, 10)
      |> assign_browser_native_modules()
      # BT-2636: the set of expanded Changes-pane rows (keyed by the row's
      # `{class, selector}`), driven by the leading disclosure caret. A row's
      # structured net-vs-disk diff renders beneath it only while its key is in
      # this set; collapsed by default so the table stays compact.
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
      # live push that lands BEFORE the mount load resolves is not clobbered by the
      # (staler) mount snapshot. `:source_loaded` covers the source-dependent pair
      # (browser_classes + changes — they always refresh together via
      # `:do_source_refresh`); `:bindings_loaded` covers the bindings pane. Both
      # start `false` and are set `true` ONLY by a *successful* push refresh (see
      # `:do_source_refresh` / `:BindingChanged`); `handle_async(:mount_load, …)`
      # then applies a surface's mount read only while its flag is still `false`.
      # Success-only gating is the crux of the edge case: an early push that ERRORED
      # leaves the flag `false`, so the later-completing mount load's *successful*
      # data still folds in (no lingering error flash). A remount = new process =
      # fresh `false` flags, so reconnect/resume re-loads normally. `autoflush` has
      # no competing push, so it always folds in (no flag).
      |> assign(:source_loaded, false)
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
      |> stream(:transcript, [])
      |> stream(:repl, [])
    else
      other ->
        Logger.error("subscribe failed: #{inspect(other)}")
        # Drop whichever subscription may have succeeded, then release the
        # session through the registry (which closes it) so we don't leave a
        # dangling subscriber or an orphaned session behind.
        Workspace.unsubscribe_transcript(self())
        Workspace.unsubscribe_bindings(self())
        Workspace.unsubscribe_classes(self())
        force_close(socket.assigns[:token], pid)

        assign(socket,
          connected: false,
          error: "subscribe failed: #{inspect(other)}"
        )
    end
  end

  # BT-2598: best-effort subscribe to the class-lifecycle push stream. A failure
  # (an older workspace without the wiring, a transient dist hiccup) is logged but
  # does not fail the mount — the cockpit degrades to the clean-tab re-read on
  # re-activation rather than a live push, and re-subscribes on the next remount.
  defp subscribe_classes_best_effort(socket) do
    case Facade.dispatch(:subscribe_classes, %{pid: self()}, ctx(socket)) do
      :ok ->
        :ok

      other ->
        Logger.warning("subscribe_classes failed (push refresh degraded): #{inspect(other)}",
          domain: [:beamtalk, :liveview]
        )

        :ok
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

  # The Workspace dock's three actions (BT-2490) all evaluate the entered code (or
  # the tracked selection, the spike's "evaluates selection vs buffer") and differ
  # only in what they do with the result term — so they ride the SAME `eval`
  # facade op and the existing `render_term` formatting rather than inventing new
  # server ops:
  #
  #   * doIt      (⌘D) — evaluate for side effects; show a terse "✓ evaluated".
  #   * printIt   (⌘P) — evaluate and show the result term (the classic eval).
  #   * inspectIt (⌘I) — evaluate, then inspect the *result term* in the Inspector
  #     (reuses `inspect_term/4`, the same read-surface path bindings drill into).
  #
  # The clicked action rides the eval `<form>` submit as the `action` field; a
  # plain submit (or the e2e test's `render_submit(%{expr: …})`) carries no action
  # and defaults to printIt — the historical eval behaviour the tests assert on.
  @impl true
  def handle_event("eval", %{"expr" => expr} = params, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    action = eval_action(params)
    target = eval_target(expr, socket)

    case Facade.dispatch(:eval, %{session_pid: pid, code: target}, ctx(socket)) do
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
      # off-vocabulary op (`:forbidden_op`) — returning a 2-tuple the workspace
      # eval contract never produces. Render it as an actionable message rather
      # than crashing the LiveView on an unmatched case clause.
      {:error, reason} ->
        {:noreply,
         assign(socket, result: nil, output: nil, error: facade_error(reason), expr: expr)}
    end
  end

  # No session (attach failed) — don't crash on a missing assign.
  def handle_event("eval", %{"expr" => expr}, socket) do
    {:noreply,
     assign(socket, result: nil, output: nil, error: "not attached to workspace", expr: expr)}
  end

  # Backend-driven autocomplete (BT-2544): the CodeMirror editors' autocomplete
  # CompletionSource round-trips the current line up to the caret; we answer with
  # ranked candidates from the live session via the `complete` op (receiver-aware:
  # class names, selectors for a known receiver, session bindings, pseudo-vars).
  # `:read` capability — completion runs no user code — so the Observer role
  # completes too. We `{:reply, …}` on the same event so CodeMirror resolves its
  # async source; an unreachable workspace, a denied op, or a missing session all
  # degrade to an empty list (autocomplete simply shows nothing).
  @impl true
  def handle_event("complete", %{"code" => code}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) and is_binary(code) do
    completions =
      case Facade.dispatch(:complete, %{session_pid: pid, code: code}, ctx(socket)) do
        {:ok, list} when is_list(list) -> list
        _ -> []
      end

    {:reply, %{"completions" => completions}, socket}
  end

  def handle_event("complete", _params, socket) do
    {:reply, %{"completions" => []}, socket}
  end

  # Live-image hover (BT-2555): the CodeMirror editors' `hoverTooltip` source
  # round-trips the editor line up to the hovered token; we answer with
  # signature + doc-comment markdown from the live session via the `hover` op
  # (a class name → its docs; a `Receiver selector` pair → that method's docs).
  # `:read` capability — hover runs no user code — so the Observer role hovers
  # too. We `{:reply, …}` on the same event so CodeMirror resolves its async
  # tooltip source; an unreachable workspace, a denied op, a missing session, or
  # nothing-to-show all degrade to an empty string (no tooltip is shown).
  @impl true
  def handle_event("hover", %{"code" => code}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) and is_binary(code) do
    hover =
      case Facade.dispatch(:hover, %{session_pid: pid, code: code}, ctx(socket)) do
        {:ok, docs} when is_binary(docs) -> docs
        _ -> ""
      end

    {:reply, %{"hover" => hover}, socket}
  end

  def handle_event("hover", _params, socket) do
    {:reply, %{"hover" => ""}, socket}
  end

  # Live parse-only diagnostics (BT-2556): the CodeMirror editors'
  # `@codemirror/lint` source round-trips the FULL buffer; we answer with
  # error/warning ranges from the compiler's side-effect-free `diagnostics` path
  # (parse + semantic check, NO codegen / install / eval / ChangeLog), so it is
  # safe to fire as the buffer changes and is a `:read` op — the Observer role
  # sees diagnostics too. We `{:reply, …}` on the same event so CodeMirror
  # resolves its async lint source; an unreachable workspace, a denied op, or a
  # bad shape all degrade to an empty list (no squiggles shown). Diagnostics do
  # not need a session pid — they analyse the buffer in isolation — so unlike
  # `complete`/`hover` there is no `session_pid` guard.
  #
  # Debounce lives on the CLIENT: the `@codemirror/lint` `linter(…, {delay})`
  # only invokes this source after the editor has been idle, so rapid keystrokes
  # never flood the workspace (the same pattern the LSP uses to debounce
  # `didChange`). A server-side *drop* debounce would break the request/reply
  # contract — a coalesced request's reply would never arrive — so the throttle
  # is deliberately upstream of the round-trip, not in this handler.
  @impl true
  def handle_event("diagnostics", %{"code" => code} = params, socket) when is_binary(code) do
    # `mode` (BT-2569) selects the parse grammar. The method-editor CmEditor sends
    # "method" (a bare method body — `=>` is not a valid top-level token, so the
    # default script grammar would false-positive); the Workspace + REPL editors
    # send nothing. Forward the raw client value — the facade normalises it to a
    # known binary ("method" | "expression"), the single boundary for every caller.
    diagnostics =
      case Facade.dispatch(
             :diagnostics,
             %{code: code, mode: Map.get(params, "mode")},
             ctx(socket)
           ) do
        {:ok, list} when is_list(list) -> list
        _ -> []
      end

    {:reply, %{"diagnostics" => diagnostics}, socket}
  end

  def handle_event("diagnostics", _params, socket) do
    {:reply, %{"diagnostics" => []}, socket}
  end

  # Switch the Workspace dock's active tab (Workspace / REPL / Transcript /
  # Changes / Tests, BT-2490, REPL added BT-2543, Tests BT-2557). Pure view state
  # — no workspace round-trip — EXCEPT the Tests tab lazily loads its test
  # catalogue the first time it is opened (discovery is a cheap `:read` op, but
  # there is no point running it for users who never open the tab). An unknown
  # tab is ignored rather than rendered, so a crafted value can't blank the dock.
  @impl true
  def handle_event("dock_tab", %{"tab" => "tests"}, socket) do
    {:noreply, socket |> assign(dock_tab: "tests") |> ensure_test_classes()}
  end

  # ADR 0082 Amendment 1 (BT-2586): opening the Git tab refreshes the panel from
  # real git on the workspace project root (lazy first-open, like Tests).
  def handle_event("dock_tab", %{"tab" => "git"}, socket) do
    {:noreply, socket |> assign(dock_tab: "git") |> assign_git()}
  end

  def handle_event("dock_tab", %{"tab" => tab}, socket)
      when tab in ~w(workspace repl transcript changes) do
    {:noreply, assign(socket, dock_tab: tab)}
  end

  def handle_event("dock_tab", _params, socket), do: {:noreply, socket}

  # ── Git panel events (ADR 0082 Amendment 1, BT-2586) ─────────────────────────
  # Read events (refresh, diff) are available to every role; the mutating events
  # (stage/unstage/commit/revert) are gated by the Facade `:execute` capability
  # *and* hidden in the template for the Observer role.

  def handle_event("git_refresh", _params, socket) do
    {:noreply, assign_git(socket)}
  end

  # Toggle the inline diff for one path: a second click on the same path closes it.
  def handle_event("git_diff", %{"path" => path}, socket) when is_binary(path) do
    if socket.assigns.git_diff_path == path do
      {:noreply, assign(socket, git_diff_path: nil, git_diff: nil)}
    else
      case Facade.dispatch(:git_diff, %{path: path}, ctx(socket)) do
        {:ok, diff} when is_map(diff) ->
          {:noreply, assign(socket, git_diff_path: path, git_diff: diff, git_error: nil)}

        {:error, reason} ->
          {:noreply, assign(socket, git_error: facade_error(reason))}
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

  # Malformed git payloads (missing key / non-binary value): surface a git-panel
  # validation error rather than letting a crafted WebSocket event crash the
  # LiveView, consistent with the `new_class` / `revert` fallbacks above.
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

  # ── Test-runner pane (BT-2557) ───────────────────────────────────────────────
  #
  # The GUI equivalent of a Smalltalk Test Runner: a dock tab that lists the live
  # image's `TestCase` subclasses, runs all or a selected class through the
  # attached session (never a shelled-out `beamtalk test`), and shows per-case
  # pass/fail with failure detail — with an affordance to open a failing method
  # in the method editor. Discovery is a `:read` op (the Observer may browse the
  # catalogue); running tests is `:execute` (Owner-only, it evaluates code), so
  # the run controls are owner-gated in the template, mirroring the eval form.

  # Re-discover the test catalogue (the "refresh" affordance). The discovery is a
  # `:read` reflection op, but it is still a blocking workspace RPC — so it runs
  # off-socket via `discover_test_classes/1` (`:test_discover` `start_async`,
  # BT-2599) rather than stalling the LiveView process against a slow node. We
  # reset `test_classes` to the nil sentinel so the pane shows its "discovering"
  # state (not the misleading "No TestCase subclasses" empty-state) until the
  # `handle_async(:test_discover, …)` fold resolves.
  @impl true
  def handle_event("tests_refresh", _params, socket) do
    {:noreply, socket |> assign(:test_classes, nil) |> discover_test_classes()}
  end

  # Run every loaded TestCase subclass (`test-all`).
  def handle_event("run_tests", _params, socket) do
    {:noreply, run_tests(socket, nil)}
  end

  # Load the project's test/ files into the live image, then re-discover the
  # catalogue (`load_tests`, `:execute` — Owner-only). A freshly-opened project
  # holds only src/ classes, so without this the catalogue is empty (BT-2557).
  def handle_event("load_tests", _params, socket) do
    {:noreply, load_tests(socket)}
  end

  # Run a single selected test class (`test`, `class` = the row's class).
  def handle_event("run_test_class", %{"class" => class}, socket) when is_binary(class) do
    {:noreply, run_tests(socket, class)}
  end

  # Open a (failing) test method in the method editor. Test selectors are
  # instance-side, so the side is always "instance"; reuses the System Browser's
  # method-tab opener (BT-2491) so the test runner and browser share one editor.
  def handle_event("open_test_method", %{"class" => class, "selector" => selector}, socket)
      when is_binary(class) and is_binary(selector) do
    {:noreply, open_method_tab(socket, class, "instance", selector)}
  end

  # Fallback clauses for the guarded test handlers: a crafted WebSocket message
  # with a missing / non-binary `class`/`selector` must be ignored, not crash the
  # socket on a FunctionClauseError before RBAC is reached (matching `save_method`,
  # `revert`, `browser_select_class`, etc.).
  def handle_event("run_test_class", _params, socket), do: {:noreply, socket}
  def handle_event("open_test_method", _params, socket), do: {:noreply, socket}

  # ── REPL tab (BT-2543) ───────────────────────────────────────────────────────
  #
  # The REPL is the *conversational, line-at-a-time* idiom (distinct from the
  # editor-primary Workspace and the ambient-log Transcript): a request→response
  # scrollback with the input pinned at the bottom. Submitting shares the SAME
  # `eval` facade op + session as the Workspace — same structured result, same
  # surface-shared `render_term` display rules — and only differs in presentation:
  # each submit appends a `› request` / `→ response` pair to the `:repl` stream
  # instead of inserting inline. Ambient `Transcript show:` output keeps streaming
  # to the Transcript tab over the existing subscription; it is NOT duplicated
  # into the scrollback here. Per the BT-2543 confirmation, Enter submits in the
  # REPL (terminal convention) while Shift/⌘-Enter inserts a newline — the
  # divergence is enforced by the ReplInput hook, not here.
  @impl true
  def handle_event("repl_eval", %{"expr" => expr}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    trimmed = String.trim(expr)

    cond do
      trimmed == "" ->
        # Empty submit (bare Enter) is a no-op — never append a blank entry or
        # disturb the history cursor.
        {:noreply, socket}

      meta = repl_meta_command(trimmed) ->
        # A `:`-prefixed meta-command (BT-2543 follow-up). The IDE handles these
        # itself — driving the matching pane or pointing at it — and NEVER sends
        # them to `eval`, which would choke trying to compile `:h` as Beamtalk.
        {:noreply,
         socket
         |> handle_repl_meta(meta, expr)
         |> repl_record_history(expr)
         |> repl_clear_input()}

      true ->
        socket =
          case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, ctx(socket)) do
            {:ok, term, _output, _warnings} ->
              socket |> repl_append_ok(expr, term) |> repl_help_followup(expr)

            {:error, reason, _output, _warnings} ->
              repl_append_error(socket, expr, Workspace.render_error(reason))

            # Facade short-circuit (RBAC denial / off-vocabulary op) — a 2-tuple the
            # eval contract never produces; render it as the entry's response rather
            # than crashing the LiveView.
            {:error, reason} ->
              repl_append_error(socket, expr, facade_error(reason))
          end

        {:noreply, socket |> repl_record_history(expr) |> repl_clear_input()}
    end
  end

  # No session — the first clause's `is_pid(pid)` guard only fails when
  # bind_session never ran, so the REPL stream + assigns (`:repl_seq`,
  # `:repl_terms`, `:repl_history`, `:repl_history_pos`) don't exist and the pane
  # isn't rendered. A crafted `repl_eval` during the attach-failure window must
  # NOT touch the REPL helpers (which read those assigns) — that would KeyError /
  # crash the LiveView. Guard on the assigns' presence and no-op when absent,
  # otherwise surface the "not attached" entry (the defensive path for any future
  # state where the assigns exist but the session pid is gone).
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
  # composer's edges (↑ on the first line, ↓ on the last), so mid-buffer cursor
  # navigation is untouched. `repl_history_pos` walks the most-recent-first ring:
  # ↑ moves further back (toward older entries), ↓ moves toward the present and
  # past the newest restores the empty live input. The recalled text is pushed to
  # the hook (the input is hook-owned / phx-update=ignore, so the server can't set
  # it through morphdom).
  @impl true
  def handle_event("repl_history_prev", _params, socket) do
    {:noreply, repl_recall(socket, :prev)}
  end

  def handle_event("repl_history_next", _params, socket) do
    {:noreply, repl_recall(socket, :next)}
  end

  # Inspect a `→ result` term in the Inspector (BT-2543): results stay live
  # objects even in the terminal idiom. The term was stashed server-side under the
  # entry id at append time (the scrollback DOM is display-only); look it up and
  # drive the same `inspect_term` path bindings/Print-it use. In `"float"` mode
  # (BT-2493) it opens a floating window instead of the docked pane. An unknown id
  # (a stale entry after a reconnect dropped the term map) is ignored.
  @impl true
  def handle_event("repl_inspect", %{"id" => id}, %{assigns: %{repl_terms: terms}} = socket) do
    case Map.fetch(terms, id) do
      {:ok, term} ->
        label = "REPL result"

        socket =
          if socket.assigns.inspector_mode == "float" do
            open_window_for_term(socket, label, term)
          else
            inspect_term(socket, label, term, [%{label: label, term: term}])
          end

        {:noreply, socket}

      :error ->
        {:noreply, socket}
    end
  end

  def handle_event("repl_inspect", _params, socket), do: {:noreply, socket}

  # Inspect a binding by name: look up its live term and drill into it via the
  # read-surface `inspect` op. Reference-following starts here. In `"float"` mode
  # (BT-2493) the click opens a *new floating window* on the binding instead of
  # driving the docked pane; in `"docked"` mode it drives the docked Inspector as
  # before. Docked is the default, so the existing single-pane flow is untouched
  # unless the user flipped the top-bar Dock/Float toggle.
  @impl true
  def handle_event("inspect", %{"name" => name}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    if socket.assigns.inspector_mode == "float" do
      {:noreply, open_window(socket, pid, name)}
    else
      {:noreply, inspect_binding(socket, pid, name)}
    end
  end

  # Drill into an object-valued field of the currently-inspected object: the
  # field term is itself a live `{:beamtalk_object, …}` handle, so following it
  # is the same read-surface inspect call one level deeper. The clicked field's
  # term is carried back to the server by index against the current rows, so we
  # never round-trip a flattened string.
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

  # Jump back to an earlier level of the drill breadcrumb (BT-2486): truncate the
  # trail at the clicked crumb and re-inspect that level's live term. Defensive
  # against a client-supplied index that no longer maps to a crumb.
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
  # toggle, inspector.jsx). Freezing drops the per-object change subscription so
  # the pane holds the snapshot it has — no more field-flash, no re-reads.
  # Unfreezing re-subscribes the *current* head object and refreshes its fields +
  # stats immediately so the pane catches up to the live state. A toggle with no
  # object inspected (or a scalar head) just flips the flag.
  def handle_event("freeze_toggle", _params, socket) do
    {:noreply, toggle_freeze(socket)}
  end

  # Owner-only "send a message" quick action (the spike's PokeBar / quick-pokes,
  # inspector.jsx). Sends the typed Beamtalk message to the inspected actor by
  # eval'ing `<binding> <message>` against the workspace session — the same `eval`
  # facade op the Workspace dock uses, so poke invents no new server op and rides
  # the existing RBAC gate (an Observer's `eval` is refused; the bar is also
  # owner-gated in the markup). The object is addressed by its *binding name* (the
  # head crumb / target label), which is the live handle's source-level name; a
  # drilled field with no binding name can't be poked, so we say so.
  def handle_event("poke", %{"message" => message}, socket) when is_binary(message) do
    {:noreply, poke_object(socket, message)}
  end

  def handle_event("poke", _params, socket), do: {:noreply, socket}

  # ── floating inspector windows (BT-2493, epic BT-2482 Phase 3) ───────────────

  # Flip the Inspector between docked and floating ("overlay") modes (the spike's
  # Dock/Float toggle). Switching to docked leaves the open windows alone — they
  # stay in state and reappear if the user flips back — so a misclick doesn't tear
  # down a desk full of inspectors and their subscriptions. Only an explicit
  # window close releases a window's watch.
  def handle_event("set_inspector_mode", %{"mode" => mode}, socket)
      when mode in ~w(docked float) do
    {:noreply, assign(socket, inspector_mode: mode)}
  end

  def handle_event("set_inspector_mode", _params, socket), do: {:noreply, socket}

  # Open / close the top-bar appearance (Tweaks) dropdown. Pure view state — the
  # panel itself stays mounted (see `:show_settings` in `mount/3`); this only
  # flips the dropdown's visibility. `close_settings` is the click-away / Escape
  # path.
  def handle_event("toggle_settings", _params, socket) do
    {:noreply, assign(socket, show_settings: !socket.assigns.show_settings)}
  end

  def handle_event("close_settings", _params, socket) do
    {:noreply, assign(socket, show_settings: false)}
  end

  # Panel visibility toggles (BT-2559): close/toggle side panels and dock.
  def handle_event("toggle_browser", _params, socket) do
    {:noreply, assign(socket, show_browser: !socket.assigns.show_browser)}
  end

  def handle_event("toggle_inspector", _params, socket) do
    {:noreply, assign(socket, show_inspector: !socket.assigns.show_inspector)}
  end

  def handle_event("toggle_dock", _params, socket) do
    {:noreply, assign(socket, show_dock: !socket.assigns.show_dock)}
  end

  # Expand/collapse the method-editor doc block (BT-2558). The signature stays
  # visible either way; this only reveals/hides the rendered `///` doc body so it
  # doesn't permanently occupy the top of the editor. Sticky across tab switches —
  # one preference, not per-method — so a user who wants docs open keeps them open,
  # and sticky across reconnects too (BT-2570): `terminate/2` stashes this flag in
  # the registry and `restore_doc/3` re-applies it on the resuming mount, so a
  # socket drop / redeploy / laptop wake no longer re-collapses an expanded block.
  def handle_event("toggle_doc", _params, socket) do
    {:noreply, assign(socket, doc_expanded: !socket.assigns.doc_expanded)}
  end

  def handle_event("close_browser", _params, socket) do
    {:noreply, assign(socket, show_browser: false)}
  end

  # Close *only* the Inspector pane (BT-2611), not the whole right column. The `×`
  # lives in the Inspector sub-pane header, so it must dismiss just the Inspector
  # and leave the Bindings pane (and the column) visible — the whole-column
  # show/hide stays on `toggle_inspector`/`.panel-toggle`. We reset the inspector
  # target/rows/crumbs/error back to the empty state and tear down the live
  # subscription via `track_object(nil)` so re-inspecting an object later rebinds
  # cleanly. Unfreeze too, so a frozen pane doesn't reopen stale on the next
  # inspect. `show_inspector` is intentionally left untouched.
  def handle_event("close_inspector", _params, socket) do
    socket =
      socket
      |> assign(
        inspect_target: nil,
        inspect_rows: [],
        inspect_crumbs: [],
        inspect_error: nil,
        inspect_frozen: false
      )
      |> track_object(nil)

    {:noreply, socket}
  end

  # Drill into an object-valued field of a *floating window* (BT-2493): the field
  # term is carried by index against that window's current rows, exactly like the
  # docked `"drill"` event but scoped to one window. Defensive against a malformed
  # id/index so a crafted event can't crash the LiveView.
  def handle_event("window_drill", %{"id" => id, "index" => index}, socket) do
    with %{} = win <- find_window(socket, id),
         {i, ""} when i >= 0 <- Integer.parse(index),
         %{term: term, name: name} <- Enum.at(win.rows, i) do
      crumbs = win.crumbs ++ [%{label: to_string(name), term: term}]

      {:noreply,
       update_window(socket, id, fn w -> inspect_window(socket, w, name, term, crumbs) end)}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("window_drill", _params, socket), do: {:noreply, socket}

  # Walk a floating window's drill breadcrumb back to an earlier level: truncate
  # its crumb trail at the clicked index and re-inspect that level's live term.
  def handle_event("window_crumb", %{"id" => id, "index" => index}, socket) do
    with %{} = win <- find_window(socket, id),
         {i, ""} when i >= 0 <- Integer.parse(index),
         %{term: term, label: label} <- Enum.at(win.crumbs, i) do
      crumbs = Enum.take(win.crumbs, i + 1)

      {:noreply,
       update_window(socket, id, fn w -> inspect_window(socket, w, label, term, crumbs) end)}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("window_crumb", _params, socket), do: {:noreply, socket}

  # Close a floating window: drop it from the list and release its per-object
  # change subscription (BT-2493 acceptance: "Close removes the window and releases
  # any subscriptions"). Idempotent — closing an unknown id is a no-op.
  def handle_event("window_close", %{"id" => id}, socket) do
    {:noreply, close_window(socket, id)}
  end

  def handle_event("window_close", _params, socket), do: {:noreply, socket}

  # Bring a floating window to the front (z-order follows focus, the spike's
  # stacking). The WindowDrag JS hook fires this on a mousedown anywhere in the
  # window; we bump the clicked window's z above the current max so it overlays the
  # others. Pure view state — no workspace round-trip.
  def handle_event("window_focus", %{"id" => id}, socket) do
    {:noreply, focus_window(socket, id)}
  end

  def handle_event("window_focus", _params, socket), do: {:noreply, socket}

  # Persist a floating window's final position after a drag (BT-2493): the
  # WindowDrag hook reports x/y ONCE on drop (no per-mousemove round-trip), so the
  # position lives in LV state and survives an unrelated re-render. Coordinates are
  # clamped to non-negative integers so a crafted payload can't place a window off
  # into NaN-land.
  def handle_event("window_moved", %{"id" => id, "x" => x, "y" => y}, socket) do
    {:noreply, update_window(socket, id, fn w -> %{w | x: clamp_coord(x), y: clamp_coord(y)} end)}
  end

  def handle_event("window_moved", _params, socket), do: {:noreply, socket}

  # Bring every floating window back onto the default on-screen ladder (BT-2527
  # #4): recovery for a window dragged/restored outside the visible viewport. Pure
  # view state — positions only.
  def handle_event("window_reset_positions", _params, socket) do
    {:noreply, reset_window_positions(socket)}
  end

  # Freeze / unfreeze a single floating window's live tracking (per-window freeze,
  # mirroring the docked `"freeze_toggle"`). Each window watches independently, so
  # one frozen window holds its snapshot while others keep tracking.
  def handle_event("window_freeze", %{"id" => id}, socket) do
    {:noreply, update_window(socket, id, fn w -> toggle_window_freeze(socket, w) end)}
  end

  def handle_event("window_freeze", _params, socket), do: {:noreply, socket}

  # Owner-only per-window poke (the docked `"poke"`, scoped to one window): send
  # the typed message to that window's inspected actor by eval'ing
  # `<binding> <message>`. Rides the same RBAC-gated eval op + well-formedness gate
  # as the docked poke; a window not at a named-binding root reports it can't send.
  def handle_event("window_poke", %{"id" => id, "message" => message}, socket)
      when is_binary(message) do
    {:noreply, update_window(socket, id, fn w -> poke_window(socket, w, message) end)}
  end

  def handle_event("window_poke", _params, socket), do: {:noreply, socket}

  # ── method editor (Wave 3, write-surface ADR 0082) ──────────────────────────

  # Save (durably patch) the edited method on the workspace via the write-surface.
  # On success the method is compiled + flushed into the live BEAM module and an
  # entry is recorded in the workspace ChangeLog, so a subsequent eval observes the
  # new behaviour and the change appears in the change-history pane. A failed
  # compile/save returns a structured #beamtalk_error{} we render as an actionable
  # message (not a flattened string).
  @impl true
  def handle_event(
        "save_method",
        %{"class" => class, "selector" => selector, "source" => source} = params,
        socket
      )
      when is_binary(class) and is_binary(selector) and is_binary(source) do
    # The active tab's id rides the form as a hidden field (BT-2494) so a
    # successful compile clears *that* tab's dirty dot and refreshes its base.
    # The historical save_method payload (the BT-2409 e2e) carries no tab id —
    # `params["tab"]` is then nil and the save still works, just without a tab
    # to reconcile.
    {:noreply, save_method(socket, class, selector, source, params["tab"])}
  end

  # Malformed payload (missing keys or non-binary values): never let a crafted
  # form event crash the LiveView — `save_method/5` calls `String.trim/1`, which
  # would raise on a non-binary. Surface a validation error instead.
  def handle_event("save_method", _params, socket) do
    {:noreply, assign(socket, save_result: nil, save_error: "Invalid method form payload.")}
  end

  # ── tabbed method editor (BT-2494, epic BT-2482 Phase 2) ────────────────────

  # Switch the focused editor tab. Pure view state — no workspace round-trip; an
  # id that no longer maps to an open tab is ignored rather than blanking the
  # editor. Switching also re-syncs the visible class/selector/source assigns
  # (which the save_method form reads) to the newly-active tab.
  def handle_event("tab_select", %{"id" => id}, socket) do
    {:noreply, activate_tab(socket, id)}
  end

  def handle_event("tab_select", _params, socket), do: {:noreply, socket}

  # Close a tab (the spike's × affordance). The strip may empty completely — the
  # editor then shows its empty state (no default tab is re-seeded). Closing the
  # active tab moves focus to the previous remaining tab, or clears focus when it
  # was the last one open.
  def handle_event("tab_close", %{"id" => id}, socket) do
    {:noreply, close_tab(socket, id)}
  end

  def handle_event("tab_close", _params, socket), do: {:noreply, socket}

  # Open a fresh class-definition tab (the spike's "+ def" affordance): a tab
  # whose source is a *class definition* rather than a method body, so saving it
  # compiles the class. The class name comes from the active tab (so "+ def"
  # opens the definition of the class you're editing); a definition tab already
  # open for that class is re-focused rather than duplicated.
  def handle_event("open_definition", _params, socket) do
    {:noreply, open_definition(socket)}
  end

  # Open the *selected* class's definition tab from the System Browser's "class
  # definition" entry (BT-2491). The class rides the click; a malformed payload
  # is ignored rather than crashing the LiveView.
  def handle_event("browser_open_definition", %{"class" => class}, socket)
      when is_binary(class) and class != "" do
    {:noreply, open_definition(socket, class)}
  end

  def handle_event("browser_open_definition", _params, socket), do: {:noreply, socket}

  # BT-2578: toggle the read-only native backing-source pane on a class-definition
  # tab. A native: class (ADR 0056) only has `self delegate` facade methods — the
  # real logic lives in the backing gen_server module's `handle_call` clauses — so
  # this surfaces that module's `.erl` read-only. The backing source is lazily
  # fetched on first open (a `:read` op, safe for the Observer) and cached on
  # `native_view`; a second click collapses it.
  def handle_event("browser_open_native", %{"class" => class}, socket)
      when is_binary(class) and class != "" do
    if native_shown?(socket.assigns, class) do
      {:noreply, assign(socket, native_view: nil)}
    else
      {:noreply, assign(socket, native_view: load_native_view(socket, class))}
    end
  end

  def handle_event("browser_open_native", _params, socket), do: {:noreply, socket}

  # BT-2656: switch the left browser column between the class tree (`classes`) and
  # the separate Native browser (`native`) via the panel-head `Classes | Native`
  # toggle. Switching into Native mode re-fetches the module list so a dependency
  # loaded mid-session is discoverable; an unknown mode is ignored rather than
  # blanking the panel.
  def handle_event("browser_mode", %{"mode" => "native"}, socket) do
    {:noreply, assign(assign_browser_native_modules(socket), browser_mode: :native)}
  end

  def handle_event("browser_mode", %{"mode" => "classes"}, socket) do
    {:noreply, assign(socket, browser_mode: :classes)}
  end

  def handle_event("browser_mode", _params, socket), do: {:noreply, socket}

  # BT-2656/BT-2661: narrow the Native browser by source origin (project / deps /
  # stdlib / all), mirroring the class tree's `browser_source`. Pure view state over
  # the already-loaded rows — no workspace round-trip; an unknown value is ignored.
  # A deliberate pick marks the filter "chosen" so the BT-2661 Project default
  # (applied once the modules load) can never override it.
  def handle_event("native_source", %{"src" => src}, socket)
      when src in ~w(all project deps stdlib) do
    {:noreply, assign(socket, native_source: src, native_source_chosen: true)}
  end

  def handle_event("native_source", _params, socket), do: {:noreply, socket}

  # BT-2667: open a standalone native module's `.erl` as a first-class read-only
  # editor TAB (a `:native` tab kind) rather than the old single-slot overlay
  # assign. The module source now coexists with class/method tabs, switches and
  # closes like any tab, and scrolls inside the editor pane. A module already open
  # is re-focused (keyed by `native:<module>`) rather than duplicated. A module
  # with no readable source still opens — the tab body shows the honest "Erlang
  # source not available" empty state (BT-2648/BT-2668).
  def handle_event("browser_open_native_module", %{"module" => module}, socket)
      when is_binary(module) and module != "" do
    {:noreply, open_native_module_tab(socket, module)}
  end

  def handle_event("browser_open_native_module", _params, socket), do: {:noreply, socket}

  # BT-2667: dismiss the error inside an open native-module tab. The error lives on
  # the tab's cached `native_view`, so clear it there (a no-op if the active tab is
  # not a native tab or carries no error).
  def handle_event("dismiss_native_module_error", _params, socket) do
    {:noreply,
     update_active_tab(socket, fn
       %{kind: :native, native_view: %{} = nv} = tab ->
         %{tab | native_view: Map.put(nv, :error, nil)}

       tab ->
         tab
     end)}
  end

  # BT-2670: save a project-owned native (`.erl`) module from its editable tab —
  # compile the edited buffer, hot-reload the module, and write the source to
  # disk. The active tab must be an editable `:native` tab; the source rides the
  # form's hidden `source` field (the CodeMirror-mirrored textarea, exactly like
  # the method editor). A crafted save against a non-native / read-only tab is a
  # graceful no-op. The ⌘S chord submits this form, same as the method editor.
  def handle_event("native_save", %{"source" => source}, socket) when is_binary(source) do
    {:noreply, save_native_source(socket, source)}
  end

  def handle_event("native_save", _params, socket), do: {:noreply, socket}

  # BT-2578: jump from a `self delegate` method to its Erlang implementation.
  # Opens (or focuses) the class-definition tab and expands the native pane with
  # the method's selector resolved to its matching `handle_call` clause (which the
  # pane highlights). The selector→clause mapping is best-effort: a delegate that
  # completes in `handle_info` resolves to no clause, and the pane says so rather
  # than pretending.
  def handle_event("browser_jump_native", %{"class" => class, "selector" => selector}, socket)
      when is_binary(class) and class != "" and is_binary(selector) do
    socket = open_definition(socket, class)
    {:noreply, assign(socket, native_view: load_native_view(socket, class, selector))}
  end

  def handle_event("browser_jump_native", _params, socket), do: {:noreply, socket}

  # Open a blank "new method" tab for the *selected* class (the System Browser's
  # "new method" entry). A new-method tab is a `:method` tab with no selector yet —
  # the only editor surface that still shows a selector input (the breadcrumb can't
  # name a selector that doesn't exist), so the author types the selector + body.
  # The starter tab used to fill this role on startup; it now opens on demand only.
  def handle_event("new_method", %{"class" => class}, %{assigns: %{role: :owner}} = socket)
      when is_binary(class) and class != "" do
    # Author on whichever side the browser is showing (instance/class), so "new
    # method" while viewing the class side opens a class-side tab.
    {:noreply, open_new_method(socket, class, socket.assigns.browser_side)}
  end

  # Non-owner (Observer) or malformed payload: a no-op. Authoring is owner-only —
  # the entry is rendered only for `:owner`, and a crafted event from a read-only
  # role must not even open a (non-savable) scratch tab in their strip.
  def handle_event("new_method", _params, socket), do: {:noreply, socket}

  # Track edits to the active tab so its dirty dot reflects unsaved changes
  # (BT-2494). The save_method form's `phx-change` reports the live source on
  # each keystroke; we stash it on the active tab and recompute its dirty flag
  # (source != the last-compiled base). Client-supplied, so a non-binary / absent
  # source is ignored rather than crashing.
  #
  # A new-method tab no longer carries a separate selector input (BT-2606): the
  # author writes the full method (signature + body) in the CodeMirror body, and
  # the selector is parsed from that body on save — so there is nothing extra to
  # mirror here. The breadcrumb derives its label live from the same tracked
  # source (`breadcrumb/1`).
  def handle_event("edit_source", %{"source" => source}, socket)
      when is_binary(source) do
    {:noreply, track_edit(socket, source)}
  end

  def handle_event("edit_source", _params, socket), do: {:noreply, socket}

  # Flush all pending durable changes to disk ("Save All to Disk", ADR 0082
  # `Workspace flush`). The summary's conflicts/skipped lists carry recoverable
  # conditions; a hard runtime failure renders as a structured error.
  def handle_event("flush", _params, socket) do
    {:noreply, flush_changes(socket)}
  end

  # Toggle the System Browser's "New Class" modal open/closed (BT-2293, BT-2645).
  # Closed by default; the ＋ button in the browser head flips it. Opening the
  # modal resets the field values + clears any stale in-modal validation error
  # from a prior attempt so the owner gets a clean slate (the superclass defaults
  # back to `Object`).
  def handle_event("toggle_new_class", _params, socket) do
    opening? = !socket.assigns.new_class_open

    socket =
      if opening? do
        assign(socket,
          new_class_open: true,
          new_class_error: nil,
          new_class_name: "",
          new_class_super: "Object"
        )
      else
        assign(socket, new_class_open: false, new_class_error: nil)
      end

    {:noreply, socket}
  end

  # Close the New Class modal (Esc / close button / scrim click), discarding the
  # in-flight fields without creating anything (BT-2645).
  def handle_event("close_new_class", _params, socket) do
    {:noreply, assign(socket, new_class_open: false, new_class_error: nil)}
  end

  # Create a brand-new class from the New Class modal's name + superclass fields
  # (ADR 0082 Phase 5 `Workspace newClass:at:`, BT-2293, BT-2645). The owner types
  # a plain PascalCase class name and picks a superclass (default `Object`); the
  # `<Superclass> subclass: <Name>` definition is synthesized server-side — the
  # user never types `subclass:`. The target `.bt` path is derived from the class
  # name (`Greeter` → `src/Greeter.bt`); the user thinks in classes, not files. A
  # successful create logs a durable `new-class` ChangeLog entry (written to disk
  # later on flush), appears in the Changes pane, and opens + selects the new class.
  def handle_event("new_class", %{"name" => name} = params, socket)
      when is_binary(name) do
    superclass = Map.get(params, "superclass", "Object")
    superclass = if is_binary(superclass), do: superclass, else: "Object"
    {:noreply, new_class(socket, name, superclass)}
  end

  # Malformed payload (missing key / non-binary value): surface an in-modal
  # validation error rather than letting a crafted event crash the LiveView.
  def handle_event("new_class", _params, socket) do
    {:noreply, assign(socket, new_class_error: "Invalid new-class form payload.")}
  end

  # Revert one pending in-memory method patch (ADR 0082 Phase 5 `Workspace
  # changes revert:`, BT-2293), keyed by the `(class, selector)` carried on the
  # ChangeLog row's revert button. Refreshes the Changes pane so the fresh
  # revert entry is visible.
  def handle_event("revert", %{"class" => class, "selector" => selector}, socket)
      when is_binary(class) and is_binary(selector) do
    {:noreply, revert_change(socket, class, selector)}
  end

  # Malformed payload (missing keys / non-binary values): surface a validation
  # error rather than silently no-op'ing, consistent with `new_class`/`save_method`.
  def handle_event("revert", _params, socket) do
    {:noreply, status_error(socket, "Invalid revert request.")}
  end

  # Toggle the structured diff disclosure for one Changes-pane row (BT-2636),
  # keyed by the row's `{class, selector}` carried on the leading caret. Pure view
  # state — no workspace round-trip; flips the key in/out of `:expanded_changes`,
  # showing/hiding that row's `unified_diff` body.
  def handle_event("toggle_change_diff", %{"class" => class, "selector" => selector}, socket)
      when is_binary(class) and is_binary(selector) do
    key = {class, selector}

    expanded =
      if MapSet.member?(socket.assigns.expanded_changes, key) do
        MapSet.delete(socket.assigns.expanded_changes, key)
      else
        MapSet.put(socket.assigns.expanded_changes, key)
      end

    {:noreply, assign(socket, expanded_changes: expanded)}
  end

  def handle_event("toggle_change_diff", _params, socket), do: {:noreply, socket}

  # ── System Browser (BT-2491, epic BT-2482 Phase 2) ──────────────────────────

  # Toggle the class tree between Hierarchy (indented by superclass) and Category
  # (grouped by annotation). Pure view state over the already-loaded class rows —
  # no workspace round-trip; an unknown view is ignored rather than blanking the
  # tree.
  def handle_event("browser_view", %{"view" => view}, socket)
      when view in ~w(hierarchy category) do
    {:noreply, assign(socket, browser_view: view)}
  end

  def handle_event("browser_view", _params, socket), do: {:noreply, socket}

  # Narrow the class tree by source origin (project / deps / stdlib / all). Pure
  # view state over the already-loaded rows — no workspace round-trip; an unknown
  # value is ignored rather than blanking the tree (BT-2557).
  #
  # BT-2597: if the new filter hides the currently-selected class, clear the
  # selection (and its protocol/method pane) so the right pane can't show a
  # "ghost" selection for a class no longer visible in the tree.
  def handle_event("browser_source", %{"src" => src}, socket)
      when src in ~w(all project deps stdlib) do
    # BT-2661: a deliberate pick marks the filter "chosen" so the BT-2661 initial
    # default (applied once the async class load lands) can never override it.
    socket = assign(socket, browser_source: src, browser_source_chosen: true)

    socket =
      if selected_class_visible?(socket) do
        socket
      else
        assign(socket, selected_class: nil, selected_protocol: nil, browser_protocols: [])
      end

    {:noreply, socket}
  end

  def handle_event("browser_source", _params, socket), do: {:noreply, socket}

  # Toggle the instance/class side. The protocol/method list is class-side
  # specific (a class's instance methods differ from its class methods), so
  # flipping the side re-fetches the selected class's protocols for the new side
  # and clears the protocol filter. Pure toggle when no class is selected yet.
  def handle_event("browser_side", %{"side" => side}, socket)
      when side in ~w(instance class) do
    socket = assign(socket, browser_side: side, selected_protocol: nil)

    case socket.assigns.selected_class do
      nil -> {:noreply, assign(socket, browser_protocols: [])}
      class -> {:noreply, load_protocols(socket, class, side)}
    end
  end

  def handle_event("browser_side", _params, socket), do: {:noreply, socket}

  # Select a class in the tree: fetch its protocols (for the current side) and
  # reset the protocol filter. A non-binary / absent class name is ignored rather
  # than crashing the LiveView.
  def handle_event("browser_select_class", %{"class" => class}, socket)
      when is_binary(class) do
    socket =
      assign(socket, selected_class: class, selected_protocol: nil)

    {:noreply, load_protocols(socket, class, socket.assigns.browser_side)}
  end

  def handle_event("browser_select_class", _params, socket), do: {:noreply, socket}

  # Set the protocol filter (the method list shows only that protocol's
  # selectors). An empty value clears the filter back to "all" — the spike's ∗
  # row. Pure view state over the already-loaded protocol tree.
  def handle_event("browser_select_protocol", %{"protocol" => protocol}, socket)
      when is_binary(protocol) do
    filter = if protocol == "", do: nil, else: protocol
    {:noreply, assign(socket, selected_protocol: filter)}
  end

  def handle_event("browser_select_protocol", _params, socket), do: {:noreply, socket}

  # Select a method: open (or focus) it as a tab in the method editor, seeded with
  # its image-accurate source. Browsing *is* editing — the Smalltalk idiom — so a
  # browser click feeds the same write-surface the omni search and Senders /
  # Implementors navigation already do (`open_method_tab`), rather than a separate
  # read-only panel. The class/side/selector ride the click; a malformed payload is
  # ignored.
  def handle_event(
        "browser_select_method",
        %{"class" => class, "side" => side, "selector" => selector},
        socket
      )
      when is_binary(class) and is_binary(side) and is_binary(selector) do
    {:noreply, open_method_tab(socket, class, side, selector)}
  end

  def handle_event("browser_select_method", _params, socket), do: {:noreply, socket}

  # ── omni search (BT-2495, epic BT-2482 Phase 3) ─────────────────────────────

  # Filter the workspace symbol index (classes + selectors) against the live
  # search box text. The OmniSearch hook's `keyup` reports the query; we fetch
  # the symbol outline once and filter it server-side into ranked rows the
  # results popover walks. An empty query closes the popover. The hook owns the
  # arrow/enter highlight client-side, so the server only re-renders the list.
  # An unchanged-query keyup is a no-op: `phx-keyup` fires on EVERY key release,
  # including the arrow/enter keys the OmniSearch hook drives. Re-rendering the
  # identical result list on those would patch the popover and snap the hook's
  # keyboard highlight back to the top — so a keyup whose value matches the
  # current query must not round-trip. (The hook's `updated/0` also guards this,
  # but skipping the re-render entirely is cheaper and removes the race at source.)
  def handle_event("omni_search", %{"value" => query}, socket) when is_binary(query) do
    if query == socket.assigns.omni_query do
      {:noreply, socket}
    else
      {:noreply, run_omni_search(socket, query)}
    end
  end

  def handle_event("omni_search", _params, socket), do: {:noreply, socket}

  # Open an omni-search result: a class opens in the System Browser; a selector
  # opens its (first) implementor in the method-editor tab strip. The chosen
  # result's identity rides the click/enter from the hook. Closes the popover.
  def handle_event(
        "omni_open",
        %{"kind" => "class", "class" => class},
        socket
      )
      when is_binary(class) do
    {:noreply, socket |> open_class(class) |> close_omni()}
  end

  def handle_event(
        "omni_open",
        %{"kind" => "selector", "class" => class, "side" => side, "selector" => selector},
        socket
      )
      when is_binary(class) and is_binary(side) and is_binary(selector) do
    {:noreply, socket |> open_method_tab(class, side, selector) |> close_omni()}
  end

  def handle_event("omni_open", _params, socket), do: {:noreply, close_omni(socket)}

  # Dismiss the omni-search popover (Escape / blur, reported by the hook) without
  # opening anything.
  def handle_event("omni_close", _params, socket), do: {:noreply, close_omni(socket)}

  # ── senders / implementors popovers (BT-2495) ───────────────────────────────

  # The method editor's Senders / Implementors buttons: query the navigation
  # channel for the active tab's selector and open the result popover. Both ride
  # the same `nav-query` read (kinds `senders` / `implementors`); a missing
  # selector (e.g. a class-definition tab) is ignored so the buttons no-op
  # gracefully rather than querying an empty selector.
  def handle_event("senders", _params, socket) do
    {:noreply, run_nav_query(socket, :senders)}
  end

  def handle_event("implementors", _params, socket) do
    {:noreply, run_nav_query(socket, :implementors)}
  end

  # ── native-module callers popover (BT-2669) ─────────────────────────────────
  #
  # The native-module viewer's Callers button: query the navigation channel for
  # the Beamtalk methods that call into the focused `:native` tab's module via
  # `(Erlang <module>) …` (the reverse of "go to native source") and open the
  # same result popover the Senders/Implementors buttons use. A non-native tab is
  # a graceful no-op so the button (only rendered on native tabs) never queries.
  def handle_event("native_callers", _params, socket) do
    {:noreply, run_native_callers_query(socket)}
  end

  # ── protocol actions (BT-2639) ──────────────────────────────────────────────
  #
  # The protocol equivalent of Senders/Implementors: on a class-definition tab
  # whose class is a Protocol, query its required methods / conforming classes and
  # open the same result popover. Both ride the `nav-query` channel (kinds
  # `required_methods` / `conforming_classes`); a non-protocol def tab is a
  # graceful no-op so the buttons (only rendered for protocols) never query a
  # plain class.
  def handle_event("required_methods", _params, socket) do
    {:noreply, run_protocol_nav_query(socket, :required_methods)}
  end

  def handle_event("conforming_classes", _params, socket) do
    {:noreply, run_protocol_nav_query(socket, :conforming_classes)}
  end

  # Open a conforming-class row (BT-2639): clicking a class in the Conforming
  # classes popover opens that class's definition pane and points the System
  # Browser tree at it, then closes the popover. Reuses the existing open-class
  # definition path (`open_definition/2`).
  def handle_event("nav_open_class", %{"class" => class}, socket) when is_binary(class) do
    socket =
      socket
      |> open_definition(class)
      |> assign(nav_popover: nil)

    {:noreply, socket}
  end

  def handle_event("nav_open_class", _params, socket),
    do: {:noreply, assign(socket, nav_popover: nil)}

  # Open the Implementors of a required-method selector (BT-2639): clicking a row
  # in the Required methods popover re-runs the `nav-query` `implementors` kind for
  # that selector (reusing the BT-2495 nav path), replacing the popover contents
  # with the implementing classes.
  def handle_event("nav_required_open", %{"selector" => selector}, socket)
      when is_binary(selector) and selector != "" do
    {:noreply, run_nav_query_for(socket, :implementors, selector)}
  end

  def handle_event("nav_required_open", _params, socket),
    do: {:noreply, assign(socket, nav_popover: nil)}

  # Open a site from the Senders/Implementors popover in the method-editor tab
  # strip (its class + selector + side) and point the System Browser at that
  # class/side too, so a jump to another class navigates the tree alongside the
  # editor, then close the popover.
  def handle_event(
        "nav_open",
        %{"class" => class, "side" => side, "selector" => selector},
        socket
      )
      when is_binary(class) and is_binary(side) and is_binary(selector) do
    socket =
      socket
      |> open_method_tab(class, side, selector)
      |> navigate_browser(class, side)
      |> assign(nav_popover: nil)

    {:noreply, socket}
  end

  def handle_event("nav_open", _params, socket), do: {:noreply, assign(socket, nav_popover: nil)}

  # Dismiss the Senders/Implementors popover.
  def handle_event("nav_close", _params, socket), do: {:noreply, assign(socket, nav_popover: nil)}

  # ── Ctrl/Cmd-click go-to-definition (BT-2666) ───────────────────────────────
  #
  # The CodeMirror editors' `cm_goto.js` extension fires this when the user
  # modifier-clicks a symbol. `token` is the bare identifier under the pointer;
  # `code` is the editor line up to (and including) that token (the same
  # `Receiver selector` line-prefix the `hover` op consumes). We resolve the
  # clicked symbol to a definition target against the LIVE image, mirroring the
  # LSP `definition_provider.rs` resolution order (class def, then selector send;
  # locals/params are a future client-side resolve), and open it by REUSING the
  # existing nav plumbing:
  #
  #   * a known CLASS name → open its `:def` definition tab (`open_definition/2`)
  #     and point the System Browser at it, exactly as `nav_open_class` does.
  #   * otherwise treat it as a SELECTOR send → run the BT-2495 `implementors`
  #     `nav-query`. One implementor → open that method tab directly
  #     (`open_method_tab/4` + `navigate_browser/3`, the `nav_open` path); several
  #     → open the shared Senders/Implementors popover so the user picks; none →
  #     a brief flash (the unresolved no-op), leaving the editor untouched.
  #
  # A class name takes priority over a same-named selector, matching the LSP
  # (class declaration before selector). Unresolvable input (empty token, an
  # unknown symbol with no implementors) flashes "No definition found" rather
  # than crashing or silently doing nothing — the graceful no-op the AC asks for.
  def handle_event("goto_definition", %{"token" => token} = params, socket)
      when is_binary(token) do
    code = Map.get(params, "code", "")
    {:noreply, run_goto_definition(socket, token, code)}
  end

  def handle_event("goto_definition", _params, socket), do: {:noreply, socket}

  # ── dismissable status notices (BT-2612) ────────────────────────────────────
  #
  # Generic dismiss for top-level *scalar* status assigns. The key arrives from
  # the client and is NEVER turned into an atom (`String.to_atom/1` on user input
  # is a memory/atom-table attack vector) — instead it is mapped through a fixed
  # whitelist to the assign we clear. Unknown keys are ignored (no-op), matching
  # the existing "clear to nil" convention every backing handler uses.
  def handle_event("dismiss_notice", %{"key" => key}, socket) do
    case dismiss_key_to_assign(key) do
      nil -> {:noreply, socket}
      assign_key -> {:noreply, assign(socket, assign_key, nil)}
    end
  end

  def handle_event("dismiss_notice", _params, socket), do: {:noreply, socket}

  # Dismiss the error inside the live native-source pane: `@native_view` is a map
  # whose `:error` field carries the banner. Clear only that field so the rest of
  # the pane (content/clauses/meta) is preserved; if the pane is closed
  # (`native_view: nil`) this is a no-op.
  def handle_event("dismiss_native_error", _params, socket) do
    case socket.assigns[:native_view] do
      %{} = nv -> {:noreply, assign(socket, native_view: Map.put(nv, :error, nil))}
      _ -> {:noreply, socket}
    end
  end

  # Dismiss the error inside the Senders/Implementors popover without closing the
  # whole popover (which `nav_close` does). `@nav_popover` is a map; clear only
  # its `:error` field.
  def handle_event("dismiss_nav_error", _params, socket) do
    case socket.assigns[:nav_popover] do
      %{} = nav -> {:noreply, assign(socket, nav_popover: Map.put(nav, :error, nil))}
      _ -> {:noreply, socket}
    end
  end

  # Dismiss a per-window inspector error. `@windows` is a list of window maps; the
  # client sends the window `id` so we clear `:error` on the matching window only.
  def handle_event("dismiss_window_error", %{"id" => id}, socket) do
    {:noreply, update_window(socket, id, fn w -> Map.put(w, :error, nil) end)}
  end

  def handle_event("dismiss_window_error", _params, socket), do: {:noreply, socket}

  # Selection tracking (BT-2485, BT-2539): the method-editor CmEditor hook
  # reports the editor's current selection (text + offsets). We hold it in
  # `edit_selection` so a later pane can evaluate the selected expression rather
  # than the whole buffer (the spike's "evaluates selection" vs "evaluates
  # buffer" distinction). The payload is client-supplied, so accept only the
  # well-formed shape and ignore anything else rather than crash the LiveView.
  def handle_event("select_source", %{"text" => text, "tab_id" => tab_id} = params, socket)
      when is_binary(text) do
    # Stale/in-flight selection guard (BT-2549): a departing CmEditor can dispatch
    # one final `select_source` via `pushEvent` just before its `destroyed()`
    # callback runs; that event can land *after* `sync_active/2` cleared
    # `:edit_selection` on the tab switch, re-populating it with coordinates from
    # the closed tab. The editor instance stamps each push with the tab it edits
    # (data-tab-id), so ignore any stamp that no longer matches the active tab.
    if tab_id == socket.assigns.active_tab do
      selection = %{
        text: text,
        start: clamp_offset(params["start"]),
        end: clamp_offset(params["end"])
      }

      {:noreply, assign(socket, edit_selection: selection)}
    else
      {:noreply, socket}
    end
  end

  # Malformed payload (missing text, non-binary, or missing the "tab_id" key):
  # ignore rather than crash the LiveView (the payload is client-supplied). A
  # present-but-mismatched stamp (incl. `tab_id: null`) is handled by the guarded
  # clause above, which drops it when it doesn't match the active tab.
  def handle_event("select_source", _params, socket), do: {:noreply, socket}

  # Selection tracking for the Workspace dock's editor (BT-2490). Tracked in a
  # SEPARATE assign (`ws_selection`) from the method editor's `edit_selection` so
  # the dock's doIt/printIt/inspectIt evaluate *this* editor's selection — a
  # selection left in the method editor must not leak into a Workspace eval. Same
  # defensive shape as `select_source`.
  def handle_event("select_workspace", %{"text" => text} = params, socket)
      when is_binary(text) do
    selection = %{
      text: text,
      start: clamp_offset(params["start"]),
      end: clamp_offset(params["end"])
    }

    {:noreply, assign(socket, ws_selection: selection)}
  end

  def handle_event("select_workspace", _params, socket), do: {:noreply, socket}

  # Cap on the Transcript pane depth: the client keeps the most recent N lines
  # (via `stream_insert(:transcript, …, limit: -N)`), bounding the DOM in step
  # with the producer's 1000-entry ring buffer (`beamtalk_transcript_stream`).
  # A burst of `Transcript show:` output can't grow the rendered pane unbounded.
  @transcript_scrollback_limit 1000

  # Transcript push, delivered directly over distribution to this LiveView pid.
  #
  # BT-2609: a high-output expression (e.g. `Hanoi solve: 8`) fans out one
  # `{:transcript_output, _}` message *per line* from `beamtalk_transcript_stream`.
  # Inserting each individually means one render + diff push per line — a render
  # storm that floods the LiveView mailbox and stalls the socket (the apparent
  # "REPL hang"). Instead, drain every `{:transcript_output, _}` already queued in
  # the mailbox and `stream_insert` the whole burst in a single render pass. Each
  # insert carries `limit: -@transcript_scrollback_limit` so the rendered DOM
  # stays bounded in step with the producer's 1000-entry ring buffer — the pane
  # can't grow without limit. No line is dropped on our side: every drained
  # message is inserted; only lines that scroll past the depth cap are evicted by
  # the client (consistent with the REPL scrollback, BT-2543).
  @impl true
  def handle_info({:transcript_output, text}, socket) do
    lines = drain_transcript([transcript_line(text)])

    socket =
      Enum.reduce(lines, socket, fn line, acc ->
        stream_insert(acc, :transcript, line, limit: -@transcript_scrollback_limit)
      end)

    {:noreply, socket}
  end

  # Bindings-changed push (`bindings` stream): a *signal*, not the data. Since
  # BT-2531 this rides the SystemAnnouncer bus as a `BindingChanged` announcement
  # delivered natively over distribution:
  # `{:beamtalk_announcement, sub_ref, :BindingChanged, handler, event}`. An eval
  # on any session in the workspace may have changed binding values, so re-read
  # this session's bindings through the read-surface and re-render the pane. This
  # is the "updating live as bindings change" acceptance criterion, driven by the
  # facade subscription rather than polling.
  def handle_info(
        {:beamtalk_announcement, _sub_ref, :BindingChanged, _handler, event},
        %{assigns: %{session_id: session_id, session_pid: pid}} = socket
      )
      when is_pid(pid) do
    # Bindings are per-session isolated (BT-2394: tab1 `x = 100` vs tab2 `x = 999`),
    # so only re-read when the change is for *this* session — otherwise one session's
    # eval would force every connected LiveView to re-render. The typed BindingChanged
    # carries `sessionId` (BT-2530); a nil/unknown origin falls back to a refresh so a
    # session-less event can never silently freeze the pane.
    case Map.get(event, :sessionId) do
      ^session_id -> {:noreply, mark_bindings_loaded(assign_bindings(socket, pid))}
      nil -> {:noreply, mark_bindings_loaded(assign_bindings(socket, pid))}
      _other_session -> {:noreply, socket}
    end
  end

  # Class-lifecycle push (`classes` stream, BT-2598): a class was (re)loaded or
  # removed somewhere in the workspace — a git revert's disk→image reload, another
  # session's flush, an MCP `save_method`, an external edit reloaded into the
  # image. Like the bindings stream this is a *refresh trigger*, not the data: on
  # the signal we re-pull the source-dependent surfaces so open windows reflect
  # the new image without a manual refresh. `:ClassLoaded` covers hot redefinition
  # (the revert case); `:ClassRemoved` covers a teardown. Both refresh the same
  # surfaces — the browser class list, the active ChangeLog, and every open clean
  # method/definition editor tab — so a removed class's stale tab re-reads to its
  # (now empty / disk) state too.
  def handle_info(
        {:beamtalk_announcement, _sub_ref, lifecycle, _handler, _event},
        socket
      )
      when lifecycle in [:ClassLoaded, :ClassRemoved] do
    {:noreply, schedule_source_refresh(socket)}
  end

  # BT-2600: the coalesced source refresh fired by a `ClassLoaded`/`ClassRemoved`
  # burst — re-pull the source-dependent surfaces ONCE for the whole burst, then
  # clear the pending flag so the next burst schedules afresh.
  def handle_info(:do_source_refresh, socket) do
    socket =
      socket
      |> assign(source_refresh_pending: false)
      |> refresh_after_source_change()
      |> mark_source_loaded()

    {:noreply, socket}
  end

  # Per-object change push (BT-2492, backend BT-2489): the watched actor committed
  # a state write. Like the bindings stream this is a *refresh trigger*, not the
  # data — re-read the object's fields + pid stats through the read-surface so the
  # Inspector shows the live snapshot, and bump `:flash_gen` so the FieldFlash JS
  # hook flashes the cells that changed.
  #
  # **Coalescing (no flash storm):** a hot actor can emit a burst of writes. Rather
  # than re-read on every push (which would re-render — and re-flash — the whole
  # table repeatedly), the first push schedules a single deferred refresh via a
  # self-send and sets `:refresh_pending`; intervening pushes are dropped while the
  # flag is set. The deferred `:do_object_refresh` then performs ONE re-read for
  # the whole burst. We ignore the push entirely once frozen, once we've navigated
  # off this object, or for a `pid` that isn't the currently-watched one — the
  # watch server warns it may deliver one final push after we unsubscribe (a
  # navigate-away/freeze race), so we drop a push whose pid doesn't match the head.
  def handle_info({:object_changed, pid, _slots}, %{assigns: assigns} = socket) do
    # The push fans out to TWO independent watchers: the docked Inspector (its
    # `:inspect_watch`) and any floating windows watching this same pid (BT-2493).
    # Each coalesces its own burst, so we schedule the docked refresh here and let
    # `notify_windows_changed/2` schedule a per-pid window refresh — a single push
    # can pulse both the docked pane and a float window on the same actor.
    docked =
      cond do
        assigns.inspect_frozen or not watched_pid?(assigns.inspect_watch, pid) ->
          socket

        assigns.refresh_pending ->
          # A refresh is already queued for this burst — collapse this push into it.
          socket

        true ->
          Process.send_after(self(), :do_object_refresh, refresh_debounce_ms())
          assign(socket, refresh_pending: true)
      end

    {:noreply, notify_windows_changed(docked, pid)}
  end

  # The coalesced refresh fired by `{:object_changed, …}`: re-read the watched
  # object's fields + stats once for the whole burst, then clear the pending flag
  # so the next burst schedules afresh. Guarded against a stale timer firing after
  # the pane froze or navigated away (the watched term went nil / changed).
  def handle_info(:do_object_refresh, %{assigns: %{inspect_watch: term}} = socket)
      when not is_nil(term) do
    {:noreply, refresh_inspector(assign(socket, refresh_pending: false), term)}
  end

  def handle_info(:do_object_refresh, socket) do
    {:noreply, assign(socket, refresh_pending: false)}
  end

  # The coalesced per-window refresh fired by `notify_windows_changed/2` for one
  # `pid`: re-read every floating window whose watched head is that pid, clearing
  # ONLY those windows' pending flags so the burst collapses into one re-read +
  # flash per window. A window watching a *different* pid keeps its own
  # `:refresh_pending` untouched — its own `{:do_window_refresh, otherpid}` timer
  # is still in flight and must not be pre-empted (else that window's refresh would
  # be silently dropped). A window that froze or navigated off this pid since the
  # timer armed no longer matches `watched_pid?/2`, so it is left as-is.
  def handle_info({:do_window_refresh, pid}, socket) do
    windows =
      Enum.map(socket.assigns.windows, fn w ->
        cond do
          # Pending refresh for this pid: re-read + flash, clearing the flag.
          w.refresh_pending and watched_pid?(w.watch, pid) ->
            refresh_window(%{w | refresh_pending: false}, socket, w.watch)

          # Still watching this pid but not pending (already serviced / never
          # scheduled): clear any stale flag so it can't wedge future refreshes.
          watched_pid?(w.watch, pid) ->
            %{w | refresh_pending: false}

          # A different pid (or no watch): leave it untouched — its own timer (if
          # any) is still in flight and must not be pre-empted.
          true ->
            w
        end
      end)

    {:noreply, assign(socket, :windows, windows)}
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  # BT-2590 (S1): the off-socket git load (`assign_git/1`'s `start_async`)
  # completed. The task returned `{status_result, log_result}` — the raw
  # `Facade.dispatch` outcomes — which we fold into the panel assigns here, on the
  # LiveView process, so the render is driven from a single coherent update.
  @impl true
  def handle_async(:git_load, {:ok, {status_result, log_result}}, socket) do
    socket =
      socket
      |> apply_git_status(status_result)
      |> apply_git_log(log_result)

    {:noreply, socket}
  end

  # The git-load task exited. A real crash surfaces as a git-panel error rather
  # than taking down the LiveView. The `:cancelled` clause is a defensive no-op:
  # `assign_git/1` `cancel_async`-es the prior load before starting a new one, and
  # while LiveView normally prunes that stale ref before its exit is delivered, we
  # guard the atom explicitly so a cancellation can never render a spurious error.
  # On a genuine failure we reset BOTH status and log so the panel can't show a
  # stale commit list beside the error banner (a torn read).
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

  # BT-2591: the off-socket mount-time reads (`start_mount_load/2`'s `start_async`)
  # completed. The task returned the four raw `Facade.dispatch` outcomes; we fold
  # them into their assigns here, on the LiveView process, through the same pure
  # `apply_*` helpers the sync refresh callers use — so the async path and any
  # future sync caller agree.
  #
  # BT-2619 (race): a `BindingChanged`/`classes` live push (or any post-mount
  # action) can land *before* this mount load resolves and populate `bindings` /
  # `browser_classes` / `changes` with fresher data. This fold is the *mount-
  # initial* state, so blindly overwriting would clobber that fresher push with
  # staler mount data.
  #
  # We gate each surface on its per-surface "loaded" flag (`:source_loaded` for
  # the browser_classes + changes pair, `:bindings_loaded` for bindings): a flag
  # is set `true` ONLY by a *successful* push refresh (see `:do_source_refresh` /
  # `:BindingChanged`), so we apply a surface's mount read only while its flag is
  # still `false`, then set it `true` so a later sync refresh path stays the source
  # of truth.
  #
  # Success-only gating is what handles the "early push errored, mount succeeded"
  # edge case: a push refresh that fired before this fold and itself failed (e.g.
  # `ClassLoaded` while the workspace was momentarily unreachable → `changes_error`
  # set, list still empty) leaves the flag `false` — so this fold's *successful*
  # mount data still folds in and the pane shows real data rather than getting
  # stuck on the transient error until the next push. A genuinely-empty workspace
  # (no push at all) keeps both flags `false`, so the empty-but-successful mount
  # read still applies and the panes render their empty state.
  #
  # `autoflush` is a stable settings probe with no live push, so it always folds in.
  def handle_async(:mount_load, {:ok, result}, socket) do
    socket =
      socket
      |> fold_mount_read(:source_loaded, result.browser_classes, &apply_browser_classes/2)
      |> fold_mount_read(:bindings_loaded, result.bindings, &apply_bindings/2)
      |> fold_mount_read(:source_loaded, result.changes, &apply_changes/2)
      |> assign(source_loaded: true, bindings_loaded: true)
      # autoflush has no competing live push — always apply the mount read.
      |> apply_autoflush(result.autoflush)

    {:noreply, socket}
  end

  # The mount load crashed/exited. The assigns already hold their loading/empty
  # defaults (lists empty, autoflush false), which *are* the graceful-degradation
  # fallback — so we just keep them rather than crash the mount. A `:cancelled`
  # exit (none today, but defensive — matching `:git_load`) is a no-op too.
  def handle_async(:mount_load, {:exit, :cancelled}, socket), do: {:noreply, socket}

  def handle_async(:mount_load, {:exit, reason}, socket) do
    Logger.error("mount-time workspace load crashed: #{inspect(reason)}",
      domain: [:beamtalk, :liveview]
    )

    {:noreply, socket}
  end

  # BT-2597: the off-socket test run/load (`run_tests/2` / `load_tests/1`)
  # completed. The task tags its dispatch result `{:run, _}` or `{:load, _}` so
  # the right result-application path runs; either way the op is no longer in
  # flight, so the controls re-enable.
  def handle_async(:test_op, {:ok, {:run, dispatch_result}}, socket) do
    {:noreply, socket |> apply_test_result(dispatch_result) |> assign(tests_running: false)}
  end

  def handle_async(:test_op, {:ok, {:load, dispatch_result}}, socket) do
    {:noreply, socket |> apply_test_load(dispatch_result) |> assign(tests_running: false)}
  end

  # A newer run/load `cancel_async`-ed this one. Safe as a no-op only because
  # every `cancel_async(:test_op, …)` is immediately followed by a paired
  # `start_async(:test_op, …)` (in `run_tests/2` / `load_tests/1`) that has
  # already set `tests_running: true` — so the replacement task owns the running
  # state. A future standalone `cancel_async(:test_op, …)` (e.g. a Cancel button)
  # would need to reset `tests_running` itself. Mirrors the `:git_load` no-op.
  def handle_async(:test_op, {:exit, :cancelled}, socket), do: {:noreply, socket}

  def handle_async(:test_op, {:exit, reason}, socket) do
    Logger.error("test run/load crashed: #{inspect(reason)}", domain: [:beamtalk, :liveview])

    # Clear any prior run's results so a stale pass/fail table can't sit beside
    # the crash banner (a torn read) — matching the `:git_load` crash handler and
    # the `apply_test_result/2` dispatch-error path.
    {:noreply,
     assign(socket,
       tests_running: false,
       test_results: nil,
       tests_error: "The test run failed unexpectedly."
     )}
  end

  # BT-2599: the off-socket test-catalogue discovery (`discover_test_classes/1` →
  # `list_tests`, `:read`) completed. We fold the raw dispatch outcome onto the
  # socket through the pure `apply_test_classes/3` helper — the same path the
  # load-tests re-discovery uses — so the async and sync callers agree. The
  # `keep_error?` flag (set by the partial-load re-discovery) rides a transient
  # assign so a *successful* re-discovery doesn't clear a partial-load banner.
  def handle_async(:test_discover, {:ok, result}, socket) do
    keep_error? = socket.assigns[:tests_discover_keep_error] || false

    {:noreply,
     socket
     |> apply_test_classes(result, keep_error?)
     |> assign(:tests_discover_keep_error, false)}
  end

  # A newer discovery (rapid double-refresh / open-then-refresh) `cancel_async`-ed
  # this one — a no-op, mirroring the `:git_load` / `:test_op` cancellation. The
  # replacement task already reset `test_classes` to the nil sentinel, so the
  # pane stays in its "discovering" state until that newer result lands.
  def handle_async(:test_discover, {:exit, :cancelled}, socket), do: {:noreply, socket}

  # The discovery task crashed/exited. Degrade to a `tests_error` rather than
  # taking down the socket (matching the `:git_load` / `:test_op` crash handlers).
  # Leave `test_classes` at the nil sentinel so the pane shows only the error —
  # not the misleading "No TestCase subclasses" empty-state — and retries on the
  # next open/refresh.
  def handle_async(:test_discover, {:exit, reason}, socket) do
    Logger.error("test discovery crashed: #{inspect(reason)}", domain: [:beamtalk, :liveview])

    {:noreply,
     assign(socket,
       test_classes: nil,
       tests_error: "Couldn't discover tests — the discovery failed unexpectedly.",
       tests_discover_keep_error: false
     )}
  end

  # True when `pid` is the pid backing the currently-watched object term — so a
  # late push for an object we've since navigated away from (or never watched) is
  # ignored rather than spuriously re-reading + flashing the current head. Kept
  # below the `handle_info/2` clauses so they stay grouped (compiler warning).
  defp watched_pid?({:beamtalk_object, _c, _m, watched}, pid)
       when is_pid(watched) and is_pid(pid),
       do: watched == pid

  defp watched_pid?(_watch, _pid), do: false

  # The distinct live terms watched by the open floating windows (BT-2493) — the
  # set we must unsubscribe on terminate. Deduped on the watched pid so two windows
  # on the same actor unsubscribe it once.
  defp window_watched_terms(nil), do: []

  defp window_watched_terms(windows) when is_list(windows) do
    windows
    |> Enum.map(& &1.watch)
    |> Enum.filter(&match?({:beamtalk_object, _c, _m, p} when is_pid(p), &1))
    |> Enum.uniq_by(fn {:beamtalk_object, _c, _m, pid} -> pid end)
  end

  @impl true
  def terminate(_reason, socket) do
    # Best-effort: drop our Transcript + bindings subscriptions so the workspace
    # doesn't keep pushing to this (now-dead) pid. The event server also
    # auto-removes dead subscribers via its monitor, so this is belt-and-braces.
    #
    # Then hand the session to the registry's grace window rather than closing it
    # outright. A LiveView `terminate/2` fires on BOTH a real tab close AND a
    # transient socket drop (the latter immediately re-mounts and reconnects). If
    # we closed the session here, a reconnect would always find it gone and lose
    # the tab's accumulated state. So `release/1` schedules a short reap that a
    # reconnecting `checkout/1` cancels (resume); if no reconnect arrives, the
    # registry closes the workspace-supervised session — no orphaned sessions.
    #
    # The session is owned by the workspace's `beamtalk_session_sup` (not this
    # LiveView process), so it does NOT go away just because we exit — the
    # registry's reap is what actually reclaims it.
    if socket.assigns[:connected] do
      Workspace.unsubscribe_transcript(self())
      Workspace.unsubscribe_bindings(self())

      # Also drop any live per-object change subscription (BT-2492) so the watch
      # server stops pushing to this dead pid. Belt-and-braces: the watch server
      # monitors subscribers and auto-removes dead ones, same as the streams above.
      case socket.assigns[:inspect_watch] do
        {:beamtalk_object, _c, _m, pid} = term when is_pid(pid) ->
          Workspace.unsubscribe_object_changes(term, self())

        _ ->
          :ok
      end

      # And drop every floating window's per-object subscription (BT-2493). The
      # watch server keys subscriptions by `(pid, subscriber)` so one unsubscribe
      # per distinct watched pid suffices even if several windows share an actor —
      # `Enum.uniq` over the watched terms avoids a redundant (harmless) RPC.
      for term <- window_watched_terms(socket.assigns[:windows]) do
        Workspace.unsubscribe_object_changes(term, self())
      end

      case socket.assigns[:token] do
        token when is_binary(token) ->
          # Resumable session: stash the open floating-inspector windows so a
          # reconnect within the grace window rebuilds the desk (BT-2527 #3), and
          # likewise stash the doc-block expand state so an expanded block survives
          # the reconnect rather than re-collapsing (BT-2570), then defer teardown
          # to the grace window. The stash dies with the entry if no reconnect
          # arrives, so a genuinely-closed tab leaves nothing behind.
          SessionRegistry.stash_windows(token, build_window_stash(socket))
          SessionRegistry.stash_doc(token, socket.assigns[:doc_expanded])
          SessionRegistry.release(token)

        _ ->
          # No token (resume disabled): nothing in the registry owns this
          # session, so close it directly here or it would leak.
          case socket.assigns[:session_pid] do
            pid when is_pid(pid) -> Workspace.close_session(pid)
            _ -> :ok
          end
      end
    end

    :ok
  end

  # Render a facade short-circuit (RBAC denial / off-vocabulary op) as a clear,
  # user-facing message. These are Phoenix-side decisions, so they don't go
  # through the workspace error formatter.
  defp facade_error(:unauthorized),
    do: "Not authorized: your role may not perform this operation."

  defp facade_error(:forbidden_op), do: "Operation not permitted."
  defp facade_error(reason), do: Workspace.render_error(reason)

  # Run a mutating git op through the Facade (Owner-gated) and refresh the panel
  # so the new status/log is reflected. An :unauthorized/error result surfaces in
  # the panel rather than crashing it.
  # A content-mutating git op (revert / `git restore -- <path>`) needs more than a
  # git-panel refresh: it changes the on-disk working tree, so it must (1) not
  # silently clobber unflushed in-memory edits for that path, and (2) reload the
  # affected module(s) into the live image so image == disk and open windows
  # reflect the reverted code (BT-2598). Routed through `git_revert_event/2`.
  defp git_mutate_event(socket, :git_revert_file, %{path: path} = params) do
    if path_has_pending_edits?(socket, path) do
      # Decision 2: do not revert under unflushed in-memory ChangeLog edits — live
      # work would be silently lost. Block and tell the user to flush or discard
      # the pending entry first. No git call is made.
      assign(socket, git_error: pending_revert_warning(path))
    else
      git_revert_event(socket, params)
    end
  end

  # Stage / unstage / commit do not change working-tree *content* (the file the
  # user is editing), so they keep the original behaviour: dispatch and refresh
  # the git panel only.
  defp git_mutate_event(socket, op, params) do
    case Facade.dispatch(op, params, ctx(socket)) do
      {:ok, _} -> assign_git(socket)
      {:error, reason} -> assign(socket, git_error: facade_error(reason))
    end
  end

  # Revert the working-tree change, then reload the reverted file into the live
  # image and refresh every source-dependent surface (browser, ChangeLog, open
  # editor tabs) plus the git panel. The reload's `ClassLoaded` push *also* drives
  # the refresh for other connected sessions; reloading + refreshing here makes the
  # acting session update synchronously rather than waiting on its own push.
  defp git_revert_event(socket, %{path: path} = params) do
    case Facade.dispatch(:git_revert_file, params, ctx(socket)) do
      {:ok, _} ->
        {reloaded, reload_note} = reload_reverted_path(socket, path)

        socket
        # The reload reconciled the image to the reverted (disk) body, so the
        # reloaded class' open tabs are no longer divergent — clear their
        # `unflushed` badge before the re-read (which would otherwise preserve the
        # already-set divergence, mirroring `clear_disk_differs/2` after a flush).
        |> assign(:tabs, clear_disk_differs(socket.assigns.tabs, reloaded))
        # BT-2655: re-read the reverted `:def` tabs' *editable* definition buffer so
        # the visible editor shows the reverted header without a close/reopen.
        # `refresh_after_source_change/1` below re-reads `:method` tab bodies on its
        # own (and now bumps `editor_rev` to remount the editor — see
        # `resync_active_tab/2`), but it deliberately leaves a `:def` tab's editable
        # definition buffer untouched (a generic push must not clobber a concurrent
        # edit). A revert is the safe exception: it is blocked for this path under
        # pending edits (`path_has_pending_edits?/2`, BT-2598 d2), so overwriting the
        # editable buffer for exactly the reloaded `:def` set is both safe and
        # expected. Method tabs are intentionally left to the refresh below to avoid
        # a redundant second `browse_method_source` round-trip per tab.
        |> reload_reverted_def_buffers(reloaded)
        |> refresh_after_source_change()
        |> assign_git()
        # A clean revert whose reload failed surfaces its note in the shared
        # status area (the same slot revert / new-class outcomes use), NOT
        # `git_error` — `assign_git/1` and the async git load both clear
        # `git_error`, so the note would not survive there. The working tree was
        # reverted, but the image may not have reloaded.
        |> maybe_status_error(reload_note)

      {:error, reason} ->
        assign(socket, git_error: facade_error(reason))
    end
  end

  defp maybe_status_error(socket, nil), do: socket
  defp maybe_status_error(socket, note), do: status_error(socket, note)

  # Reload the reverted file from disk into the live image (image == disk, BT-2585),
  # returning the `{class, selector}` set of the reloaded class' currently-open
  # `:method` tabs (so their `unflushed` badge can be cleared) and a reload-failure
  # note (or nil). A reload failure (a deleted file, a file with a compile error at
  # HEAD) is non-fatal: the working tree was still reverted, and the subsequent
  # refresh re-reads what the image can serve; the note is surfaced afterwards.
  defp reload_reverted_path(socket, path) do
    case Facade.dispatch(:reload, %{path: path}, ctx(socket)) do
      {:ok, class_names} when is_list(class_names) ->
        {reloaded_tab_keys(socket.assigns.tabs, class_names), nil}

      {:error, reason} ->
        {MapSet.new(), "Reverted #{path}, but reload failed: #{facade_error(reason)}"}
    end
  end

  # The disk keys of open `:method` and `:def` tabs whose class is among the
  # just-reloaded class names — the tabs whose `unflushed` badge a revert should
  # clear (their image now matches the reverted disk body). Method tabs key on
  # `{class, selector}`; a `:def` tab keys on `{class, :def}` so a revert that
  # changed a class header (state, superclass) also clears the open def tab's badge
  # without needing a re-open (BT-2600).
  defp reloaded_tab_keys(tabs, class_names) do
    reloaded = MapSet.new(class_names)

    for tab <- tabs,
        key = tab_disk_key(tab),
        key != nil,
        MapSet.member?(reloaded, elem(key, 0)),
        into: MapSet.new(),
        do: key
  end

  # BT-2655: re-read the editable *definition* buffer of every open clean `:def` tab
  # whose disk key is in `reloaded` (the reverted class' def tabs), so the visible
  # editor reflects the reverted class header without a close/reopen. This is the
  # one piece the generic push refresh (`refresh_source_tab/2`) intentionally skips:
  # it re-reads only the `:def` doc block, never the editable definition body, so a
  # concurrent edit during another session's flush is not clobbered. A revert is the
  # safe exception — blocked under pending edits for the reverted path
  # (`path_has_pending_edits?/2`, BT-2598 d2), so no in-progress edit can be lost.
  # A dirty tab is left untouched all the same (defence in depth); method tabs and
  # tabs outside `reloaded` pass through and are handled by the refresh that follows.
  defp reload_reverted_def_buffers(socket, reloaded) do
    tabs =
      Enum.map(socket.assigns.tabs, fn tab ->
        if match?(%{kind: :def, dirty: false}, tab) and
             MapSet.member?(reloaded, tab_disk_key(tab)) do
          reread_reverted_def_buffer(socket, tab)
        else
          tab
        end
      end)

    socket
    |> assign(:tabs, tabs)
    |> resync_active_tab(tabs)
  end

  # Re-read one reverted `:def` tab's editable definition skeleton (header + state)
  # from the now-reverted live image. Only overwrite the editable buffer when the
  # re-fetch actually returned a skeleton — `class_definition_info/2` yields `""` on
  # a transient fetch failure, and blanking a tab the user is looking at would be
  # worse than leaving the prior (about-to-be-correct) body until the next refresh.
  defp reread_reverted_def_buffer(socket, tab) do
    case class_definition_info(socket, tab.class) do
      {"", _comment, _native_module, _class_modifiers, _is_protocol} ->
        tab

      {definition, _comment, _native_module, _class_modifiers, _is_protocol} ->
        # Only the editable definition buffer is touched here; the doc block + badges
        # are refreshed by `refresh_source_tab/2` in the follow-up push refresh.
        %{tab | source: definition, base: definition, dirty: false, disk_differs: false}
    end
  end

  # Whether the file `path` git is about to revert has unflushed in-memory
  # ChangeLog edits (BT-2598 decision 2). The cockpit `changes` rows carry only
  # `class`/`selector`, so map each pending class to its source file via the
  # browser class list (which carries `source_file`) and compare against `path`.
  # Path comparison is by trailing-segment match so a project-relative `path`
  # (`src/Foo.bt`) matches an absolute or differently-rooted `source_file`.
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

  # Two paths refer to the same file when one is a trailing-segment suffix of the
  # other (so `src/Foo.bt` matches `/abs/project/src/Foo.bt`). Both nil/non-binary
  # → no match.
  defp paths_match?(a, b) when is_binary(a) and is_binary(b) do
    a == b or String.ends_with?(a, "/" <> b) or String.ends_with?(b, "/" <> a)
  end

  defp paths_match?(_a, _b), do: false

  # The git-panel message shown when a revert is blocked because the file has
  # unflushed in-memory edits (BT-2598 decision 2) — names the path and the two
  # ways forward (flush to keep the work on disk, or discard the pending entry).
  defp pending_revert_warning(path) do
    "Cannot revert #{path}: it has unflushed in-memory edits. " <>
      "Flush (Save All to Disk) to keep them, or discard the pending change in the Changes pane first."
  end

  # Human label for a porcelain status atom (BT-2586). `beamtalk_git` classifies
  # each XY column into one of these; "—" marks the no-change column.
  defp git_state_label(:unmodified), do: "—"
  defp git_state_label(:modified), do: "modified"
  defp git_state_label(:added), do: "added"
  defp git_state_label(:deleted), do: "deleted"
  defp git_state_label(:renamed), do: "renamed"
  defp git_state_label(:copied), do: "copied"
  defp git_state_label(:untracked), do: "untracked"
  defp git_state_label(:ignored), do: "ignored"
  defp git_state_label(:unmerged), do: "unmerged"
  defp git_state_label(:type_changed), do: "type-changed"
  defp git_state_label(other), do: to_string(other)

  # The Senders/Implementors popover heading for a nav kind (BT-2495).
  defp nav_kind_label(:senders), do: "Senders"
  defp nav_kind_label(:implementors), do: "Implementors"
  # BT-2639: the protocol-action popover headings.
  defp nav_kind_label(:required_methods), do: "Required methods"
  defp nav_kind_label(:conforming_classes), do: "Conforming classes"
  # BT-2669: the native-module callers popover heading.
  defp nav_kind_label(:callers_of_native_module), do: "Callers"

  # ── Structured unified-diff view (BT-2636) ──────────────────────────────────

  # Parse a verbatim unified-diff string into structured lines for the
  # `unified_diff/1` component. Presentation only: the diff text seam (Changes'
  # `present_diff/1`, git's `git_diff/1`) is unchanged — we classify it here at
  # render time so add/remove/context/hunk/meta lines can be coloured and the
  # marker lifted into a fixed-width gutter.
  #
  # Each entry is `%{kind, marker, content}`:
  #   * `:add`     — a `+` line; marker "+", content is the line WITHOUT the `+`.
  #   * `:remove`  — a `-` line; marker "-", content without the `-`.
  #   * `:context` — a ` ` (space-prefixed) line; marker " ".
  #   * `:hunk`    — an `@@ … @@` header; the whole line is the content.
  #   * `:meta`    — file headers (`diff --git`, `index`, `--- `, `+++ `, mode
  #                  changes, `\ No newline…`, etc.); content is the whole line.
  #
  # The leading marker is stripped from add/remove/context content so source
  # indentation lines up across rows regardless of marker; the marker rides the
  # gutter instead. All other content (including any leading whitespace beyond
  # the marker) is preserved verbatim. A blank/binary/nil diff yields `[]`.
  @doc false
  def parse_diff(diff) when is_binary(diff) and diff != "" do
    diff
    # Strip the standard git/diff trailing newline so we don't emit a spurious
    # blank meta row at the bottom; intentional blank context lines are
    # space-prefixed and untouched.
    |> String.trim_trailing("\n")
    |> String.split("\n")
    |> Enum.map(&classify_diff_line/1)
  end

  def parse_diff(_), do: []

  # `--- ` / `+++ ` file headers must be classified as :meta BEFORE the bare
  # `+`/`-` add/remove clauses, since they start with the same character.
  defp classify_diff_line("+++ " <> _ = line), do: %{kind: :meta, marker: "", content: line}
  defp classify_diff_line("--- " <> _ = line), do: %{kind: :meta, marker: "", content: line}
  defp classify_diff_line("@@" <> _ = line), do: %{kind: :hunk, marker: "", content: line}
  defp classify_diff_line("+" <> rest), do: %{kind: :add, marker: "+", content: rest}
  defp classify_diff_line("-" <> rest), do: %{kind: :remove, marker: "-", content: rest}
  defp classify_diff_line(" " <> rest), do: %{kind: :context, marker: " ", content: rest}

  defp classify_diff_line("diff --git" <> _ = line),
    do: %{kind: :meta, marker: "", content: line}

  defp classify_diff_line("index " <> _ = line), do: %{kind: :meta, marker: "", content: line}
  # Everything else (mode lines, `\ No newline at end of file`, blank trailing
  # split fragments, etc.) is neutral metadata.
  defp classify_diff_line(line), do: %{kind: :meta, marker: "", content: line}

  # Per-line CSS class for the structured diff body.
  defp diff_line_class(:add), do: "diff-line diff-add"
  defp diff_line_class(:remove), do: "diff-line diff-del"
  defp diff_line_class(:hunk), do: "diff-line diff-hunk"
  defp diff_line_class(:meta), do: "diff-line diff-meta"
  defp diff_line_class(:context), do: "diff-line diff-ctx"

  # Shared structured diff renderer (BT-2636). Takes a verbatim unified-diff
  # string and renders it as coloured, gutter-aligned rows — reused by the
  # Changes pane (net-vs-disk) and the Git pane (staged/worktree). The marker
  # (`+`/`-`/space) sits in a fixed-width gutter so content left-aligns across
  # rows. A blank/binary diff renders nothing (callers show their own empty
  # state, e.g. git's "No textual diff" note).
  attr :diff, :string, required: true

  defp unified_diff(assigns) do
    assigns = assign(assigns, :lines, parse_diff(assigns.diff))

    ~H"""
    <div :if={@lines != []} class="bt-diff">
      <div :for={line <- @lines} class={diff_line_class(line.kind)}>
        <span class="diff-gutter" aria-hidden="true">{line.marker}</span><span class="diff-content">{line.content}</span>
      </div>
    </div>
    """
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
  # `ws_selection` current (distinct from the method editor's `edit_selection`);
  # an empty or whitespace-only selection falls back to the buffer.
  defp eval_target(expr, socket) do
    if ws_selection?(socket.assigns), do: socket.assigns.ws_selection.text, else: expr
  end

  # Whether the Workspace editor has a non-blank selection to evaluate. Takes the
  # bare `assigns` so the render template can call it directly too.
  defp ws_selection?(assigns) do
    case assigns[:ws_selection] do
      %{text: text} when is_binary(text) -> String.trim(text) != ""
      _ -> false
    end
  end

  # Render an eval success according to the chosen action (BT-2542 Workspace
  # rebuild). The growing below-editor `.ws-result` success bubble is gone; the
  # Workspace is now editor-primary, so feedback is split:
  #
  #   * print_it   — the classic Workspace "Print it": the result is inserted
  #     INLINE into the CodeMirror buffer after the evaluated region (pushed to
  #     the `CmEditor` hook as `ws_insert_result`, rendered as a collapsible
  #     block widget — NOT doc text, so "evaluate buffer" never re-runs it). A
  #     terse `→ result` also flashes in the transient status line.
  #   * do_it      — evaluate for side effects; a subtle, self-clearing
  #     `✓ evaluated` status only (no buffer insert; ambient output → Transcript).
  #   * inspect_it — show the term in the status AND open it in the Inspector.
  #
  # All three carry the result/confirmation in `result` (rendered as the thin
  # `.eval-status` line) and bump `eval_seq` to restart its fade.
  defp eval_success(socket, "do_it", _term, output, expr) do
    eval_status(socket, "✓ evaluated", output, expr)
  end

  defp eval_success(socket, "inspect_it", term, output, expr) do
    socket = eval_status(socket, "→ " <> Workspace.render_term(term), output, expr)

    # In `"float"` mode (BT-2493) Inspect-it opens a floating window on the
    # `→ result` term rather than driving the docked pane; docked mode keeps the
    # original single-pane behaviour.
    if socket.assigns.inspector_mode == "float" do
      open_window_for_term(socket, "→ result", term)
    else
      inspect_term(socket, "→ result", term, [%{label: "→ result", term: term}])
    end
  end

  # print_it (and the historical default): insert the result inline in the buffer
  # AND flash it in the status line.
  defp eval_success(socket, _print_it, term, output, expr) do
    rendered = Workspace.render_term(term)
    # Capture the anchor from the ORIGINAL socket before the pipe: `eval_status`
    # doesn't touch `ws_selection`, but binding it here makes that independence
    # explicit and survives a future pipe stage that might clear the selection.
    anchor = ws_anchor(socket)

    # `push_event` is page-wide — every CmEditor hook that registered the
    # handler receives it. Scope it to the Workspace editor by element id (the
    # client drops a mismatched target) so a second inline editor (BT-2543) can't
    # also insert this result. The id is the shared `workspace_editor_id/0` so the
    # target and the template host can't drift apart.
    socket
    |> eval_status("→ " <> rendered, output, expr)
    |> push_event("ws_insert_result", %{
      text: rendered,
      anchor: anchor,
      target: workspace_editor_id()
    })
  end

  # DOM id of the Workspace CodeMirror editor host — the single source of truth
  # shared by the template element and the `ws_insert_result` push target, so a
  # rename can't silently break inline results (the client guard discards a push
  # whose target doesn't match `this.el.id`).
  defp workspace_editor_id, do: "workspace-editor-overlay"

  # The doc offset to anchor an inline result after: the end of the tracked
  # selection (the evaluated region) when evaluating a selection, else nil so the
  # client falls back to the live buffer end. Echoed in the `ws_insert_result`
  # push so a cursor move during the eval round-trip (wider over a remote
  # distribution node) can't drop the widget on the wrong line.
  defp ws_anchor(socket) do
    if ws_selection?(socket.assigns) do
      case socket.assigns.ws_selection[:end] do
        offset when is_integer(offset) -> offset
        _ -> nil
      end
    end
  end

  # Assign the transient eval-status line + bump its re-key sequence. Shared by
  # every success branch so the status fade restarts consistently per eval.
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

  # Normalise a client-supplied selection offset to a non-negative integer (or
  # nil). The CmEditor hook sends integer offsets, but the payload is
  # untrusted, so a missing / negative / non-integer value collapses to nil.
  defp clamp_offset(n) when is_integer(n) and n >= 0, do: n
  defp clamp_offset(_), do: nil

  # ── Transcript helpers (BT-2609) ─────────────────────────────────────────────

  # Selectively drain every `{:transcript_output, _}` already sitting in the
  # mailbox (zero-timeout receive) and accumulate the rendered line maps. The
  # caller seeds `acc` with the line that triggered the handle_info, so the
  # whole queued burst is coalesced into one batch of stream inserts — one
  # render pass instead of one per line. Order is preserved (acc is built in
  # arrival order, reversed once at the end).
  #
  # The drain is capped at @transcript_scrollback_limit per pass: the upstream
  # ring buffer bounds server-side *history*, not how many messages are already
  # queued in this pid's mailbox (concurrent high-output evals, or a slow client
  # backing up renders, can pile up more than the cap). Without a cap a single
  # handle_info could hold the callback draining thousands of entries. Anything
  # beyond the cap stays in the mailbox and triggers another handle_info pass
  # naturally — no lines are lost, and the per-callback work stays bounded.
  #
  # `count` mirrors `length(acc)` so the cap check stays O(1) instead of
  # re-measuring the list each pass. Invariant: callers seed `count` to the
  # initial accumulator length — the sole call site passes a one-element list,
  # so the default of 1 holds. Keep this in sync if a second call site is added.
  defp drain_transcript(acc, count \\ 1)

  defp drain_transcript(acc, count) when count >= @transcript_scrollback_limit do
    Enum.reverse(acc)
  end

  defp drain_transcript(acc, count) do
    receive do
      {:transcript_output, text} ->
        drain_transcript([transcript_line(text) | acc], count + 1)
    after
      0 -> Enum.reverse(acc)
    end
  end

  defp transcript_line(text) do
    %{id: System.unique_integer([:positive]), text: to_string(text)}
  end

  # ── REPL helpers (BT-2543) ───────────────────────────────────────────────────

  # Cap on the REPL scrollback depth: the client keeps the most recent N entries
  # (via `stream_insert(:repl, …, limit: -N)`) and `repl_terms` is evicted in
  # lockstep, so a long session can't grow the DOM or the assigns map unbounded.
  @repl_scrollback_limit 200

  # Append a successful `› request` / `→ response` pair to the scrollback and
  # stash the live result term under the entry id so a later Inspect click can
  # re-open it. The response is the surface-shared `render_term` rendering — the
  # SAME string the Workspace `→ result` shows — so the two surfaces stay
  # display-consistent. Long responses are marked so the template can collapse
  # them within the entry rather than letting one result flood the scrollback.
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
    # term, then evict the one that just scrolled past the depth cap. Each entry
    # is a small reference (the object lives in the workspace process), but a long
    # session would still grow the map unboundedly without this.
    |> update(:repl_terms, fn terms -> terms |> Map.put(id, term) |> repl_evict(seq) end)
    |> stream_insert(:repl, entry, limit: -@repl_scrollback_limit)
    |> repl_scroll_to_bottom()
  end

  # Append an error entry: the `→ response` carries the rendered error and there
  # is no live term to inspect, so no Inspect affordance and nothing stashed.
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
    # An error entry stashes no term, but still bump the cap so an OK term that
    # scrolled past the depth limit (counting errors too) is evicted in step.
    |> update(:repl_terms, &repl_evict(&1, seq))
    |> stream_insert(:repl, entry, limit: -@repl_scrollback_limit)
    |> repl_scroll_to_bottom()
  end

  # Cap the scrollback depth (client DOM via `stream_insert(limit:)`) and the
  # `repl_terms` map in lockstep. Entry ids are monotonic (`repl-entry-N`, N =
  # seq), so the entry now `@repl_scrollback_limit` positions back is exactly the
  # one the client dropped — evict its term (a no-op for an error entry, which
  # was never stashed).
  defp repl_evict(terms, seq) do
    evicted = seq - @repl_scrollback_limit
    if evicted > 0, do: Map.delete(terms, repl_entry_id(evicted)), else: terms
  end

  # Scroll the scrollback to the newest entry on each append (classic terminal
  # behaviour): a new submission should reveal its result even if the user had
  # scrolled up to read older output. The scroll is a pure client effect, so it
  # rides a push to the ReplInput hook rather than a re-render.
  defp repl_scroll_to_bottom(socket) do
    push_event(socket, "repl_scroll_bottom", %{})
  end

  defp repl_entry_id(seq), do: "repl-entry-#{seq}"

  # DOM id of the REPL input editor host — the single source of truth shared by
  # the template element and the `repl_set_input` push target, so a rename can't
  # silently break history recall / submit-clear. (There is a single ReplInput
  # instance on the page; `push_event/3` reaches every hook registered for the
  # event, so this id is for template/push-target consistency, not filtering.)
  defp repl_input_id, do: "repl-input"

  # First-line (capped) preview shown in a collapsed long response's `<summary>`:
  # enough to recognise the result without expanding, with an ellipsis when the
  # full text is longer.
  defp repl_preview(text) when is_binary(text) do
    first = text |> String.split("\n", parts: 2) |> hd()

    cond do
      String.length(first) > 80 -> String.slice(first, 0, 80) <> "…"
      first != text -> first <> " …"
      true -> first
    end
  end

  # A response is "long" (worth collapsing within its entry) when it spills past a
  # handful of lines or a few hundred chars — the threshold that keeps a single
  # verbose result from pushing the rest of the scrollback off-screen.
  defp repl_long?(text) when is_binary(text) do
    # `parts: 7` short-circuits the split after 6 newlines instead of
    # materialising every line just to count past six.
    String.length(text) > 320 or length(String.split(text, "\n", parts: 7)) > 6
  end

  # Record a submitted expression at the head of the recall ring and reset the
  # ↑/↓ cursor to the live input. Every prior occurrence of the expression is
  # dropped first so re-running the same expression doesn't bloat the ring (all
  # earlier copies collapse onto the new head, shell `HISTCONTROL=erasedups`
  # style — not just consecutive runs); the ring is capped so a long session
  # can't grow the assigns unbounded.
  @repl_history_limit 100
  defp repl_record_history(socket, expr) do
    history =
      [expr | Enum.reject(socket.assigns.repl_history, &(&1 == expr))]
      |> Enum.take(@repl_history_limit)

    assign(socket, repl_history: history, repl_history_pos: nil)
  end

  # Walk the recall ring and push the recalled text to the input. `:prev` (↑)
  # moves toward older entries; `:next` (↓) moves toward the present, and stepping
  # past the newest restores the empty live input (pos = nil). An empty ring or a
  # ↓ while already at the live input is a no-op (no push, so the hook keeps the
  # in-progress text the user was typing).
  #
  # The first clause also covers the attach-failure window: when bind_session
  # never ran, `:repl_history` is absent, so a crafted ↑/↓ must NOT fall through
  # to `socket.assigns.repl_history` (a KeyError that would crash the LiveView) —
  # `is_map_key/2` guards it to a no-op.
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
        # ↓ at the live input: nothing to recall, leave the user's draft alone.
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
  # composer). The input is hook-owned (phx-update=ignore), so the server can only
  # set it by pushing to the ReplInput hook.
  defp repl_clear_input(socket) do
    push_event(socket, "repl_set_input", %{text: ""})
  end

  # ── REPL meta-commands (BT-2543 follow-up) ──────────────────────────────────
  #
  # The CLI REPL parses `:`-prefixed meta-commands client-side (see
  # crates/beamtalk-cli/src/commands/repl/mod.rs); the LiveView REPL historically
  # forwarded them straight to `eval`, which choked trying to compile `:h` as a
  # Beamtalk expression. In a graphical IDE most of those commands map onto a pane
  # that already exists (the System Browser, the Bindings pane, the Changes tab),
  # so rather than re-implement the CLI's command DSL we recognise the leading
  # colon and either DRIVE the matching pane (`:help X` focuses the System
  # Browser) or POINT the user at it. Input without a leading colon is real code
  # and falls through to `eval` untouched.
  #
  # Returns `nil` for non-meta input (the overwhelmingly common path) so the
  # caller's `cond` falls through to eval; otherwise a parsed `{kind, …}` tuple
  # `handle_repl_meta/3` routes.
  defp repl_meta_command(input) do
    if String.starts_with?(input, ":") do
      # `input` starts with ":", so splitting on whitespace always yields at least
      # the command token (a bare ":" splits to `[":"]`, routed to the catch-all).
      [cmd | rest] = String.split(input, ~r/\s+/, parts: 2, trim: true)
      repl_meta_dispatch(cmd, meta_arg(rest))
    else
      nil
    end
  end

  # First whitespace-delimited token of a meta-command's argument, with a leading
  # `#` stripped so `:help #Counter` and `:help Counter` agree. `nil` when the
  # command had no argument.
  defp meta_arg([]), do: nil

  defp meta_arg([arg]) do
    # `arg` is the non-empty second part of the outer `parts: 2, trim: true` split,
    # so this inner split always yields at least the first token.
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

  # Route a parsed meta-command. `:help X` drives the System Browser; everything
  # else appends an informational scrollback entry (a third `kind`, `:info`, the
  # template styles muted rather than as an error).
  defp handle_repl_meta(socket, {:help, nil}, expr),
    do: repl_append_info(socket, expr, repl_help_text())

  defp handle_repl_meta(socket, {:help, class}, expr),
    do: repl_focus_class(socket, expr, class)

  defp handle_repl_meta(socket, {:point, message}, expr),
    do: repl_append_info(socket, expr, message)

  # Route a meta-command to a dock tab (BT-2557: `:test` → Tests pane). Switches
  # the dock to `tab`, lazily loads the Tests catalogue when that is the target,
  # and appends a confirming info entry — the GUI equivalent of the CLI command.
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
  # `:help Class` → `Beamtalk help: Class`). We validate against the live symbol
  # index first so an unknown name gives a clean message instead of pointing the
  # browser at a class that doesn't exist.
  defp repl_focus_class(socket, expr, class) do
    if class in browser_class_names(socket) do
      socket
      |> open_class(class)
      |> repl_append_info(expr, "Opened #{class} in the System Browser ◂")
    else
      # "No class named X" is feedback about a meta-command, not a code-evaluation
      # failure, so it uses the muted `:info` styling (like an unknown `:cmd`) rather
      # than the red error arrow reserved for eval errors.
      repl_append_info(
        socket,
        expr,
        "No class named #{class}. Browse classes in the System Browser, or search with the omni bar (top)."
      )
    end
  end

  # The class names known to the live image, from the same symbol index the omni
  # search uses, as a MapSet so the `class in browser_class_names(socket)`
  # membership check in `repl_focus_class/3` is O(1). An empty index (dispatch
  # failure / RBAC denial) just means every `:help X` reports "no such class"
  # rather than crashing.
  defp browser_class_names(socket) do
    socket
    |> symbol_rows()
    |> Enum.filter(&(&1.kind == "class"))
    |> MapSet.new(& &1.class)
  end

  # `:help` with no argument: a short tour of where the CLI REPL's commands live
  # in the IDE, so a muscle-memory `:h` lands somewhere useful instead of erroring.
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

  # After a successful eval, if the expression was a `Beamtalk help: Class` send
  # (the CLI's `:help` desugaring, and a natural thing to type directly), focus
  # the System Browser on that class too — the help text stays in the scrollback
  # AND the browser navigates to the subject. Non-help evals pass through
  # untouched.
  #
  # Unlike `repl_focus_class/3`, this skips the `browser_class_names/1` validation
  # and calls `open_class` directly: the `eval` already succeeded, which proves the
  # class exists in the runtime, so a symbol-index lookup would be redundant (and
  # would falsely reject a class defined moments earlier if the index is briefly
  # stale). `load_protocols` handles an empty result gracefully regardless.
  defp repl_help_followup(socket, expr) do
    case Regex.run(~r/^\s*Beamtalk\s+help:\s+#?([A-Z]\w*)/, expr) do
      [_, class] -> open_class(socket, class)
      _ -> socket
    end
  end

  # Append an informational meta-command response (`kind: :info`): no live term,
  # so no Inspect affordance and nothing stashed. Mirrors `repl_append_error/3`'s
  # bookkeeping (seq bump + term-map eviction in lockstep with the scrollback cap).
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

  # ── method editor helpers (Wave 3) ──────────────────────────────────────────

  # Compile (⌘S) the active tab's source. A class-definition tab evals its whole
  # definition (compiling the class); a method tab drives the write-surface
  # `save` install chokepoint (compile + flush). The tab kind is read from the
  # open-tab list by id — NOT inferred from the payload — so the historical
  # method save_method payload (no tab id, the BT-2409 e2e) keeps its exact
  # behaviour. On success the matching tab's dirty dot clears and its base source
  # is updated to the compiled text.
  defp save_method(socket, class, selector, source, tab_id) do
    case tab_id && find_tab(socket, tab_id) do
      %{kind: :def} = tab -> save_definition(socket, tab, source)
      _ -> save_method_body(socket, class, selector, source, tab_id)
    end
  end

  # Validate the edit form, then drive the write-surface save. Empty class or an
  # un-derivable selector is a local validation error (rendered without a
  # round-trip); a real save threads the body value straight to the workspace
  # install chokepoint.
  #
  # On a *new-method* tab there is no selector input anymore (BT-2606): the author
  # writes the full method (signature + body) in the body, so the selector is
  # parsed from the source signature here and the form's (empty) `selector` field
  # is ignored. An existing-method tab keeps the breadcrumb selector that rode the
  # hidden field. The derived selector still passes through the `:save` op, where
  # the compiler re-parses it from the body and rejects any mismatch — so a parse
  # that disagrees with the compiler fails loudly rather than installing under the
  # wrong key.
  defp save_method_body(socket, class, selector, source, tab_id) do
    class = String.trim(class)

    selector =
      case tab_id && find_tab(socket, tab_id) do
        %{new: true} -> parse_method_signature_selector(source)
        _ -> String.trim(selector)
      end

    socket =
      assign(socket,
        edit_class: class,
        edit_selector: selector,
        edit_source: source
      )

    cond do
      class == "" ->
        assign(socket, save_result: nil, save_error: "Enter a class name to save a method.")

      selector == "" ->
        assign(socket,
          save_result: nil,
          save_error: "Could not parse a method signature from the source."
        )

      true ->
        case Facade.dispatch(
               :save,
               %{class: class, selector: selector, source: source},
               ctx(socket)
             ) do
          {:ok, saved_class} ->
            # The patch is live + logged; refresh the change-history pane so the
            # new entry is visible (ChangeLog coherence). A successful compile
            # also clears the active tab's dirty dot and re-bases it on the
            # compiled source.
            socket
            |> assign(
              save_result: "Saved #{selector} on #{saved_class}",
              save_error: nil,
              flush_result: nil,
              flush_error: nil
            )
            |> compile_clean(tab_id, source)
            |> promote_new_method_tab(tab_id, saved_class, selector)
            |> assign_changes()

          {:error, reason} ->
            # `reason` may be a facade RBAC denial (`:unauthorized`) for a crafted
            # event from a read-only role, or a workspace #beamtalk_error{}.
            assign(socket, save_result: nil, save_error: facade_error(reason))
        end
    end
  end

  # Compile a class-definition tab (BT-2494) by evaluating its definition source
  # against the workspace — exactly the path the e2e tests use to define a class,
  # so "saving a class definition compiles the class" needs no new server op
  # (ADR 0082; the `eval` facade op). An empty body is a local validation error;
  # a compile failure renders the structured `#beamtalk_error{}`.
  defp save_definition(socket, tab, source) do
    socket = assign(socket, edit_source: source)
    pid = socket.assigns[:session_pid]

    cond do
      String.trim(source) == "" ->
        assign(socket, save_result: nil, save_error: "Enter a class definition to compile.")

      not is_pid(pid) ->
        assign(socket, save_result: nil, save_error: "not attached to workspace")

      true ->
        save_definition_eval(socket, tab, source, pid)
    end
  end

  defp save_definition_eval(socket, tab, source, pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: source}, ctx(socket)) do
      {:ok, _term, _output, _warnings} ->
        socket
        |> assign(
          save_result: "Compiled #{tab.class}",
          save_error: nil,
          flush_result: nil,
          flush_error: nil
        )
        |> compile_clean(tab.id, source)
        |> assign_changes()
        # BT-2586/BT-2590: refresh the git panel when it is open AND autoflush is
        # on — only then did the save write through to disk. With autoflush off
        # the save patches the live image only, so the on-disk working tree is
        # unchanged and the git shell-out is skipped (no redundant refresh).
        |> maybe_refresh_git_after_save()

      {:error, reason, _output, _warnings} ->
        assign(socket, save_result: nil, save_error: Workspace.render_error(reason))

      {:error, reason} ->
        assign(socket, save_result: nil, save_error: facade_error(reason))
    end
  end

  # BT-2670: save (edit → compile → reload → write-back) the active native tab's
  # `.erl`. The save is only honoured for an editable `:native` tab — a crafted
  # event against a read-only / non-native tab is a graceful no-op. The op may
  # return a clean success, structured compile errors (rendered inline via the
  # shared `@save_error` notice), or an authorization/dispatch error.
  defp save_native_source(socket, source) do
    case active_tab(socket.assigns) do
      %{kind: :native, native_view: %{editable: true}, class: module} = tab ->
        socket = assign(socket, edit_source: source)

        case Facade.dispatch(
               :save_native_source,
               %{module: module, source: source},
               ctx(socket)
             ) do
          {:value, %{"ok" => true}} ->
            # Compiled, reloaded, and written to disk. Clear the dirty dot,
            # re-base the tab on the saved source, and refresh the cached
            # native_view content so a later read shows the new source.
            socket
            |> assign(
              save_result: "Saved #{module}.erl",
              save_error: nil,
              flush_result: nil,
              flush_error: nil
            )
            |> compile_clean(tab.id, source)
            |> update_native_view_content(tab.id, source)

          {:value, %{"errors" => [_ | _] = errors}} ->
            assign(socket, save_result: nil, save_error: native_compile_error(errors))

          {:error, reason} ->
            assign(socket, save_result: nil, save_error: facade_error(reason))

          _other ->
            assign(socket, save_result: nil, save_error: "Could not save native source.")
        end

      _ ->
        socket
    end
  end

  # BT-2670: refresh the editable native tab's cached `native_view.content` to the
  # just-saved source so a re-render (or a Callers query) reflects what is now on
  # disk and in the image, without a round-trip back to `browse-native-source`.
  defp update_native_view_content(socket, tab_id, source) do
    update_active_tab_by_id(socket, tab_id, fn
      %{kind: :native, native_view: %{} = nv} = tab ->
        %{tab | native_view: Map.put(nv, :content, source)}

      tab ->
        tab
    end)
  end

  # BT-2670: flatten the structured native compile-error maps (the same shape the
  # load path produces — `path`/`kind`/`message`/optional `line`) into a single
  # inline message, mirroring how a `.bt` compile error reads. The first error's
  # message + line leads; a count tail signals there are more.
  defp native_compile_error(errors) do
    count = length(errors)
    first = List.first(errors)
    msg = Map.get(first, "message", "Erlang compilation failed")

    located =
      case Map.get(first, "line") do
        line when is_integer(line) -> "line #{line}: #{msg}"
        _ -> msg
      end

    if count > 1, do: "#{located} (+#{count - 1} more)", else: located
  end

  # Create a new class from the New Class modal's name + superclass (BT-2293,
  # BT-2645). The owner supplies a plain PascalCase class name and a superclass
  # (default `Object`); validation (PascalCase, non-empty, non-duplicate) runs
  # locally — a rejected name surfaces an in-modal error and never round-trips.
  # A valid name synthesizes the `<Superclass> subclass: <Name>` definition,
  # derives the target `.bt` path from the name (so the user never types a file
  # path), threads source + path to the workspace newClass chokepoint, refreshes
  # the Changes pane (ChangeLog coherence), and opens + selects the new class.
  defp new_class(socket, name, superclass) do
    name = String.trim(name)
    superclass = trim_superclass(superclass)

    with :ok <- validate_new_class_name(socket, name),
         :ok <- validate_superclass(superclass) do
      source = superclass <> " subclass: " <> name
      {:ok, path} = derive_class_path(name)
      dispatch_new_class(socket, name, superclass, source, path)
    else
      {:error, message} ->
        # Keep the in-flight field values so the re-rendered modal shows what the
        # owner typed, and route the error to the modal-local assign (never the
        # method-editor's shared `save_error` — BT-2645).
        assign(socket,
          new_class_open: true,
          new_class_name: name,
          new_class_super: superclass,
          new_class_error: message
        )
    end
  end

  # An empty / blank superclass falls back to the default `Object` — the modal's
  # select defaults to it, but a crafted payload or cleared typeahead must not
  # synthesize a headerless `subclass: Name`.
  defp trim_superclass(superclass) do
    case String.trim(superclass) do
      "" -> "Object"
      trimmed -> trimmed
    end
  end

  # Validate the superclass field the same way as the class name (BT-2645): a bare
  # PascalCase identifier. The empty case is already normalised to `Object` by
  # `trim_superclass/1`. This rejects a crafted payload (e.g. embedded newlines /
  # syntax) locally, matching the name field rather than relying on the server
  # parser to reject the synthesized source.
  defp validate_superclass(superclass) do
    if Regex.match?(~r/^[A-Z][A-Za-z0-9_]*$/, superclass),
      do: :ok,
      else:
        {:error, "Superclass must be a class name starting with a capital letter, e.g. `Object`."}
  end

  # Validate a new class name locally (BT-2645): non-empty, PascalCase
  # (`^[A-Z][A-Za-z0-9_]*$`), and not a duplicate of an existing class in the
  # browse list. Returns `:ok` or `{:error, message}` for an in-modal error.
  @new_class_name_re ~r/^[A-Z][A-Za-z0-9_]*$/
  defp validate_new_class_name(socket, name) do
    cond do
      name == "" ->
        {:error, "Enter a class name to create a class."}

      not Regex.match?(@new_class_name_re, name) ->
        {:error,
         "Class name must be PascalCase — start with an uppercase letter, e.g. `Greeter`."}

      class_name_taken?(socket, name) ->
        {:error, "A class named #{name} already exists."}

      true ->
        :ok
    end
  end

  # Is `name` already a class in the loaded browse list? Compared against the
  # `"name"` field of every browse row (the same list the tree renders), so a
  # duplicate is caught before any round-trip.
  defp class_name_taken?(socket, name) do
    Enum.any?(socket.assigns[:browser_classes] || [], fn row ->
      Map.get(row, "name") == name
    end)
  end

  defp dispatch_new_class(socket, name, superclass, source, path) do
    case Facade.dispatch(:new_class, %{source: source, path: path}, ctx(socket)) do
      {:ok, created_path} ->
        socket
        |> assign_browser_classes()
        |> assign_changes()
        # Open + select the NEW class (`name`), not the superclass: the def tab
        # focuses it and the tree highlights it (BT-2645). `open_definition`
        # re-syncs the active-tab editor assigns (clearing `save_result`), so the
        # success status is assigned *after* it — otherwise the "Created …" banner
        # would be wiped by the very tab it opens.
        |> open_definition(name)
        |> assign(
          selected_class: name,
          selected_protocol: nil,
          save_result: "Created new class — #{created_path}",
          save_error: nil,
          flush_result: nil,
          flush_error: nil,
          new_class_open: false,
          new_class_error: nil
        )
        # BT-2586/BT-2590: a new class only writes its `.bt` file to disk when
        # autoflush is on (it is otherwise a durable in-memory ChangeLog entry,
        # written at the next flush — `maybe_autoflush(durable)` in
        # beamtalk_repl_loader). So reflect it in the git panel only when autoflush
        # is on, matching the save path; with autoflush off the working tree is
        # unchanged and the shell-out is skipped.
        |> maybe_refresh_git_after_save()

      {:error, reason} ->
        # Route a failed create to the in-modal error (keep fields + modal open),
        # never the method-editor's `save_error` (BT-2645).
        assign(socket,
          new_class_open: true,
          new_class_name: name,
          new_class_super: superclass,
          new_class_error: facade_error(reason)
        )
    end
  end

  # Derive the in-project `.bt` path for a new class from its name (BT-2293,
  # BT-2646): `Greeter` → `src/greeter.bt`, `EventStore` → `src/event_store.bt`.
  # The basename is snake_cased to match the project convention (every file in a
  # package `src/` is snake_case). The runtime's `newClass:at:` validation
  # snake_case-normalises both the declared class name and the path basename
  # (`beamtalk_repl_loader:validate_new_class/3` via `to_snake_case/1`), so a
  # snake_case file maps cleanly to the PascalCase class. The name is already
  # PascalCase-validated by the caller, so this always succeeds; it returns
  # `{:ok, path}` to keep the call-site explicit.
  #
  # The `src/` prefix is assumed — it's the canonical package source dir the
  # runtime resolves (`resolve_package_module` tries `src/` then `test/`). A
  # project with a different layout would get a `target_outside_project` error at
  # creation time (not silently on flush). If per-project source dirs ever land,
  # this is the spot to read the configured dir instead of hardcoding `src/`.
  defp derive_class_path(name) do
    {:ok, "src/" <> to_snake_case(name) <> ".bt"}
  end

  # Snake-case a PascalCase class name, mirroring the runtime's
  # `beamtalk_repl_loader:to_snake_case/1` EXACTLY so the IDE-derived filename and
  # the loader's basename normalisation agree (BT-2646). The rule: the first
  # character is lowercased unconditionally; thereafter an uppercase letter gets a
  # leading `_` ONLY when the previous character was a lowercase letter. This
  # collapses acronyms (`HTTPServer` → `httpserver`) rather than splitting every
  # capital — diverging from the runtime here would make the loader reject or
  # mis-locate the created file. Digits and other characters pass through verbatim
  # and do not count as "lowercase" for the boundary test.
  defp to_snake_case(name) do
    name
    |> String.to_charlist()
    |> snake_chars(false, [])
  end

  defp snake_chars([], _prev_was_lower?, acc), do: acc |> Enum.reverse() |> List.to_string()

  defp snake_chars([c | rest], prev_was_lower?, acc) when c >= ?A and c <= ?Z do
    lowered = c + 32

    if prev_was_lower? do
      snake_chars(rest, false, [lowered, ?_ | acc])
    else
      snake_chars(rest, false, [lowered | acc])
    end
  end

  defp snake_chars([c | rest], _prev_was_lower?, acc) do
    snake_chars(rest, c >= ?a and c <= ?z, [c | acc])
  end

  # Revert one pending method patch (BT-2293). On success the prior body is
  # re-installed (a fresh durable entry) and the Changes pane refreshes; a
  # non-revertable entry (new-class, class-side, no prior body) renders the
  # structured error the workspace returns.
  defp revert_change(socket, class, selector) do
    case Facade.dispatch(:revert, %{class: class, selector: selector}, ctx(socket)) do
      {:ok, reverted_class} ->
        socket
        |> assign(
          save_result: "Reverted #{selector} on #{reverted_class}",
          save_error: nil,
          flush_result: nil,
          flush_error: nil
        )
        |> assign_changes()

      {:error, reason} ->
        status_error(socket, facade_error(reason))
    end
  end

  # Set the active error line, clearing the other three status assigns so only
  # the most recent New Class / revert outcome shows in the shared status area
  # (BT-2293). Keeps the validation/error branches from leaving a stale flush
  # banner visible — the success branches already clear all four inline.
  defp status_error(socket, message) do
    assign(socket,
      save_result: nil,
      save_error: message,
      flush_result: nil,
      flush_error: nil
    )
  end

  # Drive the write-surface flush and render its summary, then refresh changes so
  # the (now-flushed) entries drop out of the active view.
  #
  # The flush reconciles every pending live `>>` patch with its on-disk body, so an
  # open `:method` tab whose patch was just written is no longer divergent — clear
  # its `unflushed` (`disk_differs`) breadcrumb badge (BT-2545). We diff the
  # pending method set captured *before* the flush against the set still pending
  # *after* (`assign_changes/1` already refreshed it): the difference is exactly
  # what this flush wrote, so a conflicted / non-flushable method keeps its badge
  # and a method untouched by this flush is never cleared.
  defp flush_changes(socket) do
    was_pending = pending_method_keys(socket.assigns.changes)

    case Facade.dispatch(:flush, %{}, ctx(socket)) do
      {:ok, summary} ->
        socket
        |> assign(flush_result: Workspace.format_flush_summary(summary), flush_error: nil)
        |> assign_changes()
        |> clear_flushed_badges(was_pending)
        # BT-2586: a flush writes pending edits to disk, so the post-flush git
        # panel must reflect them immediately (the pre→post-flush handoff).
        |> maybe_refresh_git()

      {:error, reason} ->
        assign(socket, flush_result: nil, flush_error: facade_error(reason))
    end
  end

  # Refresh the git panel only when it is the active dock tab. A flush that
  # happens while the user is on another tab leaves git untouched (it reloads
  # lazily on next open), so the common edit loop never pays for an extra git
  # shell-out it can't see. Used by the flush path, which always changes disk.
  defp maybe_refresh_git(socket) do
    if socket.assigns.dock_tab == "git", do: assign_git(socket), else: socket
  end

  # BT-2590 (S2): refresh the git panel after a *save* only when autoflush is on.
  # With autoflush off a per-method save patches the live image only — the on-disk
  # working tree is unchanged, so the git shell-out would return an identical
  # result and is pure waste. When autoflush is on the save wrote through to disk,
  # so the panel must reflect it (gated, as ever, on the Git tab being active).
  defp maybe_refresh_git_after_save(socket) do
    if socket.assigns.autoflush, do: maybe_refresh_git(socket), else: socket
  end

  @doc false
  # The `(class, selector)` set of the active ChangeLog rows — the methods with an
  # unflushed live `>>` patch. Keyed by `(class, selector)` only; the ChangeLog
  # carries no instance/class side, so that is the finest granularity available.
  # Pure; unit-tested directly (cf. `Workspace.format_flush_summary/1`).
  def pending_method_keys(changes) when is_list(changes) do
    for %{class: class, selector: selector} <- changes,
        is_binary(class),
        is_binary(selector),
        into: MapSet.new(),
        do: {class, selector}
  end

  def pending_method_keys(_), do: MapSet.new()

  @doc false
  # The `(class, selector)` keys this flush actually wrote: pending before
  # (`was_pending`) and gone from the refreshed `changes` after. A failed post-flush
  # refresh assigns `changes: []` *alongside* a `changes_error`; that empty set
  # means "couldn't read the ChangeLog", not "everything flushed", so the difference
  # would collapse to the full before-set and clear every badge — including
  # conflicts / skips never written to disk. On an errored refresh we therefore
  # return the empty set and clear nothing (it self-heals on the next clean
  # refresh). Pure; unit-tested.
  def flushed_method_keys(_was_pending, _changes, changes_error) when not is_nil(changes_error),
    do: MapSet.new()

  def flushed_method_keys(was_pending, changes, _changes_error),
    do: MapSet.difference(was_pending, pending_method_keys(changes))

  @doc false
  # Clear the `unflushed` (`disk_differs`) badge on every open tab whose disk key is
  # in `flushed`: a `:method` tab keys on `{class, selector}` (the methods a flush
  # reconciled to disk); a `:def` tab keys on `{class, :def}` (a class header a
  # revert reloaded — BT-2600). Other tabs are returned unchanged: a still-pending
  # conflict/skip (outside `flushed`) or an untouched method/def. The flush path
  # only ever passes `{class, selector}` method keys, so `:def` tabs are untouched
  # by a flush exactly as before. Pure; unit-tested.
  def clear_disk_differs(tabs, flushed) do
    Enum.map(tabs, fn tab ->
      if MapSet.member?(flushed, tab_disk_key(tab)) do
        %{tab | disk_differs: false}
      else
        tab
      end
    end)
  end

  # The disk key a tab is cleared by: `{class, selector}` for a `:method` tab,
  # `{class, :def}` for a `:def` tab (the `:def` sentinel can't collide with a real
  # binary selector). Any other shape yields `nil`, which is never a set member.
  defp tab_disk_key(%{kind: :method, class: class, selector: selector}), do: {class, selector}
  defp tab_disk_key(%{kind: :def, class: class}), do: {class, :def}
  defp tab_disk_key(_tab), do: nil

  # After a flush, clear the `unflushed` badge on the `:method` tabs this flush wrote
  # to disk (BT-2545), scoped by `flushed_method_keys/3` so conflicts / skips keep
  # their badge and methods untouched by this flush are never cleared.
  defp clear_flushed_badges(socket, was_pending) do
    flushed =
      flushed_method_keys(was_pending, socket.assigns.changes, socket.assigns[:changes_error])

    if MapSet.size(flushed) == 0 do
      socket
    else
      assign(socket, :tabs, clear_disk_differs(socket.assigns.tabs, flushed))
    end
  end

  # Read the active ChangeLog ("Workspace changes", ADR 0082) and assign display
  # rows. A workspace that is unreachable or returns an unexpected shape renders an
  # error rather than crashing the pane.
  #
  # Split into the off-socket read (`read_changes/1`) + the pure on-socket fold
  # (`apply_changes/2`) so the mount-time async load (BT-2591) and the post-action
  # refresh callers share one fold. The sync helper keeps its old signature.
  defp assign_changes(socket), do: apply_changes(socket, read_changes(socket))

  # The raw `:changes` dispatch result — runs off the LiveView process in the
  # mount-load task, so it captures only `ctx`, never `socket`.
  defp read_changes(socket), do: Facade.dispatch(:changes, %{}, ctx(socket))

  # Fold a completed `:changes` read into the socket. Pure (no dispatch); shared by
  # `handle_async(:mount_load, …)` and the sync refresh path.
  defp apply_changes(socket, rows) when is_list(rows) do
    # Prune expanded-diff carets for rows that have left @changes (flush /
    # revert), so a re-saved method doesn't re-appear already-expanded.
    live_keys = MapSet.new(rows, &{&1.class, &1.selector})
    expanded = MapSet.intersection(socket.assigns.expanded_changes, live_keys)
    assign(socket, changes: rows, changes_error: nil, expanded_changes: expanded)
  end

  defp apply_changes(socket, {:error, reason}) do
    assign(socket,
      changes: [],
      changes_error: Workspace.render_error(reason),
      expanded_changes: MapSet.new()
    )
  end

  # Defensive catch-all (BT-2591): folded from the async mount load AND the sync
  # `assign_changes/1` refresh path (post-flush/class-load/revert). An unexpected
  # shape degrades to an empty pane with an error rather than crashing the LiveView.
  defp apply_changes(socket, unexpected) do
    Logger.warning("unexpected changes result: #{inspect(unexpected)}",
      domain: [:beamtalk, :liveview]
    )

    assign(socket,
      changes: [],
      changes_error: Workspace.render_error(:unexpected_response),
      expanded_changes: MapSet.new()
    )
  end

  # BT-2598: the live image changed (a class was (re)loaded or removed). Re-pull
  # every source-dependent surface so open windows reflect the new image without a
  # manual refresh: the browser class list, the active ChangeLog, all open clean
  # editor tabs (method + definition), and the git panel when it is the active
  # dock tab (a revert's reload follows a disk change git should reflect too).
  # Driven by the `classes` push handler, so a git revert, another session's
  # flush, or an external edit reloaded into the image all converge here.
  defp refresh_after_source_change(socket) do
    socket
    |> assign_browser_classes()
    |> assign_changes()
    |> refresh_open_source_tabs()
    |> maybe_refresh_git()
  end

  # BT-2600: schedule a single coalesced `refresh_after_source_change` for a burst
  # of `ClassLoaded`/`ClassRemoved` pushes. The first push arms the deferred
  # `:do_source_refresh` and sets `:source_refresh_pending`; intervening pushes
  # collapse into it (no-op) while the flag is set — so a project sync reloading N
  # files refreshes the source-dependent surfaces once, not N times. Mirrors the
  # `{:object_changed, …}` `:refresh_pending` debounce. Direct callers
  # (e.g. the user-initiated git revert) still refresh synchronously — coalescing
  # is only for the unsolicited push burst.
  defp schedule_source_refresh(%{assigns: %{source_refresh_pending: true}} = socket), do: socket

  defp schedule_source_refresh(socket) do
    Process.send_after(self(), :do_source_refresh, refresh_debounce_ms())
    assign(socket, source_refresh_pending: true)
  end

  # BT-2598: re-read every open *clean* editor tab from the live image so a source
  # change that landed out-of-band (a git revert's reload, another session's
  # flush, an MCP edit) is reflected in the visible buffer. A `dirty` tab is left
  # untouched — never clobber the user's in-progress work — exactly as the
  # re-activation re-read in `open_method_tab/4` / `open_definition/2` does; this
  # generalises that pull into a push so the user need not re-focus the tab. An
  # empty-source fallback (a since-removed method/class, a transient facade error)
  # keeps the existing buffer rather than blanking a tab the user is looking at.
  defp refresh_open_source_tabs(socket) do
    tabs = Enum.map(socket.assigns.tabs, &refresh_source_tab(socket, &1))

    socket
    |> assign(:tabs, tabs)
    |> resync_active_tab(tabs)
  end

  # Re-read one tab's source from the image. Clean `:method`/`:def` tabs refresh;
  # dirty tabs and any other shape pass through unchanged.
  defp refresh_source_tab(socket, %{kind: :method, dirty: false} = tab) do
    case method_source_info(socket, tab.class, tab.side, tab.selector) do
      %{source: ""} ->
        tab

      info ->
        %{
          tab
          | source: info.source,
            base: info.source,
            # Pick up *new* divergence from an out-of-band patch, but never clear a
            # divergence already set locally (mirrors the re-activation invariant).
            disk_differs: tab.disk_differs or info.disk_differs,
            runtime_only: info.runtime_only,
            disk_source: reactivation_disk_source(tab, info),
            doc: info.doc,
            signature: info.signature,
            native_delegate: info.native_delegate
        }
    end
  end

  defp refresh_source_tab(socket, %{kind: :def, dirty: false} = tab) do
    # Only the read-only doc block is re-read (the editable definition buffer is
    # left untouched), matching the `:def` re-activation re-read. A failed re-fetch
    # keeps the prior backing module rather than hiding the "Erlang backend" badge.
    # BT-2605: the reflected modifier badges (sealed/abstract/native) are refreshed
    # from the same fetch so a recompile-into/out-of a modifier shows on push
    # refresh; a transient failure (native_module nil) keeps the prior native flag.
    {_definition, comment, native_module, class_modifiers, is_protocol} =
      class_definition_info(socket, tab.class)

    resolved_native = native_module || tab.native_module

    %{
      tab
      | doc: comment,
        is_protocol: is_protocol,
        native_module: resolved_native,
        # `nil` modifiers signal a transient fetch failure — keep the prior list
        # rather than clearing the badges (BT-2605 review).
        class_modifiers: class_modifiers || tab.class_modifiers,
        class_native: is_binary(resolved_native) and resolved_native != ""
    }
  end

  defp refresh_source_tab(_socket, tab), do: tab

  # Keep the rendered active-tab editor in sync after a push refresh: re-`sync_active`
  # the active tab so its breadcrumb/badges/doc block re-render from the refreshed
  # entry. A dirty active tab is untouched above, so this never disturbs an edit.
  defp resync_active_tab(socket, tabs) do
    case Enum.find(tabs, &(&1.id == socket.assigns[:active_tab])) do
      %{dirty: false} = active ->
        # BT-2655: if the re-read changed the active tab's *body* (a git revert, a
        # push reconcile), bump `editor_rev` so the `phx-update="ignore"` CodeMirror
        # host is re-keyed and remounts with the new source. A no-op re-read (same
        # body) leaves the rev — and thus the live editor instance — untouched, so a
        # routine refresh of an unchanged tab never disturbs the editor.
        socket
        |> maybe_bump_editor_rev(active.source)
        |> sync_active(active)

      _ ->
        socket
    end
  end

  defp maybe_bump_editor_rev(socket, new_source) do
    if new_source == socket.assigns[:edit_source] do
      socket
    else
      assign(socket, :editor_rev, socket.assigns.editor_rev + 1)
    end
  end

  # ── Git panel data source (ADR 0082 Amendment 1, BT-2586) ────────────────────

  # Load the post-flush git surface: working-tree status + recent log from real
  # git on the workspace project root.
  #
  # BT-2590 (S1): both git ops cross the node boundary into the workspace's
  # `collect/5` loop, each bounded by the workspace's `GIT_TIMEOUT_MS = 30_000`, so
  # a hung git could otherwise leave the LiveView socket unresponsive for ~60s per
  # call — blocking tab switches, saves, and clicks. We therefore run the two reads
  # off-socket in a single `start_async` task: the socket stays responsive while
  # git runs, the panel shows its "Loading git status…" placeholder, and the
  # results land in `handle_async(:git_load, …)`. A rapid second Refresh first
  # `cancel_async`-es the in-flight load (killing the LiveView-side Task so only
  # the latest load can update the panel — the workspace-side RPC may still
  # complete, but its response is discarded) and then starts the fresh
  # one — only the latest load wins the panel, and the LiveView is never blocked.
  #
  # A workspace that is unreachable, not a git repo, or missing `git` renders an
  # error rather than crashing the pane — the graceful-degradation requirement. We
  # clear any stale per-file diff and reset to the loading state on each refresh.
  defp assign_git(socket) do
    ctx = ctx(socket)

    socket
    |> assign(git_diff_path: nil, git_diff: nil, git_status: nil, git_log: [], git_error: nil)
    |> cancel_async(:git_load, :cancelled)
    |> start_async(:git_load, fn ->
      # Runs in a Task off the LiveView process — never touch `socket` here, only
      # the captured `ctx`. Both reads are gathered so the panel updates atomically.
      {Facade.dispatch(:git_status, %{}, ctx), Facade.dispatch(:git_log, %{count: 20}, ctx)}
    end)
  end

  # Apply a completed git status read to the socket. Pure (no dispatch); shared by
  # `handle_async/3` so the async result path and any future sync caller agree.
  # Kept total — an unexpected shape (an off-vocabulary facade reply, a malformed
  # status) degrades to a panel error rather than crashing the LiveView.
  defp apply_git_status(socket, {:ok, status}) when is_map(status),
    do: assign(socket, git_status: status, git_error: nil)

  defp apply_git_status(socket, {:error, reason}),
    do: assign(socket, git_status: nil, git_error: facade_error(reason))

  defp apply_git_status(socket, _other),
    do: assign(socket, git_status: nil, git_error: facade_error(:unexpected_git_status))

  # Apply a completed git log read.
  defp apply_git_log(socket, {:ok, commits}) when is_list(commits),
    do: assign(socket, git_log: commits)

  defp apply_git_log(socket, {:error, reason}),
    do: log_failed(socket, facade_error(reason))

  defp apply_git_log(socket, _other),
    do: log_failed(socket, facade_error(:unexpected_git_log))

  # A git-log read failed. Clear the list, and surface the error only if the
  # status read hasn't already reported one — when both fail together the status
  # pane already shows the degraded state, but a fast status beside an
  # independently-failed log (e.g. a large-history timeout) would otherwise leave
  # a valid branch next to a mysteriously empty commit list with no explanation.
  defp log_failed(socket, error) do
    socket = assign(socket, git_log: [])

    if socket.assigns.git_error,
      do: socket,
      else: assign(socket, git_error: error)
  end

  # BT-2590 (S2): read the workspace `autoflush` flag once at mount via the read
  # facade (so RBAC/audit apply uniformly). The client defaults a degraded read to
  # `false`, and an off-vocabulary/denied dispatch (`{:error, _}`) also falls back
  # to `false` — never crash the mount on the settings probe.
  #
  # BT-2591: the read now runs in the off-socket `:mount_load` task; this pure
  # fold applies its result (and any future sync caller's).
  defp apply_autoflush(socket, flag) when is_boolean(flag), do: assign(socket, :autoflush, flag)
  defp apply_autoflush(socket, _other), do: assign(socket, :autoflush, false)

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
    ctx = ctx(socket)

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

  # BT-2619: fold one mount read into its surface's assigns only if a *successful*
  # live push hasn't already loaded that surface. `loaded_key` is the per-surface
  # flag (`:source_loaded` / `:bindings_loaded`): it is `true` only when a push
  # refresh succeeded, so a `false` flag means either no push landed yet OR a push
  # landed but errored — in both cases the mount read (which carries real,
  # successful data here) should win. We do NOT clear the flag here; the caller
  # sets all flags `true` after the fold so the post-mount sync refresh path
  # remains the source of truth.
  defp fold_mount_read(socket, loaded_key, read, apply_fun) do
    if socket.assigns[loaded_key], do: socket, else: apply_fun.(socket, read)
  end

  # BT-2619: mark the source-dependent surfaces (browser_classes + changes) as
  # loaded by a push — but ONLY when the push refresh actually succeeded (neither
  # surface holds an error). An errored push leaves the flag `false` so a
  # later-completing mount fold's successful data can still replace the transient
  # error (no lingering error flash). Idempotent: re-marking after a later success
  # just re-affirms `true`.
  defp mark_source_loaded(socket) do
    if is_nil(socket.assigns.browser_error) and is_nil(socket.assigns.changes_error),
      do: assign(socket, :source_loaded, true),
      else: socket
  end

  # BT-2619: mark the bindings surface as loaded by a push — only on a successful
  # refresh (no `bindings_error`), mirroring `mark_source_loaded/1`.
  defp mark_bindings_loaded(socket) do
    if is_nil(socket.assigns.bindings_error),
      do: assign(socket, :bindings_loaded, true),
      else: socket
  end

  # ── Test-runner pane data source (BT-2557) ──────────────────────────────────

  # Load the test catalogue once (lazy first open of the Tests tab). Re-opening
  # the tab keeps the already-loaded list — use `tests_refresh` to re-discover.
  defp ensure_test_classes(socket) do
    if is_nil(socket.assigns.test_classes),
      do: discover_test_classes(socket),
      else: socket
  end

  # Discover the live image's TestCase subclasses + selectors (`list_tests`,
  # `:read`). Although `:read` reflection is usually fast, it is still a blocking
  # workspace RPC: against a slow/unresponsive node the ~5s timeout would stall
  # the LiveView process (first Tests-tab open / every manual Refresh). So it
  # runs off-socket in a `:test_discover` `start_async` task, mirroring the test
  # run/load `:test_op` (BT-2597) and the git panel's `:git_load` (BT-2590). A
  # rapid double-refresh / open-then-refresh `cancel_async`-es the prior probe so
  # only the latest result wins; the result lands in
  # `handle_async(:test_discover, …)`. The `test_classes` nil sentinel is
  # preserved meanwhile so the pane shows its "discovering" state rather than the
  # misleading "No TestCase subclasses" empty-state.
  # `keep_error?` is set by the load-tests re-discovery path: a partial load has
  # already populated `tests_error` with its compile-error summary, and a
  # *successful* discovery must NOT clear it (it would swallow the partial-load
  # banner). The flag rides a transient assign that `handle_async/3` consumes.
  defp discover_test_classes(socket, keep_error? \\ false) do
    ctx = ctx(socket)

    socket
    |> assign(:tests_discover_keep_error, keep_error?)
    |> cancel_async(:test_discover, :cancelled)
    |> start_async(:test_discover, fn ->
      # Off the LiveView process — capture only `ctx`, never `socket`.
      Facade.dispatch(:list_tests, %{}, ctx)
    end)
  end

  # Apply a completed `list_tests` dispatch to the socket. Pure (no dispatch);
  # shared by `handle_async(:test_discover, …)` so the async path and the
  # load-tests re-discovery agree (mirrors `apply_test_result/2` and
  # `apply_git_status/2`). A dispatch failure / RBAC denial renders a
  # `tests_error` rather than crashing the pane, mirroring `apply_changes/2`.
  #
  # On success we normally clear `tests_error` (a stale failure heals), but when
  # `keep_error?` is true (a partial load is showing its compile-error summary)
  # we leave `tests_error` intact so the banner survives the re-discovery.
  defp apply_test_classes(socket, {:ok, classes}, keep_error?) when is_list(classes) do
    socket = assign(socket, :test_classes, classes)
    if keep_error?, do: socket, else: assign(socket, :tests_error, nil)
  end

  defp apply_test_classes(socket, {:error, reason}, _keep_error?),
    # Leave the catalogue as the nil sentinel (not []) so the pane shows only the
    # error — not the misleading "No TestCase subclasses" empty-state — and so
    # re-opening the tab retries discovery (a transient failure heals).
    do: assign(socket, test_classes: nil, tests_error: facade_error(reason))

  defp apply_test_classes(socket, _other, _keep_error?),
    do: assign(socket, test_classes: nil, tests_error: facade_error(:unexpected_test_result))

  # Run all tests (`class` = nil) or a single class (`run_tests`, `:execute`).
  #
  # BT-2597: the run compiles + evaluates user code on the workspace node, which
  # can take seconds for a large suite — so it runs off-socket in a `:test_op`
  # `start_async` task (mirroring the git panel's `:git_load`, BT-2590) rather
  # than blocking the LiveView process. A rapid second action `cancel_async`-es
  # the in-flight op so only the latest result wins. The result lands in
  # `handle_async(:test_op, …)`; `tests_running` disables the controls meanwhile.
  defp run_tests(socket, class) do
    ctx = ctx(socket)

    socket
    |> assign(tests_running: true, tests_error: nil)
    |> cancel_async(:test_op, :cancelled)
    |> start_async(:test_op, fn ->
      # Off the LiveView process — capture only `ctx`, never `socket`.
      {:run, Facade.dispatch(:run_tests, %{class: class}, ctx)}
    end)
  end

  # Load the project's test/ files (`load_tests`, `:execute`), then re-discover
  # the catalogue so the newly-loaded TestCase subclasses appear immediately.
  #
  # BT-2597: like `run_tests/2`, the load compiles user code, so it runs in the
  # off-socket `:test_op` task; the result lands in `handle_async(:test_op, …)`.
  defp load_tests(socket) do
    ctx = ctx(socket)

    socket
    |> assign(tests_running: true, tests_error: nil)
    |> cancel_async(:test_op, :cancelled)
    |> start_async(:test_op, fn ->
      {:load, Facade.dispatch(:load_tests, %{}, ctx)}
    end)
  end

  # Apply a completed `run_tests` dispatch to the socket. Pure (no dispatch);
  # shared by `handle_async/3` so the async path and any future sync caller agree
  # (mirrors `apply_git_status/2`). An error (incl. a non-Owner RBAC denial)
  # surfaces as `tests_error` and clears any stale results.
  defp apply_test_result(socket, {:ok, result}) when is_map(result),
    do: assign(socket, test_results: result, tests_error: nil)

  defp apply_test_result(socket, {:error, reason}),
    do: assign(socket, test_results: nil, tests_error: facade_error(reason))

  defp apply_test_result(socket, _other),
    do: assign(socket, test_results: nil, tests_error: facade_error(:unexpected_test_result))

  # Apply a completed `load_tests` dispatch: refresh the catalogue to show
  # whatever loaded, surfacing partial compile errors as `tests_error`. The
  # re-discovery is kicked off via the off-socket `:test_discover` task
  # (`discover_test_classes/2`) so the fold never blocks the LiveView process.
  #
  # We reset `test_classes` to the nil sentinel so the catalogue shows its
  # "discovering" state until the off-socket re-discovery resolves with the
  # freshly-loaded classes, and pass `keep_error?: true` so the later
  # `handle_async(:test_discover, …)` fold doesn't clear this partial-load
  # banner on a successful re-discovery.
  defp apply_test_load(socket, {:ok, %{"errors" => [_ | _] = errors}}),
    do:
      socket
      |> assign(test_classes: nil, tests_error: load_tests_error(errors))
      |> discover_test_classes(true)

  # A clean load simply re-discovers the catalogue off-socket; the
  # `handle_async(:test_discover, …)` fold clears any stale `tests_error` on
  # success (via `apply_test_classes/3`) and sets it on failure.
  defp apply_test_load(socket, {:ok, _result}),
    do: socket |> assign(test_classes: nil) |> discover_test_classes()

  defp apply_test_load(socket, {:error, reason}),
    do: assign(socket, tests_error: facade_error(reason))

  defp apply_test_load(socket, _other),
    do: assign(socket, tests_error: facade_error(:unexpected_test_result))

  # Summarise compile errors from a partial test load into one line. Each error
  # is a `%{"path" => ..., "message" => ...}` map (the load-project error shape).
  defp load_tests_error(errors) do
    count = length(errors)
    first = errors |> List.first() |> Map.get("message", "")
    "#{count} test file(s) failed to load: #{first}"
  end

  # Render the aggregate run duration (seconds, from the runtime TestResult) in a
  # human unit: sub-second runs in ms, longer runs in seconds. A non-number (an
  # unexpected wire shape) renders nothing rather than crashing the summary.
  defp format_test_duration(seconds) when is_number(seconds) and seconds < 1.0 do
    "#{round(seconds * 1000)} ms"
  end

  defp format_test_duration(seconds) when is_number(seconds) do
    "#{:erlang.float_to_binary(seconds * 1.0, decimals: 2)} s"
  end

  defp format_test_duration(_), do: ""

  # Per-class pass/fail tally from the last run, keyed by class name, so the
  # catalogue can show "2✓ 1✗" next to each class without re-running. Returns nil
  # when there are no results yet or the class had no cases in the last run.
  defp test_class_tally(nil, _class), do: nil

  defp test_class_tally(test_results, class) when is_map(test_results) do
    cases = for t <- test_results["tests"] || [], t["class"] == class, do: t["status"]

    case cases do
      [] ->
        nil

      _ ->
        %{
          passed: Enum.count(cases, &(&1 == "pass")),
          failed: Enum.count(cases, &(&1 == "fail")),
          skipped: Enum.count(cases, &(&1 == "skip"))
        }
    end
  end

  # Short status glyph for a per-case result row.
  defp test_status_label("pass"), do: "✓ pass"
  defp test_status_label("fail"), do: "✗ fail"
  defp test_status_label("skip"), do: "○ skip"
  # An unanticipated status from the runner still gets a visible "?" label rather
  # than rendering the raw atom text unadorned.
  defp test_status_label(other), do: "? " <> other

  # CSS class suffix for a per-case status. Only the three known statuses carry a
  # styled rule (`.st-pass` / `.st-fail` / `.st-skip`); an unknown status falls
  # back to the neutral skip style so a row is never left unstyled with a raw
  # `st-<atom>` class that has no matching rule.
  defp test_status_class(status) when status in ~w(pass fail skip), do: "st-" <> status
  defp test_status_class(_other), do: "st-skip"

  # ── System Browser data source (BT-2491, browse ops ADR 0096) ───────────────
  #
  # The four browse ops return a `{:value, json_value}` live term verbatim
  # (wire-shaped maps/lists of binaries — JSON only at the WebSocket edge, never
  # here) or `{:error, reason}`. Each `assign_*`/`load_*` helper unwraps that and
  # holds the rows in browser assigns the render walks; a dispatch failure or an
  # RBAC denial renders a `browser_error` rather than crashing the pane.

  # Load every class in scope for the class tree (op 1, `browse-classes`). Sorted
  # workspace-side; the rows carry `superclass`/`category`/`origin` so the
  # Hierarchy and Category views and the runtime badge render off one fetch.
  #
  # Split into the off-socket read + pure fold (BT-2591) so the mount-load async
  # task and the post-action refresh callers share one fold.
  defp assign_browser_classes(socket),
    do: apply_browser_classes(socket, read_browser_classes(socket))

  defp read_browser_classes(socket), do: Facade.dispatch(:browse_classes, %{}, ctx(socket))

  defp apply_browser_classes(socket, {:value, rows}) when is_list(rows) do
    socket
    |> assign(browser_classes: rows, browser_error: nil)
    |> apply_default_browser_source(rows)
  end

  defp apply_browser_classes(socket, {:error, reason}),
    do: assign(socket, browser_classes: [], browser_error: facade_error(reason))

  # Defensive catch-all (BT-2591): this fold runs in `handle_async(:mount_load,
  # …)` AND on the sync `assign_browser_classes/1` refresh path. An unexpected
  # dispatch shape (a facade evolution, a bare atom) would otherwise crash the
  # LiveView — on the async path *after* the empty page rendered, harder to spot
  # than the old mount-time crash. Degrade to an empty tree with an error instead.
  defp apply_browser_classes(socket, unexpected) do
    Logger.warning("unexpected browse_classes result: #{inspect(unexpected)}",
      domain: [:beamtalk, :liveview]
    )

    assign(socket, browser_classes: [], browser_error: facade_error(:unexpected_response))
  end

  # BT-2661: apply the one-shot initial origin-filter default once the class rows
  # arrive. The tree opens scoped to the project's own classes ("show me my code")
  # — but only on the FIRST successful load (`:browser_source_chosen` still false)
  # and only when there is at least one project-origin class to show; a bare /
  # stdlib-only workspace falls back to "all" so the tree isn't empty on open. The
  # flag flips `true` here (and in the `browser_source` handler when the user picks
  # a filter), so a later async refresh / live push never resets a deliberate
  # choice — the chosen value always wins over the default.
  defp apply_default_browser_source(%{assigns: %{browser_source_chosen: true}} = socket, _rows),
    do: socket

  defp apply_default_browser_source(socket, rows) do
    assign(socket,
      browser_source: default_browser_source(rows),
      browser_source_chosen: true
    )
  end

  # "project" when the workspace has any project-origin class, else "all" (the
  # empty-project fallback). The `source_origin` field is the bare classification
  # (BT-2643); project rows carry "project" (the `source_origin_class/1` default
  # bucket), so an explicit equality match is enough.
  defp default_browser_source(rows) do
    if Enum.any?(rows, &(Map.get(&1, "source_origin") == "project")),
      do: "project",
      else: "all"
  end

  # BT-2648/BT-2656: load the loaded packages' hand-written native Erlang modules
  # for the separate Native browser. Each row carries `module`, `source_file`,
  # `package`, `source_origin`, and `openable`. A dispatch failure / RBAC denial
  # yields an empty list rather than crashing the browser — the class tree (the
  # primary navigator) must still render.
  defp assign_browser_native_modules(socket) do
    rows =
      case Facade.dispatch(:browse_native_modules, %{}, ctx(socket)) do
        {:value, rows} when is_list(rows) -> rows
        _ -> []
      end

    socket
    |> assign(browser_native_modules: rows)
    |> apply_default_native_source(rows)
  end

  # BT-2656/BT-2661: apply the one-shot Project-origin default to the Native browser
  # once the module rows arrive, mirroring `apply_default_browser_source/2` for the
  # class tree. Only on the FIRST load (`:native_source_chosen` still false) and only
  # when there is at least one project-origin native module to show; otherwise it
  # falls back to "all" so the list isn't empty on open. A deliberate pick flips the
  # flag in the `native_source` handler so a later refresh never resets it.
  defp apply_default_native_source(%{assigns: %{native_source_chosen: true}} = socket, _rows),
    do: socket

  defp apply_default_native_source(socket, rows) do
    assign(socket,
      native_source: default_browser_source(rows),
      native_source_chosen: true
    )
  end

  # Load `class`/`side`'s selectors grouped by protocol (op 2, `browse-protocols`)
  # for the protocol filter row + method list. The `protocols` list each carry a
  # `name` and `selectors`; an unknown class / bad side comes back as a structured
  # error we surface without blanking the rest of the pane.
  defp load_protocols(socket, class, side) do
    case Facade.dispatch(:browse_protocols, %{class: class, side: side}, ctx(socket)) do
      {:value, %{"protocols" => protocols}} when is_list(protocols) ->
        assign(socket, browser_protocols: protocols, browser_error: nil)

      {:value, _other} ->
        assign(socket, browser_protocols: [], browser_error: nil)

      {:error, reason} ->
        assign(socket, browser_protocols: [], browser_error: facade_error(reason))
    end
  end

  # ── navigation aids: omni search + senders/implementors (BT-2495) ───────────

  # Filter the workspace symbol index (`nav-symbols`) against the live query and
  # open the results popover. The index — every loaded class plus its locally
  # defined selectors — is fetched per search (the search is debounced, and a live
  # re-read keeps a mid-session class definition findable) and flattened into
  # ranked rows. An empty query closes the popover; the OmniSearch hook walks the
  # `.active` highlight over `:omni_results` client-side.
  defp run_omni_search(socket, query) do
    trimmed = String.trim(query)

    if trimmed == "" do
      close_omni(assign(socket, omni_query: query))
    else
      results = omni_filter(symbol_rows(socket), trimmed)
      assign(socket, omni_query: query, omni_results: results, omni_open: true)
    end
  end

  # Flatten the `nav-symbols` outline into search rows: one row per class, plus
  # one per locally-defined selector (instance- and class-side). Each row carries
  # the identity the popover needs to open it — a class row opens the System
  # Browser, a selector row opens an editable method tab. A dispatch failure /
  # RBAC denial yields an empty index rather than crashing the search.
  defp symbol_rows(socket) do
    case Facade.dispatch(:symbols, %{scope: "all"}, ctx(socket)) do
      {:value, %{"classes" => classes}} when is_list(classes) ->
        Enum.flat_map(classes, &class_symbol_rows/1)

      _ ->
        []
    end
  end

  defp class_symbol_rows(%{"name" => name} = class) when is_binary(name) do
    class_row = %{
      kind: "class",
      label: name,
      class: name,
      side: "instance",
      selector: nil
    }

    method_rows =
      for m <- Map.get(class, "methods", []),
          sel = Map.get(m, "selector"),
          is_binary(sel) do
        # Match the boolean exactly: a string "false" would be truthy and mis-tag
        # every instance-side method as class-side (wrong tab + wrong source read).
        side = if Map.get(m, "class_side") == true, do: "class", else: "instance"

        %{
          kind: "selector",
          label: name <> " » " <> sel <> side_suffix(side),
          class: name,
          side: side,
          selector: sel
        }
      end

    [class_row | method_rows]
  end

  defp class_symbol_rows(_), do: []

  defp side_suffix("class"), do: " (class)"
  defp side_suffix(_), do: ""

  # Case-insensitive substring match, ranked prefix-first then alphabetically, and
  # capped so a one-letter query can't render thousands of rows into the popover.
  defp omni_filter(rows, query) do
    q = String.downcase(query)

    rows
    |> Enum.filter(&String.contains?(String.downcase(&1.label), q))
    |> Enum.sort_by(fn row ->
      label = String.downcase(row.label)
      {if(String.starts_with?(label, q), do: 0, else: 1), label}
    end)
    |> Enum.take(30)
  end

  # Close the omni-search popover and clear its results.
  defp close_omni(socket), do: assign(socket, omni_open: false, omni_results: [])

  # Open a class in the System Browser (the omni-search "class" result): select it
  # in the tree and load its protocols for the current side, exactly as a click in
  # the class tree would.
  defp open_class(socket, class) do
    socket
    |> assign(selected_class: class, selected_protocol: nil)
    |> load_protocols(class, socket.assigns.browser_side)
  end

  # Point the System Browser at a class/side — select it in the tree, flip the
  # instance/class toggle to match, clear the protocol filter, and load its
  # protocols. Used when a method is opened from *outside* the browser (a
  # Senders/Implementors jump to another class) so the pane tracks the focused
  # tab, per the "browser highlights whatever the focused tab shows" design.
  defp navigate_browser(socket, class, side)
       when is_binary(class) and is_binary(side) do
    socket
    |> assign(selected_class: class, browser_side: side, selected_protocol: nil)
    |> load_protocols(class, side)
  end

  # Open (or re-focus) an *editable* method tab for class/side/selector — the
  # shared open path for an omni-search selector result and a senders/implementors
  # site. Mirrors `open_definition/1`'s find-or-create-then-focus shape; the tab id
  # is the same `method:Class:side:selector` key the editor already uses, so
  # opening the same method twice de-dupes. The buffer is seeded with the method's
  # image-accurate source so editing starts from the live body.
  defp open_method_tab(socket, class, side, selector) do
    id = "method:" <> class <> ":" <> side <> ":" <> selector

    case find_tab(socket, id) do
      %{dirty: true} ->
        # Unsaved edits live in the buffer — just refocus; never re-seed source or
        # clobber the user's in-progress work.
        activate_tab(socket, id)

      %{} = existing ->
        # Tab already open and clean: re-fetch the live image so the breadcrumb badges
        # reflect an out-of-band image patch (e.g. an image compile / MCP `save_method`)
        # that landed while the tab sat open, instead of the snapshot taken at first
        # open. On an empty-source fallback — a transient facade error or a
        # since-deleted method — keep the existing buffer rather than blanking a tab the
        # user is looking at (the precondition is a clean tab, so nothing typed is lost,
        # but the visible source should not silently vanish).
        case method_source_info(socket, class, side, selector) do
          %{source: ""} ->
            activate_tab(socket, id)

          info ->
            refreshed = %{
              existing
              | source: info.source,
                base: info.source,
                # Pick up *new* divergence (false → true) from an out-of-band patch,
                # but never clear a divergence already set locally by an in-memory
                # compile (`compile_clean/3`): clearing on flush is BT-2545's path.
                # Keeps the false → true invariant the `nil ->` branch documents.
                disk_differs: existing.disk_differs or info.disk_differs,
                runtime_only: info.runtime_only,
                # BT-2714: re-derive derived-ness from the live image too, so a
                # tab that (out-of-band) became / stopped being synthetic tracks it.
                synthetic: info.synthetic,
                # Carry the on-disk body forward across the re-activation. A fresh
                # snapshot wins when the image is back in sync with disk; a method
                # whose image diverged but is *still* disk-backed keeps the prior
                # snapshot instead of regressing to nil (BT-2565); a now runtime-only
                # method drops to nil (BT-2550).
                disk_source: reactivation_disk_source(existing, info),
                # Re-read the doc block from the live image too (BT-2558), so an
                # out-of-band edit to the method's `///` doc / signature is
                # reflected when the clean tab is re-activated.
                doc: info.doc,
                signature: info.signature,
                # Re-derive the native-delegate flag from the live image too.
                native_delegate: info.native_delegate
            }

            socket
            |> update_active_tab_by_id(id, fn _ -> refreshed end)
            |> assign(:active_tab, id)
            |> sync_active(refreshed)
        end

      nil ->
        info = method_source_info(socket, class, side, selector)
        # BT-2605: fetch the owning class once so the editor can badge the
        # class-level modifiers (sealed/abstract/native) alongside the method.
        {_def, _comment, class_native_module, class_modifiers, _is_protocol} =
          class_definition_info(socket, class)

        # BT-2642: snapshot the owning class's origin + package from the loaded
        # tree rows so the editor header can badge the package for this tab.
        {source_origin, package} = class_origin_package(socket, class)

        tab = %{
          id: id,
          kind: :method,
          class: class,
          side: side,
          selector: selector,
          # BT-2642: package/origin badge for the editor header.
          source_origin: source_origin,
          package: package,
          source: info.source,
          base: info.source,
          dirty: false,
          # Image-divergence snapshot at browse time (the badges the old
          # read-only pane carried): `disk_differs` = an unflushed live `>>`
          # patch, `runtime_only` = no static source on disk. `disk_differs` is
          # later set to `true` by `compile_clean/3` on an in-memory compile, and
          # both flags are re-derived from the live image when a clean tab is
          # re-activated (see the `%{} = existing` branch above).
          disk_differs: info.disk_differs,
          runtime_only: info.runtime_only,
          # BT-2714: a compiler-derived method — renders read-only (no editable
          # CodeMirror), showing the resolved doc block instead of a blank buffer.
          synthetic: info.synthetic,
          # The on-disk body captured while the image matched disk, so a later
          # compile diffs against it instead of flagging every re-save (BT-2550).
          disk_source: disk_body_snapshot(info),
          # The method's `///` doc-comment + signature for the read-only doc
          # block (BT-2558); nil when the method carries no doc / signature.
          doc: info.doc,
          signature: info.signature,
          # BT-2578: native backing module is a class-level fact, never set on a
          # method tab; `native_delegate` marks a `self delegate` method whose
          # implementation lives in the backing module (the jump affordance).
          native_module: nil,
          native_delegate: info.native_delegate,
          # BT-2605: class-level modifier badges for the header (sealed/abstract/
          # native), reflected off the owning class.
          class_modifiers: class_modifiers,
          class_native: is_binary(class_native_module) and class_native_module != "",
          new: false
        }

        socket
        |> assign(:tabs, socket.assigns.tabs ++ [tab])
        |> assign(:active_tab, id)
        |> sync_active(tab)
    end
  end

  # The method's image-accurate source (`browse-method-source`) plus the
  # divergence flags the editor breadcrumb badges: `disk_differs` (unflushed live
  # patch) and `runtime_only` (sourceless runtime method). `source` is "" for a
  # sourceless method / error, so opening it gives an empty editable buffer rather
  # than crashing.
  defp method_source_info(socket, class, side, selector) do
    case Facade.dispatch(
           :browse_method_source,
           %{class: class, side: side, selector: selector},
           ctx(socket)
         ) do
      {:value, result} when is_map(result) ->
        %{
          source: if(is_binary(result["source"]), do: result["source"], else: ""),
          disk_differs: result["disk_differs"] == true,
          runtime_only: runtime_only?(result),
          # BT-2714: a compiler-derived method (value accessors / `with<Field>:`
          # setters / actor `new`/`spawn`) has no editable source — the backend
          # returns `source: null` but resolves the real doc/signature. The tab
          # renders read-only (no CodeMirror), showing that doc block instead of
          # a blank editable buffer.
          synthetic: result["source_status"] == "synthetic",
          # BT-2558: the method's `///` doc-comment and signature, carried so the
          # editor can show a read-only doc block alongside the editable body.
          # `nil` when the method has no doc / no resolvable signature.
          doc: doc_text(result["doc"]),
          signature: doc_text(result["signature"]),
          # BT-2578: a `self delegate` method (ADR 0056) on a native: class — its
          # real implementation lives in the backing module's `handle_call`
          # clauses, reachable via the "→ Erlang implementation" jump.
          native_delegate: result["native_delegate"] == true
        }

      # Facade returned a value but not the expected map (sourceless / malformed
      # payload): open an empty editable buffer with no badges.
      {:value, _non_map} ->
        empty_source_info()

      # Facade error (class/method missing, dispatch failure): same empty-buffer
      # fallback, kept as its own arm so the error origin stays distinguishable
      # when debugging.
      {:error, _reason} ->
        empty_source_info()

      _ ->
        empty_source_info()
    end
  end

  # Defaults for a method with no resolvable image source: an empty editable buffer
  # and no divergence badges (and no doc block — BT-2558).
  defp empty_source_info,
    do: %{
      source: "",
      disk_differs: false,
      runtime_only: false,
      synthetic: false,
      doc: nil,
      signature: nil,
      native_delegate: false
    }

  # The on-disk method body to diff a later compile against (BT-2550 item 2). We
  # can only know it when the image matched disk at open: `disk_differs: false` and
  # not runtime-only means the backend confirmed the image body appears verbatim in
  # the on-disk class source, so the image `source` *is* the on-disk body. A
  # runtime-only or already-diverged method has no body we can pin to disk → `nil`,
  # and `compile_clean/3` falls back to its conservative flag.
  defp disk_body_snapshot(%{runtime_only: true}), do: nil

  defp disk_body_snapshot(%{disk_differs: false, source: src}) when is_binary(src) and src != "",
    do: src

  defp disk_body_snapshot(_), do: nil

  # The on-disk body to carry forward when a *clean* tab is re-activated (BT-2565).
  # `disk_body_snapshot/1` only yields a body while the image matches disk
  # (`disk_differs: false`), so a tab whose image diverged from disk via an
  # in-memory compile — `compile_clean/3` set `disk_differs: true` while preserving
  # the body captured at open — would otherwise re-derive `nil` on re-activation,
  # regressing a later exact-on-disk-body re-compile back to the conservative
  # `unflushed` flag. We split the two ways `disk_body_snapshot/1` returns nil:
  #
  #   * now runtime-only (no on-disk body at all) → drop to nil, matching the
  #     conservative fallback for a method that genuinely lost its disk source.
  #     This guards against a naive `existing.disk_source || …` retaining a stale
  #     snapshot for a method that legitimately transitioned to runtime-only.
  #   * still disk-backed but image-diverged → keep the prior `existing.disk_source`
  #     so the on-disk body stays pinned across the round-trip.
  #
  # A fresh snapshot (image back in sync with disk) always wins over the carried one.
  #
  # The carried `existing.disk_source` is only as fresh as tab-open time: if the file
  # is rewritten out-of-band (another session flushes, an external editor) *while the
  # image is diverged*, the carried body goes stale, and a later compile of the *old*
  # on-disk body would clear `unflushed` against disk that has since moved on — a
  # narrow false-negative (concurrent out-of-band writes during divergence, BT-2567).
  # As of BT-2567 the backend's `disk_differs` is a *live* re-read of the on-disk
  # file (not a load-time snapshot), so this self-corrects on the next re-activation:
  # `info.disk_differs` comes back `true` and the `existing.disk_differs or
  # info.disk_differs` merge re-raises the badge. The residual window is only the
  # transient between an out-of-band write and the next re-activation — and only when
  # the user re-compiles the *old* on-disk body in that gap. The conservative
  # pre-BT-2565 path avoided even that by re-flagging *every* re-activated diverged
  # tab — the false-positive BT-2565 fixes. The common-case win is worth the residual.
  @doc false
  def reactivation_disk_source(_existing, %{runtime_only: true}), do: nil

  def reactivation_disk_source(existing, info),
    do: disk_body_snapshot(info) || existing.disk_source

  # Normalise a browse-payload doc/signature field to a non-empty binary or nil.
  # The op already returns `null` (decoded to nil) for absent fields; this also
  # drops a stray empty string so the editor never shows a blank doc block.
  defp doc_text(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      _ -> value
    end
  end

  defp doc_text(_), do: nil

  # Label for the collapsible doc block's toggle (BT-2558, BT-2604). Deliberately a
  # short generic label rather than the method signature: the signature already shows
  # in the breadcrumb and as the first line of the editable source below, so repeating
  # it on the toggle just triples it. A class-definition tab's doc *is* the class
  # comment, hence the distinct wording.
  defp doc_summary_label(%{kind: :def}), do: "Class comment"
  defp doc_summary_label(_), do: "Documentation"

  # The class' editable definition skeleton (`browse-class-definition` →
  # `definition`, the synthesized `Super subclass: Name` header + state slots)
  # paired with its doc-block comment, fetched in one browse op. Returns
  # `{definition, comment}` where `definition` is a binary (`""` for a file-less
  # ClassBuilder class with no skeleton, so the editor body is always a string)
  # and `comment` is the rendered doc text or `nil` (the same comment
  # `Beamtalk help:` renders, so the browser and `help:` agree on a class' docs).
  # `{"", nil}` if the browse fails — the tab then opens empty rather than
  # erroring.
  defp class_definition_info(socket, class) do
    case Facade.dispatch(:browse_class_definition, %{class: class}, ctx(socket)) do
      {:value, %{} = result} ->
        definition =
          case Map.get(result, "definition") do
            text when is_binary(text) -> text
            _ -> ""
          end

        # BT-2639: `is_protocol` is a runtime-reflection boolean on the
        # class-definition row (op 4) — NOT a header string-sniff — so the
        # def-tab can reliably render the protocol action row.
        {definition, doc_text(Map.get(result, "comment")), native_backing_module(result),
         class_modifiers_from(result), Map.get(result, "is_protocol") == true}

      _ ->
        # Failure sentinel: `nil` modifiers (distinct from `[]`, a plain class with
        # no modifiers) so a transient fetch failure can be told apart from a real
        # empty result and the caller can keep the prior badges (BT-2605 review).
        # `is_protocol` defaults to `false` on failure (no protocol action row).
        {"", nil, nil, nil, false}
    end
  end

  # BT-2605: the reflected class-level modifiers for the editor-header badges, in a
  # stable display order. These are *runtime reflection* booleans from
  # `browse-class-definition` (op 4) — not parsed from the `definition` skeleton,
  # which carries no leading modifier keywords. A missing/false flag contributes no
  # badge. (BT-2629: `typed` is now reflected too — the already-emitted is_typed
  # meta flag is threaded through the runtime, mirroring sealed/abstract.)
  defp class_modifiers_from(result) do
    Enum.filter([:sealed, :typed, :abstract], &(Map.get(result, Atom.to_string(&1)) == true))
  end

  # BT-2578: the backing Erlang module name of a native: class (ADR 0056), or
  # nil for an ordinary class. Drives the "Erlang backend" badge + the read-only
  # native pane on a class-definition tab.
  defp native_backing_module(%{"native" => true} = result) do
    case Map.get(result, "backing_module") do
      mod when is_binary(mod) and mod != "" -> mod
      _ -> nil
    end
  end

  defp native_backing_module(_result), do: nil

  # BT-2578: fetch a native class's backing Erlang source for the read-only pane.
  # `content: nil` is the honest "source not available" empty state (a `.beam`-only
  # build that shipped no `.erl`); a non-native class / dispatch failure carries an
  # `error` string instead.
  defp load_native_view(socket, class, selector \\ nil) do
    base = %{
      class: class,
      backing_module: nil,
      source_file: nil,
      source_origin: nil,
      editable: false,
      content: nil,
      clauses: [],
      # The selector a method→clause jump asked for, and the matching clause the
      # backend resolved (or nil — a delegate may complete in `handle_info`).
      requested_selector: selector,
      selected_clause: nil,
      error: nil
    }

    params = if selector, do: %{class: class, selector: selector}, else: %{class: class}

    case Facade.dispatch(:browse_native_source, params, ctx(socket)) do
      {:value, %{} = r} ->
        %{
          base
          | backing_module: Map.get(r, "backing_module"),
            # The Erlang op returns the atom `null` for absent values, which
            # arrives over distribution as the Elixir atom `:null` (NOT `nil`).
            # Normalise so template `is_nil/1` guards and `:if` truthiness behave
            # (a raw `:null` is truthy and `is_nil(:null)` is false) — otherwise
            # the "no matching handle_call clause" explanation never renders and a
            # stripped-source path would interpolate as ":null" (BT-2578).
            # BT-2668: clean the path to project-relative — never the absolute host
            # path.
            source_file: clean_native_path(Map.get(r, "source_file")),
            source_origin: Map.get(r, "source_origin"),
            editable: Map.get(r, "editable") == true,
            content: nonempty_string(Map.get(r, "content")),
            clauses: Map.get(r, "clauses", []),
            selected_clause: map_or_nil(Map.get(r, "selected_clause"))
        }

      {:error, reason} ->
        %{base | error: facade_error(reason)}

      _ ->
        %{base | error: "Could not load Erlang source."}
    end
  end

  # True when a clause row is the one a jump selected (selector + line match).
  defp clause_active?(%{"selector" => s, "line" => l}, %{"selector" => s, "line" => l}), do: true
  defp clause_active?(_clause, _selected), do: false

  defp nonempty_string(s) when is_binary(s) and s != "", do: s
  defp nonempty_string(_), do: nil

  # Normalise the Erlang `null` atom (delivered as `:null` over distribution) and
  # any non-conforming value to a clean Elixir `nil` / typed value, so template
  # guards (`is_nil/1`, `:if`) and interpolation behave (BT-2578).
  defp map_or_nil(m) when is_map(m), do: m
  defp map_or_nil(_), do: nil

  # True when the native pane is currently showing `class`'s backing source.
  defp native_shown?(%{native_view: %{class: shown}}, class), do: shown == class
  defp native_shown?(_assigns, _class), do: false

  # BT-2648: fetch a standalone native module's source for the read-only pane,
  # keyed by `module` (not class). Same normalisation as `load_native_view/3`
  # (the Erlang `null` atom arrives as `:null` over distribution); `content: nil`
  # is the honest "source not available" empty state.
  defp load_native_module_view(socket, module) do
    base = %{
      module: module,
      backing_module: nil,
      source_file: nil,
      source_origin: nil,
      editable: false,
      content: nil,
      clauses: [],
      requested_selector: nil,
      selected_clause: nil,
      error: nil
    }

    case Facade.dispatch(:browse_native_module_source, %{module: module}, ctx(socket)) do
      {:value, %{} = r} ->
        %{
          base
          | backing_module: Map.get(r, "backing_module"),
            # BT-2668: clean the path to project-relative — never the absolute host
            # path (the workspace may be remote / another user's machine).
            source_file: clean_native_path(Map.get(r, "source_file")),
            source_origin: Map.get(r, "source_origin"),
            editable: Map.get(r, "editable") == true,
            content: nonempty_string(Map.get(r, "content")),
            clauses: Map.get(r, "clauses", []),
            selected_clause: map_or_nil(Map.get(r, "selected_clause"))
        }

      {:error, reason} ->
        %{base | error: facade_error(reason)}

      _ ->
        %{base | error: "Could not load Erlang source."}
    end
  end

  # BT-2667: open (or re-focus) a standalone native module's `.erl` as a read-only
  # `:native` editor tab. The tab is keyed by `native:<module>` so re-opening the
  # same module focuses the existing tab rather than stacking a duplicate, and a
  # native tab coexists with class/method tabs in the strip. The source is fetched
  # once at open and cached on the tab's `native_view`; a `:beam`-only module still
  # opens (its body shows the "source not available" empty state).
  defp open_native_module_tab(socket, module) do
    id = "native:" <> module

    case find_tab(socket, id) do
      %{} -> activate_tab(socket, id)
      nil -> add_native_module_tab(socket, id, module)
    end
  end

  defp add_native_module_tab(socket, id, module) do
    view = load_native_module_view(socket, module)
    # BT-2670: a project-owned native (`view.editable == true`) opens as an
    # EDITABLE tab — seed the write-surface `source`/`base` from the fetched
    # `.erl` content so the CodeMirror editor shows it and dirty-tracking works
    # (mirroring a method/class tab). Deps/stdlib natives keep `source: ""` and
    # the read-only render branch.
    initial_source = if view.editable, do: view.content || "", else: ""

    tab = %{
      id: id,
      kind: :native,
      class: module,
      side: nil,
      selector: nil,
      # The fetched native Erlang source view (content/clauses/error/clean
      # source_file/editable). For a project-owned native the tab is editable
      # (compile + reload + write-back via `native_save`); deps/stdlib stay
      # read-only and the editor render takes the read-only :native branch.
      native_view: view,
      source: initial_source,
      base: initial_source,
      dirty: false,
      disk_differs: false,
      runtime_only: false,
      disk_source: nil,
      doc: nil,
      signature: nil,
      is_protocol: false,
      # No Beamtalk-class modifier/origin badges on a raw `.erl` tab; the source
      # origin shows in the native pane header instead. Keys present so the shared
      # tab helpers' dot-access never crashes.
      native_module: nil,
      native_delegate: false,
      class_modifiers: nil,
      class_native: false,
      source_origin: nil,
      package: nil,
      new: false
    }

    socket
    |> assign(:tabs, socket.assigns.tabs ++ [tab])
    |> assign(:active_tab, id)
    |> sync_active(tab)
  end

  # BT-2667: the module name of the focused `:native` tab (or nil) — drives the
  # "sel" highlight on the System Browser's Native modules list so it tracks the
  # native tab the editor is showing, mirroring how the class tree tracks the
  # active class/def tab.
  defp active_native_module(assigns) do
    case active_tab(assigns) do
      %{kind: :native, class: module} -> module
      _ -> nil
    end
  end

  # BT-2668: turn an absolute on-disk `.erl` path into a clean, project-relative
  # one for display — never leak the host filesystem layout (the workspace may be
  # remote / another user's machine). Strips the build/project prefix up to a
  # recognisable source root (`apps/`, `deps/`, `src/`, `native/`, `lib/`) so a
  # path like `/home/james/src/proj/deps/http/native/x.erl` shows as
  # `deps/http/native/x.erl`. A path that is already relative is returned as-is; an
  # absolute path with no recognisable root falls back to its basename so only the
  # file name (not the directory tree) is shown. `nil`/`:null`/empty → nil (the
  # honest "no path" state — the viewer then omits the path line).
  @native_path_roots ~w(apps deps src native lib)
  def clean_native_path(path) when is_binary(path) and path != "" do
    if String.starts_with?(path, "/") do
      segments = String.split(path, "/", trim: true)

      case Enum.find_index(segments, &(&1 in @native_path_roots)) do
        nil -> List.last(segments)
        idx -> segments |> Enum.drop(idx) |> Enum.join("/")
      end
    else
      # Already relative (no leading "/"): trust it as the project-relative form.
      path
    end
  end

  def clean_native_path(_), do: nil

  # Query senders/implementors of the active method's selector (`nav-query`) and
  # open the result popover. A tab with no selector (a class-definition tab) is a
  # graceful no-op — there is nothing to query. `kind` is `:senders` |
  # `:implementors`; the facade op name matches.
  defp run_nav_query(socket, kind) do
    selector =
      case active_tab(socket.assigns) do
        %{selector: sel} -> sel
        nil -> nil
      end

    run_nav_query_for(socket, kind, selector)
  end

  # Run a senders/implementors `nav-query` for an explicit selector (BT-2639
  # reuses this for the Required-methods → Implementors jump; BT-2495's
  # active-tab path delegates here). A nil/empty selector is a graceful no-op.
  defp run_nav_query_for(socket, kind, selector) do
    if is_binary(selector) and selector != "" do
      case Facade.dispatch(kind, %{selector: selector}, ctx(socket)) do
        {:value, %{"sites" => sites}} when is_list(sites) ->
          assign(socket, nav_popover: %{kind: kind, selector: selector, sites: sites})

        {:value, _other} ->
          assign(socket, nav_popover: %{kind: kind, selector: selector, sites: []})

        {:error, reason} ->
          assign(socket,
            nav_popover: %{kind: kind, selector: selector, sites: [], error: facade_error(reason)}
          )

        # Any other shape (version skew, an unexpected reply) degrades to an empty
        # popover with a generic message rather than crashing the LiveView.
        _other ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: selector,
              sites: [],
              error: "Navigation unavailable."
            }
          )
      end
    else
      socket
    end
  end

  # Query a protocol's required methods / conforming classes (`nav-query`
  # `required_methods` / `conforming_classes`) and open the result popover
  # (BT-2639). The protocol equivalent of `run_nav_query/2`; the active tab must
  # be a class-definition tab for a Protocol — otherwise a graceful no-op (the
  # buttons only render for protocols). The popover's `selector` slot carries the
  # protocol name (the popover head shows it next to the kind label, mirroring the
  # method-selector display for senders/implementors). `kind` is
  # `:required_methods` | `:conforming_classes`; the facade op name matches.
  defp run_protocol_nav_query(socket, kind) do
    protocol =
      case active_tab(socket.assigns) do
        %{kind: :def, class: class, is_protocol: true} -> class
        _ -> nil
      end

    if is_binary(protocol) and protocol != "" do
      case Facade.dispatch(kind, %{protocol: protocol}, ctx(socket)) do
        {:value, %{"sites" => sites}} when is_list(sites) ->
          assign(socket, nav_popover: %{kind: kind, selector: protocol, sites: sites})

        {:value, _other} ->
          assign(socket, nav_popover: %{kind: kind, selector: protocol, sites: []})

        {:error, reason} ->
          assign(socket,
            nav_popover: %{kind: kind, selector: protocol, sites: [], error: facade_error(reason)}
          )

        _other ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: protocol,
              sites: [],
              error: "Navigation unavailable."
            }
          )
      end
    else
      socket
    end
  end

  # Query the Beamtalk callers of the focused native tab's module (`nav-query`
  # `callers_of_native_module`, BT-2669) and open the result popover. The active
  # tab must be a `:native` tab — otherwise a graceful no-op (the Callers button
  # only renders on native tabs). The popover's `selector` slot carries the
  # module name (shown next to the "Callers" head, mirroring the selector display
  # for senders/implementors); each row opens the calling method via `nav_open`.
  defp run_native_callers_query(socket) do
    module =
      case active_tab(socket.assigns) do
        %{kind: :native, class: class} -> class
        _ -> nil
      end

    if is_binary(module) and module != "" do
      kind = :callers_of_native_module

      case Facade.dispatch(kind, %{module: module}, ctx(socket)) do
        {:value, %{"sites" => sites}} when is_list(sites) ->
          assign(socket, nav_popover: %{kind: kind, selector: module, sites: sites})

        {:value, _other} ->
          assign(socket, nav_popover: %{kind: kind, selector: module, sites: []})

        {:error, reason} ->
          assign(socket,
            nav_popover: %{kind: kind, selector: module, sites: [], error: facade_error(reason)}
          )

        _other ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: module,
              sites: [],
              error: "Navigation unavailable."
            }
          )
      end
    else
      socket
    end
  end

  # ── go-to-definition resolution (BT-2666) ───────────────────────────────────
  #
  # Resolve a modifier-clicked symbol to a definition target and open it. The
  # class-then-selector order mirrors the LSP `definition_provider.rs`: a name
  # that is a loaded class is a class reference (→ its definition tab); anything
  # else is treated as a message send (→ its implementor(s)).
  defp run_goto_definition(socket, token, code) do
    cond do
      # An empty/blank token is a bare no-op (the JS never sends one — it only
      # fires on an identifier — but guard it without even a flash).
      String.trim(token) == "" ->
        socket

      known_class?(socket, token) ->
        socket
        |> open_definition(token)
        |> navigate_browser(token, "instance")
        # Clear any open Implementors popover so it doesn't linger behind the
        # newly-opened class tab (parity with nav_open_class/open_implementor_site).
        |> assign(nav_popover: nil)

      true ->
        case goto_selector(token, code) do
          nil -> goto_not_found(socket)
          selector -> open_implementor(socket, selector)
        end
    end
  end

  # A loaded class name (matches the System Browser tree the editor already
  # holds). Class lookup is case-sensitive and exact — Beamtalk class names are
  # capitalised identifiers, so a clicked lowercase token never matches here and
  # falls through to the selector path.
  defp known_class?(socket, token) do
    socket.assigns
    |> Map.get(:browser_classes, [])
    |> Enum.any?(fn row -> Map.get(row, "name") == token end)
  end

  # The message selector the clicked token denotes, derived from the `code`
  # line-prefix (the line up to and including the token):
  #
  #   * a KEYWORD send — the prefix ends in `…word:` (optionally with a trailing
  #     argument the click landed before) — resolves to the maximal trailing run
  #     of `word:` parts, so clicking any part of `dict at: k put: v` resolves the
  #     whole `at:put:` selector.
  #   * otherwise the bare token is a UNARY selector (e.g. `factorial`).
  #
  # An empty/whitespace token yields nil (no selector to resolve). The result is
  # only ever fed to the `implementors` nav-query, whose `binary_to_existing_atom`
  # guard turns an unknown selector into an empty result set — so a wrong guess is
  # a graceful "no definition", never a crash.
  defp goto_selector(token, code) when is_binary(code) do
    case keyword_selector(code) do
      nil -> if token == "", do: nil, else: token
      selector -> selector
    end
  end

  defp goto_selector(token, _code), do: if(token == "", do: nil, else: token)

  # Extract the trailing keyword selector (`word:word:…`) from a line prefix, or
  # nil when it is not a keyword send. The clicked keyword belongs to the send
  # that ends the prefix, so we first trim to the trailing segment — everything
  # after the last statement/grouping breaker (`.`, `;`, brackets, `^`, `|`) —
  # then join that segment's contiguous `word:` parts. This keeps unrelated sends
  # earlier on the line out of the selector (e.g. `coll at: i. obj foo: x bar: y`
  # clicked near `y` resolves to `foo:bar:`, not `at:foo:bar:`).
  defp keyword_selector(code) do
    segment = code |> String.split(~r/[.;()\[\]{}^|]/) |> List.last()
    parts = Regex.scan(~r/([A-Za-z_][A-Za-z0-9_]*):/, segment) |> Enum.map(&Enum.at(&1, 1))

    case parts do
      [] -> nil
      _ -> parts |> Enum.map(&(&1 <> ":")) |> Enum.join()
    end
  end

  # Resolve a selector to its implementor(s) via the BT-2495 `implementors`
  # nav-query and open the result. The single-hit case opens the method tab
  # directly (the whole point of go-to-definition — no extra click); several hits
  # fall back to the Senders/Implementors popover so the user disambiguates; none
  # is the graceful unresolved no-op. Mirrors `run_nav_query_for/3` but acts on
  # the result rather than always opening a popover.
  defp open_implementor(socket, selector) do
    case Facade.dispatch(:implementors, %{selector: selector}, ctx(socket)) do
      {:value, %{"sites" => [site]}} when is_map(site) ->
        open_implementor_site(socket, site)

      {:value, %{"sites" => sites}} when is_list(sites) and sites != [] ->
        # Ambiguous — reuse the BT-2495 popover so the user picks the implementor.
        assign(socket, nav_popover: %{kind: :implementors, selector: selector, sites: sites})

      _ ->
        goto_not_found(socket)
    end
  end

  # Open a single implementor site (a `{class, side, selector}` row) the same way
  # the `nav_open` popover row does: open the method tab and point the browser at
  # it, then ensure no stale popover lingers.
  defp open_implementor_site(socket, site) do
    class = Map.get(site, "class")
    selector = Map.get(site, "method")
    side = if Map.get(site, "class_side") == true, do: "class", else: "instance"

    if is_binary(class) and is_binary(selector) do
      socket
      |> open_method_tab(class, side, selector)
      |> navigate_browser(class, side)
      |> assign(nav_popover: nil)
    else
      goto_not_found(socket)
    end
  end

  # The graceful unresolved no-op (BT-2666 AC): a brief flash, the editor and any
  # open tab untouched. A transient info flash matches the cockpit's other
  # lightweight status messages and self-dismisses.
  defp goto_not_found(socket) do
    put_flash(socket, :info, "No definition found.")
  end

  # ── System Browser view helpers (BT-2491) ───────────────────────────────────

  # The class rows in display order for the active view. Hierarchy walks
  # roots→children indenting by the *true* superclass depth (BT-2637: no cap — the
  # real kernel chain is ProtoObject→Object→Value→Number→…, far deeper than the
  # spike's two levels, so a cap collapsed everything onto one indent and read as
  # flat). `class_rows/1` scales the visible indent from this depth.
  # Category groups by the class annotation, each group a `{category, rows}` pair.
  # Both feed `class_rows/1` `{row, indent, context?}` tuples so the template
  # renders one branch — Category rows are always `context? = false`; only the
  # filtered Hierarchy view (BT-2649) emits dimmed `context? = true` ancestor rows.

  # Hierarchy under an active source filter (BT-2649). Builds the tree from the
  # *full* class set so the transitive superclass ancestors connecting each
  # matching class to a root are kept as dimmed, non-interactive "context" rows —
  # otherwise the filtered set flattens (every match at indent 0). Returns
  # `{class_row, indent, context?}`; a context row's name is not in `visible_names`.
  # For the `all` filter every class is visible, so this emits zero context rows —
  # identical shape to `hierarchy_rows/1` plus a `false` context flag.
  defp hierarchy_rows_with_context(all_classes, visible_names),
    do: BtAttachWeb.ClassTree.hierarchy_rows_with_context(all_classes, visible_names)

  # Category: `{category, [class_row]}` groups, each group's classes sorted by
  # name, the groups themselves sorted by category. A class with no category falls
  # into an "(uncategorized)" bucket rather than vanishing.
  #
  # BT-2557: TestCase subclasses (`is_test`) are pulled into a dedicated "Tests"
  # bucket regardless of their package — the browser surfaces them as a category
  # so a project's tests are one click away once loaded.
  defp category_groups(classes) do
    classes
    |> Enum.group_by(&class_category_bucket/1)
    |> Enum.sort_by(fn {category, _} -> category end)
    |> Enum.map(fn {category, rows} ->
      {category, Enum.sort_by(rows, &Map.get(&1, "name"))}
    end)
  end

  defp class_category_bucket(%{"is_test" => true}), do: "Tests"
  # BT-2615: protocol class objects (ADR 0068) declare no package, so they would
  # otherwise land in "(uncategorized)". Group them under a dedicated "Protocols"
  # bucket — mirroring the "Tests" treatment — so a project's protocols are one
  # click away and don't masquerade as uncategorized classes.
  defp class_category_bucket(%{"is_protocol" => true}), do: "Protocols"
  defp class_category_bucket(class), do: Map.get(class, "category") || "(uncategorized)"

  # Narrow class rows by source origin (BT-2557). `source_origin` is the bare
  # classification "project", "stdlib", or "dependency" (BT-2643 — the package
  # name now lives in a separate `package` field). "all" is the identity filter;
  # an unknown filter also passes everything through (fail-open so the tree is
  # never silently blanked).
  defp filter_by_source(classes, "all"), do: classes

  defp filter_by_source(classes, "deps") do
    Enum.filter(classes, &(Map.get(&1, "source_origin") == "dependency"))
  end

  defp filter_by_source(classes, src) when src in ~w(project stdlib) do
    Enum.filter(classes, &(Map.get(&1, "source_origin") == src))
  end

  defp filter_by_source(classes, _src), do: classes

  # True when no class is selected, or the selected class survives the current
  # source filter (BT-2597). Used to decide whether switching filters should
  # clear a now-hidden selection.
  defp selected_class_visible?(%{assigns: %{selected_class: nil}}), do: true

  defp selected_class_visible?(%{assigns: assigns}) do
    assigns.browser_classes
    |> filter_by_source(assigns.browser_source)
    |> Enum.any?(&(Map.get(&1, "name") == assigns.selected_class))
  end

  # The flat method list for the current protocol filter: all selectors across the
  # protocol tree (filter = nil → "all") or just the selected protocol's, each
  # carrying its protocol name so the row badge / breadcrumb can show it. Sorted
  # by selector for stable order.
  defp filtered_methods(protocols, filter) do
    protocols
    |> Enum.filter(fn p -> filter == nil or Map.get(p, "name") == filter end)
    |> Enum.flat_map(fn p ->
      name = Map.get(p, "name")
      Enum.map(Map.get(p, "selectors", []), &Map.put(&1, "protocol", name))
    end)
    |> Enum.sort_by(&Map.get(&1, "selector"))
  end

  # Total selector count across the protocol tree (the "all" filter row's count).
  defp protocol_method_count(protocols) do
    Enum.reduce(protocols, 0, fn p, acc -> acc + length(Map.get(p, "selectors", [])) end)
  end

  # A row is "runtime-only" (image-diverged, ADR 0096 / BT-2483) when its origin
  # is `runtime` — in the live image with no static/disk source. The class tree
  # and method list badge these so an observer sees what is not on disk.
  defp runtime_only?(%{"origin" => "runtime"}), do: true
  defp runtime_only?(_), do: false

  # A selector row is "derived" (BT-2714) when its xref `source_status` is
  # `synthetic` — a compiler-synthesized method (Value accessors, `with<Field>:`
  # setters, keyword constructors, actor `new`/`spawn`) with no hand-written
  # source line. The method list badges these `derived` so the synthetic magic is
  # visible rather than indistinguishable from real methods.
  defp synthetic?(%{"source_status" => "synthetic"}), do: true
  defp synthetic?(_), do: false

  # BT-2714: whether the *active editor tab* was opened for a compiler-derived
  # method (its `browse-method-source` came back `source_status: synthetic`). Such
  # a tab has no editable body — it renders read-only (the doc block + a
  # "compiler-derived" note) instead of a blank CodeMirror. Bracket-style pattern
  # match so the many non-method tab kinds (class definition, native module) that
  # never set the key read as `false` rather than raising a KeyError.
  defp synthetic_tab?(%{synthetic: true}), do: true
  defp synthetic_tab?(_), do: false

  # Source origin badge helpers (BT-2552, BT-2643, BT-2641). Two orthogonal
  # fields drive the badge: `source_origin` is the classification ("stdlib" |
  # "dependency" | "project") and keys the css class + title; `package` is the
  # package name and feeds the dependency badge label. The dependency badge reads
  # as a "DEP" marker (parity with the generic "STDLIB" marker), suffixed with
  # the package name when known ("DEP · HTTP") and bare ("DEP") otherwise.
  defp source_origin_class(%{"source_origin" => "stdlib"}), do: "stdlib"
  defp source_origin_class(%{"source_origin" => "dependency"}), do: "dependency"
  defp source_origin_class(_), do: "project"

  defp source_origin_label(%{"source_origin" => "stdlib"}), do: "stdlib"

  defp source_origin_label(%{"source_origin" => "dependency"} = row),
    do: dependency_badge_label(row)

  defp source_origin_label(_), do: ""

  # Dependency badge text: "DEP · <pkg>" when the package is known, plain "DEP"
  # when it is absent/unknown. This is the badge-specific label; `package_name/1`
  # (which degrades to "unknown") still serves callers that need the raw package.
  defp dependency_badge_label(row) do
    case Map.get(row, "package") do
      pkg when is_binary(pkg) and pkg != "" -> "DEP · #{pkg}"
      _ -> "DEP"
    end
  end

  defp source_origin_title(%{"source_origin" => "stdlib"}), do: "Standard library"

  defp source_origin_title(%{"source_origin" => "dependency"} = row),
    do: "Dependency: #{package_name(row)}"

  defp source_origin_title(_), do: "Project"

  # The package name carried on a browse row's `package` field (BT-2643). Absent /
  # null packages degrade to "unknown" so a dependency badge never renders blank.
  defp package_name(row) do
    case Map.get(row, "package") do
      pkg when is_binary(pkg) and pkg != "" -> pkg
      _ -> "unknown"
    end
  end

  # BT-2642: editor-header package/origin badge for the active tab. The tab carries
  # `source_origin` ("stdlib" | "dependency" | "project" | nil) and `package`,
  # snapshotted at open from the class's browse row. The header badge reuses
  # BT-2641's vocabulary and extends it to project (which the tree hides but the
  # header shows): stdlib → "STDLIB", dependency → "DEP · <pkg>" (or bare "DEP"),
  # project → the bare project package name. Returns "" when origin is unknown or
  # a project tab carries no package, so the badge simply does not render.
  defp header_package_label(%{source_origin: "stdlib"}), do: "STDLIB"

  defp header_package_label(%{source_origin: "dependency"} = tab),
    do: dependency_badge_label(stringify_origin(tab))

  defp header_package_label(%{source_origin: "project", package: pkg})
       when is_binary(pkg) and pkg != "",
       do: pkg

  defp header_package_label(_tab), do: ""

  # The CSS modifier class keying the header badge color, per origin. Mirrors
  # `source_origin_class/1` but reads the tab's atom-keyed `source_origin`.
  defp header_origin_class(%{source_origin: "stdlib"}), do: "stdlib"
  defp header_origin_class(%{source_origin: "dependency"}), do: "dependency"
  defp header_origin_class(_tab), do: "project"

  # The tooltip spelling out the origin for the header badge. Project gets a
  # "Project: <pkg>" title (BT-2642 extends the bare "Project" the tree title used).
  defp header_origin_title(%{source_origin: "stdlib"}), do: "Standard library"

  defp header_origin_title(%{source_origin: "dependency"} = tab),
    do: "Dependency: #{package_name(stringify_origin(tab))}"

  defp header_origin_title(%{source_origin: "project", package: pkg})
       when is_binary(pkg) and pkg != "",
       do: "Project: #{pkg}"

  defp header_origin_title(_tab), do: "Project"

  # Bridge the tab's atom-keyed origin/package onto the string-keyed map the
  # BT-2641 browse-row helpers (`dependency_badge_label/1`, `package_name/1`)
  # expect, so the dependency badge text stays in one place.
  defp stringify_origin(tab),
    do: %{"source_origin" => tab[:source_origin], "package" => tab[:package]}

  # Origin + package for a class, looked up from the loaded class-tree rows
  # (BT-2643 carries both on every row). Snapshotted onto a tab at open so the
  # editor header can badge the package even when the tree pane is collapsed.
  # Falls back to `{nil, nil}` when the class isn't in the loaded rows (graceful:
  # the header badge then renders nothing).
  defp class_origin_package(socket, class) do
    case Enum.find(socket.assigns[:browser_classes] || [], &(Map.get(&1, "name") == class)) do
      %{} = row -> {Map.get(row, "source_origin"), Map.get(row, "package")}
      _ -> {nil, nil}
    end
  end

  # ── tabbed method editor data model (BT-2494) ───────────────────────────────
  #
  # A tab is a plain map; the open-tab list lives in `:tabs` and the focused
  # tab's id in `:active_tab`. The visible class/selector/source assigns (which
  # the save_method form binds) always mirror the active tab, so the existing
  # write-surface handler reads them unchanged.
  #
  #   %{
  #     id: stable string id (method-key, "def:<Class>", or "new:<Class>"),
  #     kind: :method | :def,
  #     class: "Counter",
  #     side: "instance" | "class",     # methods only
  #     selector: "increment",          # methods only ("" for an unsaved new method)
  #     source: live edit buffer,
  #     base: last-compiled source (dirty = source != base),
  #     dirty: boolean,
  #     disk_differs: boolean,   # methods only — unflushed live `>>` patch; snapshot at open, set on compile
  #     runtime_only: boolean,   # methods only — sourceless runtime method at open
  #     disk_source: binary | nil, # methods only — on-disk body captured at open (BT-2550); nil when unknown
  #     doc: binary | nil,       # BT-2558 read-only doc block: method `///` doc / class comment
  #     signature: binary | nil, # BT-2558 method signature (nil for a class-definition tab)
  #     is_protocol: boolean,    # BT-2639 def tabs only — gates the protocol action row

  #     class_modifiers: [:sealed | :typed | :abstract] | nil, # BT-2605 reflected class modifiers; nil = transient fetch failure (no badges)
  #     class_native: boolean,   # BT-2605 native: class flag, for the Native badge (all tab kinds)
  #     source_origin: "stdlib" | "dependency" | "project" | nil, # BT-2642 owning class origin, for the header package badge
  #     package: binary | nil,   # BT-2642 owning class package name, for the header package badge
  #     new: boolean             # an unsaved "new method" tab (selector input shown, not the breadcrumb)
  #   }
  #
  # The cockpit opens with NO tabs — the editor shows an empty state until the
  # user opens something (a method or class definition from the System Browser).
  # `:active_tab` is `nil` while the strip is empty; the render guards on it and
  # shows the empty-state panel instead of the editor form. Edit-backing assigns
  # stay `""` (set just before this in the assign chain) until a tab is focused.
  defp init_tabs(socket) do
    socket
    |> assign(:tabs, [])
    |> assign(:active_tab, nil)
    # BT-2655: a monotonically-bumped revision folded into the method-editor
    # overlay's element id. The CmEditor (CodeMirror) host is `phx-update="ignore"`
    # and seeds its doc from the hidden textarea only on mount, so the only way to
    # push a NEW body into the focused tab's editor is to re-key the element so
    # LiveView replaces it and the hook remounts. Switching tabs already re-keys on
    # `@active_tab`; an *in-place* body re-read of the already-active tab (a git
    # revert, a flush/push reconcile) keeps the same `@active_tab`, so we bump this
    # to force the remount and surface the reverted source without a close/reopen.
    |> assign(:editor_rev, 0)
  end

  defp find_tab(socket, id), do: Enum.find(socket.assigns.tabs, &(&1.id == id))

  # The focused tab, or `nil` when the strip is empty (startup, or after the last
  # tab is closed) — callers and the render guard on the nil. Takes the bare
  # `assigns` (not the socket) so the template can call it for the breadcrumb /
  # dirty-state; falls back to the first tab if a non-nil active id somehow no
  # longer maps.
  defp active_tab(%{tabs: tabs, active_tab: id}) do
    (id && Enum.find(tabs, &(&1.id == id))) || List.first(tabs)
  end

  # Focus a tab by id and mirror its class/selector/source into the form-backing
  # assigns. Clears any stale save/flush result so switching tabs starts clean.
  defp activate_tab(socket, id) do
    case find_tab(socket, id) do
      nil -> socket
      tab -> sync_active(assign(socket, :active_tab, id), tab)
    end
  end

  # Push the active tab's fields into the form-backing assigns so the (single)
  # save_method form always reflects the focused tab.
  defp sync_active(socket, tab) do
    assign(socket,
      edit_class: tab.class,
      edit_selector: tab.selector || "",
      edit_source: tab.source,
      # Drop the previous tab's selection: switching tabs remounts the editor, so
      # the old `{text, start, end}` no longer points at anything live. Without
      # this, a future consumer of `:edit_selection` could act on stale coords
      # from the tab the user just left.
      edit_selection: nil,
      save_result: nil,
      save_error: nil,
      flush_result: nil,
      flush_error: nil
    )
  end

  # Close a tab. The strip may go empty (the editor then shows its empty state) —
  # closing the active tab moves focus to the previous remaining tab, or clears
  # focus (`active_tab: nil`, edit assigns reset) when it was the last one.
  defp close_tab(socket, id) do
    tabs = socket.assigns.tabs

    if not Enum.any?(tabs, &(&1.id == id)) do
      socket
    else
      idx = Enum.find_index(tabs, &(&1.id == id))
      remaining = List.delete_at(tabs, idx)
      socket = assign(socket, :tabs, remaining)

      cond do
        socket.assigns.active_tab != id ->
          socket

        remaining == [] ->
          clear_active(socket)

        true ->
          next = Enum.at(remaining, max(idx - 1, 0))
          sync_active(assign(socket, :active_tab, next.id), next)
      end
    end
  end

  # Drop focus to the empty state: no active tab, edit-backing assigns reset to
  # the same blanks `init_tabs/1` starts from, and any stale save/flush result
  # cleared so the next opened tab starts clean.
  defp clear_active(socket) do
    assign(socket,
      active_tab: nil,
      edit_class: "",
      edit_selector: "",
      edit_source: "",
      edit_selection: nil,
      save_result: nil,
      save_error: nil,
      flush_result: nil,
      flush_error: nil
    )
  end

  # Open (or re-focus) a class-definition tab for the active tab's class. A def
  # tab evals its definition source on compile (saving compiles the class). On
  # first open it also reads the class' comment (BT-2558) so the editor can show
  # it as a read-only documentation block above the editable definition body.
  defp open_definition(socket) do
    # The "+ def" affordance opens the *active* tab's class — a no-op when the
    # strip is empty (nothing focused to take a class from). The button is hidden
    # in that state, so this guard is just belt-and-braces against a stale click.
    case active_tab(socket.assigns) do
      # A native (.erl) tab's `class` is a module name, not a Beamtalk class —
      # there is no class definition to open, so "+ def" is a no-op (the button is
      # also hidden for native tabs; this guards a stale click).
      %{kind: :native} -> socket
      %{class: class} -> open_definition(socket, class)
      nil -> socket
    end
  end

  # Open (or re-focus) a class-definition tab for a named class — the System
  # Browser's "class definition" entry opens the *selected* class's definition,
  # which need not be the active tab's class.
  defp open_definition(socket, class) do
    id = "def:" <> class

    case find_tab(socket, id) do
      %{} = existing ->
        # Parity with the method-tab re-activation path: refresh the read-only
        # doc block from the live image so an out-of-band class comment change
        # (MCP `save_class`, a `>>` patch) shows on re-focus instead of the
        # snapshot taken at first open. Only `doc:` is touched — the editable
        # definition buffer and its dirty flag are left untouched, so an
        # in-progress edit survives a tab switch. The skeleton `definition` the
        # browse also returns is intentionally discarded here.
        {_definition, comment, native_module, class_modifiers, is_protocol} =
          class_definition_info(socket, class)

        # Keep the prior backing module if the re-fetch fails transiently
        # (workspace unreachable → `{"", nil, nil, nil}`): a `nil` here would hide
        # the "Erlang backend" badge + pane toggle on an already-open tab while
        # `@native_view` still holds the fetched source. A successful re-fetch
        # always wins (the class of a `def:` tab does not change between
        # activations, so a non-nil result is the same module).
        resolved_native = native_module || existing.native_module

        refreshed = %{
          existing
          | doc: comment,
            is_protocol: is_protocol,
            native_module: resolved_native,
            # BT-2605: refresh the reflected modifier badges off the same fetch; a
            # `nil` result is the transient-failure sentinel, so keep the prior
            # list rather than clearing the badges (BT-2605 review).
            class_modifiers: class_modifiers || existing.class_modifiers,
            class_native: is_binary(resolved_native) and resolved_native != ""
        }

        socket
        |> update_active_tab_by_id(id, fn _ -> refreshed end)
        |> activate_tab(id)

      nil ->
        # Fetch the class skeleton (header + state slots) and its comment in one
        # browse op: the skeleton seeds the editable definition body, the comment
        # the read-only doc block. Without the skeleton the editor opened empty —
        # the doc block rendered but the class definition itself was missing
        # (BT-2558 only wired the comment).
        {definition, comment, native_module, class_modifiers, is_protocol} =
          class_definition_info(socket, class)

        # BT-2642: snapshot the class's origin + package for the header badge.
        {source_origin, package} = class_origin_package(socket, class)

        tab = %{
          id: id,
          kind: :def,
          class: class,
          side: nil,
          selector: nil,
          # BT-2639: is this class a Protocol? Gates the protocol action row
          # (Required methods / Conforming classes) on the def tab.
          is_protocol: is_protocol,
          # BT-2642: package/origin badge for the editor header.
          source_origin: source_origin,
          package: package,
          source: definition,
          base: definition,
          dirty: false,
          disk_differs: false,
          runtime_only: false,
          # A :def tab has no single on-disk method body to diff against.
          disk_source: nil,
          # The class comment as the doc block; no per-method signature on a
          # class-definition tab.
          doc: comment,
          signature: nil,
          # BT-2578: the backing Erlang module (native: classes only), nil
          # otherwise — gates the "Erlang backend" badge + read-only native pane.
          native_module: native_module,
          # BT-2605: reflected class modifiers (sealed/abstract) + native flag for
          # the header modifier badges. `class_native` is kept distinct from
          # `native_module` so the badge can also show on method tabs of the class.
          class_modifiers: class_modifiers,
          class_native: is_binary(native_module) and native_module != "",
          new: false
        }

        socket
        |> assign(:tabs, socket.assigns.tabs ++ [tab])
        |> assign(:active_tab, id)
        |> sync_active(tab)
    end
  end

  # Open (or re-focus) a blank "new method" tab for a class on `side`
  # ("instance"/"class"): a `:method` tab whose selector is not yet chosen
  # (`selector: ""`, `new: true`). The author fills the selector (via the
  # new-method-only selector input) and types the body; saving drives the same
  # write-surface `save` as any method. One blank new-method tab per (class, side)
  # — re-clicking re-focuses rather than stacking empties.
  defp open_new_method(socket, class, side) do
    id = "new:" <> class <> ":" <> side

    case find_tab(socket, id) do
      %{} -> activate_tab(socket, id)
      nil -> add_new_method_tab(socket, id, class, side)
    end
  end

  defp add_new_method_tab(socket, id, class, side) do
    # BT-2605: the class already exists, so fetch it to badge the class-level
    # modifiers (sealed/abstract/native) on the new-method tab.
    {_def, _comment, native_module, class_modifiers, _is_protocol} =
      class_definition_info(socket, class)

    # BT-2642: snapshot the class's origin + package for the header badge.
    {source_origin, package} = class_origin_package(socket, class)

    tab = %{
      id: id,
      kind: :method,
      class: class,
      side: side,
      selector: "",
      # BT-2642: package/origin badge for the editor header.
      source_origin: source_origin,
      package: package,
      source: "",
      base: "",
      dirty: false,
      disk_differs: false,
      runtime_only: false,
      disk_source: nil,
      doc: nil,
      signature: nil,
      # BT-2578: a blank new-method tab is never a native delegate (no selector
      # yet); the keys must still be present so the editor render's dot-access
      # holds for every :method tab. `native_module` stays nil on method tabs (it
      # gates the :def-only native pane); the header's Native *badge* reads the
      # separate `class_native` flag below.
      native_module: nil,
      native_delegate: false,
      # BT-2605: class-level modifier badges (sealed/abstract/native) for the
      # header — class-level facts, so they show on method tabs of the class too.
      class_modifiers: class_modifiers,
      class_native: is_binary(native_module) and native_module != "",
      # The new-method marker: drives the selector input + breadcrumb label, and
      # keeps the tab id stable (no selector to key on yet).
      new: true
    }

    socket
    |> assign(:tabs, socket.assigns.tabs ++ [tab])
    |> assign(:active_tab, id)
    |> sync_active(tab)
  end

  # Record a keystroke edit on the active tab and recompute its dirty flag
  # (source != last-compiled base). We deliberately do NOT re-assign
  # `:edit_source` here: it is only the *initial* value CodeMirror reads when the
  # CmEditor hook mounts (the hidden `source` textarea is `phx-update="ignore"`,
  # so the editor owns the live text and a server echo can't reach it anyway).
  # The tab's `source` is the truth the next compile / tab-switch reads;
  # `:edit_source` is re-synced when a tab is (re)focused — the element is re-keyed
  # on `@active_tab`, so a fresh mount picks it up.
  defp track_edit(socket, source) do
    update_active_tab(socket, fn tab -> %{tab | source: source, dirty: source != tab.base} end)
  end

  # After a successful compile, clear the saved tab's dirty dot and re-base it on
  # the compiled source. `tab_id` is nil for the historical no-tab save payload (or
  # `""` from the empty-state hidden form) — neither matches an open tab, so
  # there's nothing to reconcile and the update is a harmless no-op.
  #
  # A `:method` save is an in-memory live `>>` patch (logged to the ChangeLog) that
  # is *not* flushed to disk, so a successful compile *may* diverge the live body
  # from its on-disk counterpart: set `disk_differs` so the `unflushed` breadcrumb
  # badge appears. (Clearing it again on flush needs a `flush_completed`
  # subscription — tracked as BT-2545.) A `:def` tab evaluates a whole class
  # definition with no single on-disk method body to diverge, so leave its
  # snapshot untouched.
  #
  # We only badge a *real* divergence (BT-2550): `save_method` logs a ChangeLog
  # entry for every `>>`, so flipping `disk_differs` unconditionally false-flags a
  # byte-for-byte re-save of the on-disk body (e.g. ⌘S with no edit). When we know
  # the on-disk body (`disk_source`, captured at open while the image matched
  # disk), compare the compiled source against it; only differ when they actually
  # differ. With no known disk body (`nil` — a runtime-only or already-diverged
  # method) we conservatively flag, matching the prior behaviour.
  defp compile_clean(socket, nil, _source), do: socket
  # `""` is the empty-state hidden form's `tab=""` sentinel (no tab to re-base);
  # short-circuit to a no-op rather than try to look up a tab by an empty id.
  defp compile_clean(socket, "", _source), do: socket

  defp compile_clean(socket, tab_id, source) do
    update_active_tab_by_id(socket, tab_id, fn tab ->
      %{
        tab
        | source: source,
          base: source,
          dirty: false,
          disk_differs: compiled_disk_differs(tab, source)
      }
    end)
  end

  # Whether a just-compiled `:method` body diverges from its on-disk counterpart.
  # `disk_source` is the on-disk body captured at open (see `disk_body_snapshot/1`)
  # — a precise body-to-body comparison, so an identical re-save reads as "in
  # sync". `nil` (no known disk body) falls back to a conservative `true`. A `:def`
  # tab has no single method body to diverge, so its snapshot is left untouched.
  defp compiled_disk_differs(%{kind: :method, disk_source: disk}, source) when is_binary(disk),
    do: source != disk

  defp compiled_disk_differs(%{kind: :method}, _source), do: true
  defp compiled_disk_differs(%{disk_differs: existing}, _source), do: existing

  # After a successful save *from a new-method tab*, promote it to an ordinary
  # method tab: stamp the author-supplied `selector`, flip `new: false`, and re-key
  # the tab id to the canonical `method:<class>:<side>:<selector>` so it matches
  # what `open_method_tab` would create. Without this the tab keeps `selector: ""`,
  # so `sync_active` re-seeds the selector input to "" on the post-save re-render
  # (wiping what the author typed) and a second ⌘S trips the empty-selector guard;
  # it would also leave a stale `new:<class>` tab alongside a later real method tab
  # for the same selector. A no-op for ordinary method/def tabs (the guard only
  # matches `new: true`). When a canonical method tab for the selector is already
  # open, the scratch tab is dropped and that existing tab is focused (no duplicate,
  # no stale "Class ▸ (new method)" left behind). `focus_tab_keep_banner/3` refreshes the
  # edit assigns so the now-hidden selector reflects the saved name.
  defp promote_new_method_tab(socket, tab_id, saved_class, selector) do
    case find_tab(socket, tab_id) do
      %{new: true, side: side, source: source} = tab ->
        # Key the id off `saved_class` — the class the Facade reports the method
        # was actually compiled onto — not the form-submitted class, so the id
        # always names the class behind it (a crafted event with a mismatched
        # `class` input can't desync the id from the compiled class).
        new_id = "method:" <> saved_class <> ":" <> side <> ":" <> selector
        tabs = socket.assigns.tabs

        case find_tab(socket, new_id) do
          nil ->
            # Re-key the scratch tab in place into the canonical method tab.
            promoted = %{tab | id: new_id, selector: selector, new: false}
            replaced = Enum.map(tabs, fn t -> if t.id == tab_id, do: promoted, else: t end)
            focus_tab_keep_banner(socket, replaced, promoted)

          existing ->
            # A canonical method tab for this selector is already open: drop the
            # redundant scratch tab and focus the existing one, rather than leaving
            # a stale "Class ▸ (new method)" tab alongside it. Re-base that tab on the body
            # just compiled (same post-compile treatment `compile_clean/3` gives the
            # in-place path) so the editor shows the saved source, not its browse
            # snapshot.
            rebased = %{
              existing
              | source: source,
                base: source,
                dirty: false,
                disk_differs: compiled_disk_differs(existing, source)
            }

            replaced =
              tabs
              |> Enum.reject(&(&1.id == tab_id))
              |> Enum.map(fn t -> if t.id == new_id, do: rebased, else: t end)

            focus_tab_keep_banner(socket, replaced, rebased)
        end

      _ ->
        socket
    end
  end

  # Focus `tab` and mirror it into the edit assigns *without* clearing the
  # save/flush result banners. `sync_active/2` resets those (the "switching tabs
  # starts clean" rule), which is wrong mid-save: promotion runs as part of a
  # successful save and must keep the "Saved …" banner it just set.
  defp focus_tab_keep_banner(socket, tabs, tab) do
    socket
    |> assign(:tabs, tabs)
    |> assign(:active_tab, tab.id)
    |> assign(
      edit_class: tab.class,
      edit_selector: tab.selector,
      edit_source: tab.source,
      edit_selection: nil
    )
  end

  defp update_active_tab(socket, fun),
    do: update_active_tab_by_id(socket, socket.assigns.active_tab, fun)

  defp update_active_tab_by_id(socket, id, fun) do
    tabs = Enum.map(socket.assigns.tabs, fn t -> if t.id == id, do: fun.(t), else: t end)
    assign(socket, :tabs, tabs)
  end

  # The Class › side › selector breadcrumb label parts for the active tab.
  defp breadcrumb(%{kind: :def, class: class}), do: {class, nil, "class definition"}
  # A new-method tab has no stored selector yet (BT-2606): derive it live from the
  # body the author is typing so the breadcrumb names the method as soon as a
  # recognizable signature appears, falling back to the `(new method)` placeholder
  # until then (parenthesised so it can't be mistaken for a real selector — BT-2613).
  defp breadcrumb(%{new: true, class: class, side: side, source: source}) do
    case parse_method_signature_selector(source) do
      "" -> {class, side, "(new method)"}
      selector -> {class, side, selector}
    end
  end

  defp breadcrumb(%{class: class, side: side, selector: selector}), do: {class, side, selector}

  # BT-2605: the colored modifier badges shown in the editor header for the active
  # tab. Each entry is `%{label, class, title}` — `label` is the visible text,
  # `class` the CSS modifier class (`.modifier-tag.<class>`), `title` the
  # accessible tooltip naming the modifier. Returns `[]` when the tab carries no
  # modifiers (an instance-side method on a plain class shows nothing). Order is
  # stable: side (Class) first, then class modifiers (sealed/abstract), then
  # native — mirroring the apidocs' labels (Sealed/Abstract) for docs↔IDE parity.
  defp modifier_badges(tab) do
    side_badge(tab) ++ class_modifier_badges(tab) ++ native_badge(tab)
  end

  # A class-side method gets a distinct "Class" badge; instance-side methods and
  # class-definition tabs get none here. `:def` tabs have `side: nil`.
  defp side_badge(%{side: "class"}) do
    [%{label: "Class", class: "side", title: "Class-side method"}]
  end

  defp side_badge(_tab), do: []

  # The reflected class-level modifiers (`sealed`, `abstract`) cached on the tab —
  # present on every tab kind (BT-2605 threads it onto method tabs too). Labels are
  # capitalized to match the apidocs.
  defp class_modifier_badges(%{class_modifiers: mods}) when is_list(mods) do
    Enum.map(mods, &class_modifier_badge/1)
  end

  # No modifier list (key absent, or `nil` from a transient fetch failure) → no
  # class-modifier badges.
  defp class_modifier_badges(_tab), do: []

  # The canonical class-modifier badge labels/colors, keyed by the reflected
  # modifier atom. Kept exhaustive against `class_modifiers_from/1`'s atom set so a
  # future modifier is a compile-visible addition in both places.
  defp class_modifier_badge(:sealed),
    do: %{label: "Sealed", class: "sealed", title: "Cannot be subclassed by user code"}

  defp class_modifier_badge(:typed),
    do: %{
      label: "Typed",
      class: "typed",
      title: "All fields and methods require type annotations"
    }

  defp class_modifier_badge(:abstract),
    do: %{
      label: "Abstract",
      class: "abstract",
      title: "Must be subclassed; not directly instantiable"
    }

  # A `native:` class (ADR 0056) gets a Native badge. The flag is `class_native`
  # (set on every tab kind) rather than `native_module` so the badge shows on
  # method tabs too, while `native_module` stays reserved for the :def-only native
  # pane.
  defp native_badge(%{class_native: true}) do
    [%{label: "Native", class: "native", title: "Backed by an Erlang module (native: class)"}]
  end

  defp native_badge(_tab), do: []

  # Parse the selector from a method's source signature (BT-2606), returning `""`
  # when no valid signature is present yet. The author writes the full method
  # (`selector ... => body`) in the editor body; this recovers the selector the
  # same way the compiler's method header grammar does, so the new-method tab no
  # longer needs a separate selector input.
  #
  # It is intentionally conservative: a string it can't confidently read as a
  # header returns `""` (treated as "no signature yet"). The authoritative parse
  # still runs server-side in the `:save` op, which rejects a selector that
  # disagrees with the compiled body — so this client-side read only has to be
  # right for the breadcrumb hint and the pre-flight validation, never the install
  # key of record.
  defp parse_method_signature_selector(source) when is_binary(source) do
    source
    |> strip_leading_comments()
    |> strip_method_modifiers()
    |> selector_from_header()
  end

  defp parse_method_signature_selector(_source), do: ""

  # Drop leading blank lines and whole-line `//` / `///` comments, returning the
  # first line that begins a real method header (mirrors the backend
  # `skip_leading_comments/1`).
  defp strip_leading_comments(source) do
    trimmed = String.trim_leading(source)

    if String.starts_with?(trimmed, "//") do
      case String.split(trimmed, "\n", parts: 2) do
        [_comment, rest] -> strip_leading_comments(rest)
        [_comment] -> ""
      end
    else
      trimmed
    end
  end

  # Drop leading method modifiers (`class`, `internal`, `sealed`) that may precede
  # the selector, matching the parser's `parse_method_definition` modifier loop. A
  # modifier word is only stripped when more header text follows it — a bare
  # `class =>` is a method *named* `class`, not the `class` modifier.
  defp strip_method_modifiers(header) do
    case String.split(header, ~r/\s+/, parts: 2) do
      [word, rest] when word in ["class", "internal", "sealed"] ->
        # `class`/`internal`/`sealed` are modifiers only when a selector follows.
        cond do
          # The next token opens the body (`=>`): the word itself is the (unary)
          # selector — a method *named* `class`/`internal`/`sealed`.
          String.starts_with?(rest, "=>") ->
            header

          # The next token is `->`. This is ambiguous without type context
          # (BT-2625): `sealed -> Type =>` is a unary method named `sealed` with a
          # return type, while `sealed -> arg =>` is a `sealed` *binary* method
          # whose selector is `->`. Disambiguate by the token after `->`, matching
          # the parser's grammar: a Capitalized token reads as a return Type (the
          # modifier word is the unary selector, so keep the header), a lowercase
          # token reads as a binary-selector argument (strip the modifier so `->`
          # becomes the selector).
          String.starts_with?(rest, "->") ->
            if return_type_follows_arrow?(rest), do: header, else: strip_method_modifiers(rest)

          true ->
            strip_method_modifiers(rest)
        end

      _ ->
        header
    end
  end

  # Decide whether the text starting at a `->` is a *return-type* annotation
  # (`-> Type`) rather than a binary selector whose argument follows (`-> arg`).
  # A return type is a Capitalized identifier (a Type name); a binary-selector
  # parameter is a lowercase identifier. Matches the compiler's method-header
  # grammar (BT-2625). A `->` with no following identifier is treated as a binary
  # selector (not a return type).
  defp return_type_follows_arrow?(rest) do
    rest
    |> String.replace_prefix("->", "")
    |> String.trim_leading()
    |> case do
      <<first::utf8, _::binary>> -> first in ?A..?Z
      _ -> false
    end
  end

  # Recover the selector token(s) from the start of a method header: a keyword
  # selector (`at:put:` from `at: i put: v => …`), a binary selector (`+`, `->`,
  # `>>`, … from `+ other => …`), or a unary selector (`increment` from
  # `increment => …`). Returns `""` when the head doesn't read as a header (no
  # `=>` before the first statement break, or an empty/blank head).
  defp selector_from_header(""), do: ""

  defp selector_from_header(header) do
    # Only the header up to the body arrow can contribute a selector — bail
    # when there is no `=>` before the first newline.
    head =
      header
      |> String.split(~r/\r?\n/, parts: 2)
      |> List.first()

    cond do
      not String.contains?(head, "=>") ->
        ""

      true ->
        head
        |> String.split("=>", parts: 2)
        |> List.first()
        |> selector_from_signature_text()
    end
  end

  # Extract the selector from the text *before* the `=>` arrow. Keyword selectors
  # are the concatenation of their `keyword:` parts (parameter names, type
  # annotations and the return type are dropped); a binary selector is its leading
  # operator; a unary selector is its single identifier.
  defp selector_from_signature_text(sig) do
    sig = String.trim(sig)

    # `:(?!:)` so a compact type annotation (`i::Integer`) doesn't capture `i:` as
    # a keyword part — only a real `keyword:` (single colon) counts.
    keyword_parts = Regex.scan(~r/([A-Za-z_][A-Za-z0-9_]*):(?!:)/, sig, capture: :all_but_first)

    cond do
      sig == "" ->
        ""

      keyword_parts != [] ->
        keyword_parts |> Enum.map(fn [k] -> k <> ":" end) |> Enum.join()

      true ->
        case Regex.run(~r/^([A-Za-z_][A-Za-z0-9_]*)/, sig) do
          [_, ident] ->
            ident

          nil ->
            # Binary selector: the leading run of operator characters (e.g. `+`,
            # `->`, `>>`, `<=`). Anything else is not a recognizable header.
            case Regex.run(~r/^([-+*\/~<>=&|@%^?]+)/, sig) do
              [_, op] -> op
              nil -> ""
            end
        end
    end
  end

  # The method shown in the focused tab as a `%{class, side, selector}` ref (or nil
  # for a class-definition tab), so the System Browser can highlight the matching
  # method row. Takes bare `assigns` for the render template. An unsaved new-method
  # tab has no real selector yet (`new: true`, `selector: ""`), so it highlights
  # nothing — returning a `selector: ""` ref would never match a row anyway.
  defp selected_method_ref(assigns) do
    case active_tab(assigns) do
      %{new: true} ->
        nil

      %{kind: :method, class: class, side: side, selector: selector} ->
        %{class: class, side: side, selector: selector}

      _ ->
        nil
    end
  end

  # The class whose definition tab is focused, so the System Browser's "class
  # definition" entry can track the editor (mirrors `selected_method_ref/1` for
  # method tabs). nil when the active tab is a method or there is no def tab.
  defp selected_def_ref(assigns) do
    case active_tab(assigns) do
      %{kind: :def, class: class} -> class
      _ -> nil
    end
  end

  # ── bindings + inspector helpers ────────────────────────────────────────────

  # Read the session's live bindings through the read-surface and assign display
  # rows. Each row keeps the live `term` so the Inspector can follow object
  # references without a string round-trip; `inspectable?` flags object-valued
  # bindings the user can drill into.
  #
  # Split into the off-socket read + pure fold (BT-2591) so the mount-load async
  # task and the bindings-changed refresh path share one fold.
  defp assign_bindings(socket, pid), do: apply_bindings(socket, read_bindings(socket, pid))

  defp read_bindings(socket, pid),
    do: Facade.dispatch(:bindings, %{session_pid: pid}, ctx(socket))

  defp apply_bindings(socket, {:error, reason}),
    do: assign(socket, bindings: [], bindings_error: Workspace.render_error(reason))

  defp apply_bindings(socket, pairs) when is_list(pairs) do
    rows =
      Enum.map(pairs, fn {name, term} ->
        %{
          name: name,
          value: Workspace.render_term(term),
          inspectable: Workspace.inspectable?(term),
          kind: term_kind(term)
        }
      end)

    assign(socket, bindings: rows, bindings_error: nil)
  end

  # Defensive catch-all (BT-2591): folded from the async mount load AND the sync
  # `assign_bindings/2` refresh path. An unexpected shape degrades to an empty
  # pane with an error rather than crashing the LiveView.
  defp apply_bindings(socket, unexpected) do
    Logger.warning("unexpected bindings result: #{inspect(unexpected)}",
      domain: [:beamtalk, :liveview]
    )

    assign(socket, bindings: [], bindings_error: Workspace.render_error(:unexpected_response))
  end

  # Inspect a binding selected by name: resolve its live term from the current
  # binding list, then inspect that term. The term — not a string — drives the op.
  defp inspect_binding(socket, pid, name) do
    case Facade.dispatch(:bindings, %{session_pid: pid}, ctx(socket)) do
      pairs when is_list(pairs) ->
        case List.keyfind(pairs, name, 0) do
          # Inspecting a binding starts a fresh drill breadcrumb at this object.
          {^name, term} ->
            inspect_term(socket, name, term, [%{label: to_string(name), term: term}])

          nil ->
            assign(socket, inspect_error: "binding not found: #{name}")
        end

      {:error, reason} ->
        assign(socket, inspect_error: Workspace.render_error(reason))
    end
  end

  # Inspect a single live term via the read-surface `inspect` op and assign the
  # resulting structured-field rows plus the drill breadcrumb (`crumbs`). Object-
  # valued fields are flagged drillable, carrying their live term so the next
  # drill follows the reference one level deeper. Non-object terms are not
  # inspectable, so we say so rather than guess.
  defp inspect_term(socket, label, term, crumbs) do
    if Workspace.inspectable?(term) do
      case Facade.dispatch(:inspect, %{term: term}, ctx(socket)) do
        # BT-2634: a supervisor's content is its CHILDREN / supervision tree, not
        # actor instance vars. Each child row carries a live `{:beamtalk_supervisor,
        # …}` / `{:beamtalk_object, …}` handle, so the existing "drill" event
        # follows it as its own reference (ADR 0095), and the crumb walk-back
        # re-inspects the supervisor handle (re-listing its children). Live-tracking
        # is deliberately NOT armed for a supervisor (`track_object/2`'s catch-all):
        # no field-flash, no per-object watch, no pid-stats poll against a supervisor.
        {:ok, {:supervisor_children, child_rows}} ->
          socket
          |> assign(
            inspect_target: target_info(label, term),
            inspect_rows: supervisor_child_rows(child_rows),
            inspect_crumbs: crumbs,
            inspect_error: nil
          )
          |> track_object(term)

        {:ok, fields} when is_map(fields) ->
          socket
          |> assign(
            inspect_target: target_info(label, term),
            inspect_rows: field_rows(fields),
            inspect_crumbs: crumbs,
            inspect_error: nil
          )
          |> track_object(term)

        {:ok, scalar} ->
          socket
          |> assign(
            inspect_target: target_info(label, term),
            inspect_rows: [
              %{
                name: "value",
                value: Workspace.format_value(scalar),
                term: scalar,
                drillable: false,
                kind: term_kind(scalar)
              }
            ],
            inspect_crumbs: crumbs,
            inspect_error: nil
          )
          |> track_object(term)

        {:error, reason} ->
          # A failed inspect leaves no coherent head: reset the crumbs + rows so a
          # later freeze/poke doesn't act on a stale level, and drop any watch.
          socket
          |> assign(
            inspect_target: nil,
            inspect_rows: [],
            inspect_crumbs: [],
            inspect_error: Workspace.render_error(reason)
          )
          |> track_object(nil)
      end
    else
      socket
      |> assign(
        inspect_target: target_info(label, term),
        inspect_rows: [],
        inspect_crumbs: crumbs,
        inspect_error: "#{label} is a #{scalar_kind(term)} — no fields to inspect"
      )
      |> track_object(term)
    end
  end

  # ── live Inspector tracking (BT-2492, backend BT-2489 / ADR 0095 §5) ─────────

  # Arm (or tear down) the per-object change subscription + pid-stats read for the
  # newly-inspected `term`, called on every `inspect_term/4` so re-inspecting a
  # different object (a drill, a crumb walk-back, a fresh binding) rebinds the
  # watch onto the *current* object and drops the previous one. The flow keeps the
  # contract honest:
  #
  #   * A pid-backed object → subscribe THIS LiveView pid (over distribution) to
  #     its `{:object_changed, …}` stream, read its pid stats now, and clear any
  #     stale poke result. A frozen pane does NOT subscribe (it holds a snapshot)
  #     but still reads stats once so the chips reflect the snapshot.
  #   * A non-pid term (a scalar field, a drilled value) has nothing to watch:
  #     drop any prior subscription and clear the stats/watch.
  #
  # The previously-watched term (`:inspect_watch`) is always unsubscribed first so
  # the workspace never keeps pushing changes for an object we navigated away from.
  defp track_object(socket, {:beamtalk_object, _class, _module, pid} = term) when is_pid(pid) do
    # Reset any in-flight coalesced-refresh flag: a pending `:do_object_refresh`
    # timer was scheduled for the *previous* object, so clearing the flag lets the
    # NEW object's first change push schedule its own refresh immediately (the
    # stale timer, if it still fires, is a harmless no-op on the fresh watch).
    socket = unwatch(assign(socket, refresh_pending: false))

    socket =
      if socket.assigns.inspect_frozen do
        # Frozen: hold the snapshot — no live subscription, but read stats once so
        # the chips populate. `:inspect_watch` stays nil (nothing to unsubscribe).
        assign(socket, inspect_watch: nil)
      else
        case Facade.dispatch(:subscribe_object, %{term: term, pid: self()}, ctx(socket)) do
          :ok -> assign(socket, inspect_watch: term)
          # A non-:ok (term not watchable, dist hiccup) leaves the pane un-watched
          # rather than claiming a live subscription that isn't there.
          _ -> assign(socket, inspect_watch: nil)
        end
      end

    socket
    |> refresh_stats(term)
    |> assign(poke_result: nil, poke_error: nil)
  end

  # Non-object target: nothing to track. Drop any prior watch and clear stats.
  defp track_object(socket, _term) do
    socket
    |> unwatch()
    |> assign(inspect_stats: nil, poke_result: nil, poke_error: nil)
  end

  # Drop the docked Inspector's per-object subscription (if any) and forget the
  # watched term. Idempotent: a nil watch unsubscribes nothing.
  #
  # Reference-aware (BT-2493): the workspace keys subscriptions by `(pid,
  # subscriber)` and our subscriber is always this LiveView, so one unsubscribe
  # would silence EVERY this-pid watcher in this process. If a floating window
  # still watches the same actor, we must NOT unsubscribe here — the window still
  # needs the push. (Before floating windows existed the docked pane was the sole
  # watcher, so this collapses to the original unconditional unsubscribe.)
  defp unwatch(%{assigns: %{inspect_watch: term}} = socket)
       when not is_nil(term) do
    unless docked_pid_watched_by_window?(socket, term) do
      Facade.dispatch(:unsubscribe_object, %{term: term, pid: self()}, ctx(socket))
    end

    assign(socket, inspect_watch: nil)
  end

  defp unwatch(socket), do: assign(socket, inspect_watch: nil)

  # True when the pid backing the docked pane's `term` is also watched by an open
  # floating window — so the docked pane releasing it must keep the subscription
  # alive for the window.
  defp docked_pid_watched_by_window?(socket, {:beamtalk_object, _c, _m, pid})
       when is_pid(pid) do
    Enum.any?(socket.assigns[:windows] || [], fn w -> watched_pid?(w.watch, pid) end)
  end

  defp docked_pid_watched_by_window?(_socket, _term), do: false

  # Read the inspected actor's live process metrics (mailbox/reductions/status/…)
  # and assign the snapshot for the head chips. A read failure clears the chips
  # rather than rendering stale numbers — the change stream still drives the field
  # flash, so the pane stays useful even when stats are momentarily unavailable.
  defp refresh_stats(socket, term) do
    case Facade.dispatch(:pid_stats, %{term: term}, ctx(socket)) do
      {:ok, stats} when is_map(stats) -> assign(socket, inspect_stats: stats)
      _ -> assign(socket, inspect_stats: nil)
    end
  end

  # Re-read the *already-watched* object's fields + stats after a change push
  # (BT-2492) WITHOUT re-arming the subscription — the watch is still live, so
  # `track_object/2` would needlessly unsubscribe + resubscribe. The drill
  # breadcrumb is preserved (same level); only the field values + stats refresh.
  # `:flash_gen` bumps so the FieldFlash hook flashes the changed cells. The
  # target label/crumbs come from the current head (the last crumb's label).
  defp refresh_inspector(socket, {:beamtalk_object, _class, _module, pid} = term)
       when is_pid(pid) do
    label = current_inspect_label(socket)

    case Facade.dispatch(:inspect, %{term: term}, ctx(socket)) do
      {:ok, fields} when is_map(fields) ->
        socket
        |> assign(
          inspect_target: target_info(label, term),
          inspect_rows: field_rows(fields),
          inspect_error: nil
        )
        |> refresh_stats(term)
        |> bump_flash()

      {:ok, _scalar} ->
        # The object resolved to a scalar (no fields) — refresh stats + flash; the
        # row already reflects the value the next render reads.
        socket |> refresh_stats(term) |> bump_flash()

      {:error, _reason} ->
        # A transient read failure on a live refresh: keep the existing rows rather
        # than blanking the pane mid-track; the next push retries.
        socket
    end
  end

  defp refresh_inspector(socket, _term), do: socket

  # The label of the inspector head right now — the last drill crumb, falling back
  # to the target label. Used so a live refresh re-renders the head with the same
  # label the user navigated to.
  defp current_inspect_label(socket) do
    case List.last(socket.assigns.inspect_crumbs) do
      %{label: label} -> label
      _ -> (socket.assigns.inspect_target || %{})[:label] || "value"
    end
  end

  defp bump_flash(socket), do: assign(socket, :flash_gen, socket.assigns.flash_gen + 1)

  # The coalescing window for a burst of `{:object_changed, …}` pushes. A small
  # delay collapses a flurry of rapid writes into a single re-read + flash. Kept as
  # a function so a test can drive it deterministically if needed.
  defp refresh_debounce_ms, do: 60

  # ── floating inspector windows (BT-2493, epic BT-2482 Phase 3) ───────────────
  #
  # Each floating window is a self-contained inspector: its own drill `crumbs`,
  # `rows`, `target`, live `watch`/`stats`/`frozen` and a `flash_gen`, plus its
  # `x`/`y`/`z` placement (client-reported by the WindowDrag hook on drop / focus).
  # The window-list lives in `:windows`; helpers below open / drill / close / focus
  # / freeze a window by id and route change pushes to the right window. They reuse
  # the same read-surface ops (`:inspect`, `:subscribe_object`, `:pid_stats`) and
  # the same `target_info` / `field_rows` builders as the docked pane (BT-2486), so
  # a window's content is the docked Inspector parameterised by window id.

  # The window placement step (px): each new window is offset down-right from the
  # last so a burst of opens cascades rather than stacking dead-on (the spike's
  # `+24` cascade). The first window opens near the top-left of the overlay.
  defp window_origin_x, do: 120
  defp window_origin_y, do: 96
  defp window_cascade, do: 28

  # Open a floating window on a session binding selected by name: resolve its live
  # term (same path as the docked `inspect_binding/3`), then build a window on it.
  # A binding that no longer resolves opens a window showing the error rather than
  # silently doing nothing.
  defp open_window(socket, pid, name) do
    case Facade.dispatch(:bindings, %{session_pid: pid}, ctx(socket)) do
      pairs when is_list(pairs) ->
        case List.keyfind(pairs, name, 0) do
          {^name, term} -> open_window_for_term(socket, to_string(name), term)
          nil -> open_window_error(socket, to_string(name), "binding not found: #{name}")
        end

      {:error, reason} ->
        open_window_error(socket, to_string(name), Workspace.render_error(reason))
    end
  end

  # Open a floating window on an already-resolved live `term` (used by Inspect-it,
  # whose head is the `→ result`, and by `open_window/3`). Mints a fresh id +
  # cascade position, fills its first inspector level, and arms its watch.
  defp open_window_for_term(socket, label, term) do
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

    win = inspect_window(socket, win, label, term, win.crumbs)

    socket
    |> assign(:windows, socket.assigns.windows ++ [win])
    |> assign(:window_z, z)
  end

  # Open a window that carries only an error head (a binding that vanished): still
  # gives the user a closable window explaining what happened rather than a no-op.
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
  # A LiveView reconnect (transient socket drop / page reload) mounts a brand-new
  # process whose assigns — including the open floating-window list — start empty,
  # even though the underlying workspace session resumes with its bindings intact.
  # Without help, a desk full of inspector windows silently vanishes on a blip. So
  # `terminate/2` stashes the open windows' ROOTS in the registry (Phoenix-node
  # memory that outlives the reconnect) and the resuming mount rebuilds them here.

  # Snapshot the open floating windows into a resume stash: each window's root
  # (its first crumb — label + live term) plus placement and freeze, alongside the
  # inspector mode. Drilled levels are intentionally dropped — a resume restores
  # each window to its root (the issue's "or at least the roots"). Error-only
  # windows carry no root term, so there's nothing live to reopen — they're
  # skipped by the crumb pattern.
  defp build_window_stash(socket) do
    roots =
      for %{crumbs: [%{label: label, term: term} | _]} = w <- socket.assigns[:windows] || [] do
        %{label: to_string(label), term: term, x: w.x, y: w.y, z: w.z, frozen: w.frozen}
      end

    %{windows: roots, mode: socket.assigns[:inspector_mode] || "docked"}
  end

  # Rebuild the stashed desk on a genuine session resume. A fresh session or a
  # failed bind (not connected) leaves the empty desk untouched. Each stashed root
  # is reopened from its still-live term — the inspected actor lives on the
  # workspace node and survives the LiveView reconnect — which re-reads its fields
  # and re-arms its per-object watch for the NEW LiveView pid, then it is placed at
  # its saved position and re-frozen. The mode is restored too so a resumed Float
  # desk comes back in Float.
  defp restore_windows(socket, _token, :fresh), do: socket

  defp restore_windows(socket, token, :resumed) do
    with true <- socket.assigns[:connected],
         %{windows: [_ | _] = roots, mode: mode} <- SessionRegistry.window_stash(token) do
      socket = assign(socket, :inspector_mode, mode)
      Enum.reduce(roots, socket, &restore_window/2)
    else
      _ -> socket
    end
  end

  # Reopen one stashed root, then override the cascade position `open_window_for_term`
  # assigned with the stashed placement and re-apply its freeze.
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

  # Restore the method-editor doc-block expand state on a genuine session resume
  # (BT-2570). The block's `:doc_expanded` is a socket assign that a fresh mount —
  # which every reconnect is — re-inits to its collapsed default, so a user who
  # expanded it would lose that on any transient socket drop, redeploy, or laptop
  # wake. `terminate/2` stashes the flag in the registry (Phoenix-node memory that
  # outlives the reconnect); here we read it back and re-apply it. A fresh session
  # or a failed bind (not connected) leaves the collapsed default untouched; a
  # missing stash (nothing was expanded) likewise leaves the default.
  defp restore_doc(socket, _token, :fresh), do: socket

  defp restore_doc(socket, token, :resumed) do
    with true <- socket.assigns[:connected],
         expanded when is_boolean(expanded) <- SessionRegistry.doc_stash(token) do
      assign(socket, :doc_expanded, expanded)
    else
      _ -> socket
    end
  end

  defp restore_window_freeze(socket, _id, false), do: socket

  defp restore_window_freeze(socket, id, true) do
    update_window(socket, id, fn w -> toggle_window_freeze(socket, w) end)
  end

  # Re-cascade every open window back onto the default on-screen ladder (BT-2527
  # #4): a recovery affordance for a window dragged to (or restored at) a spot
  # outside the visible viewport. Positions only — drill state, watches, stats and
  # freeze are untouched.
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

  # Inspect a live `term` into a window `w` (parameterised twin of the docked
  # `inspect_term/4`): read the object's fields via the read-surface, set the
  # window's target/rows/crumbs/error, then (re)arm its per-object watch + stats.
  # A re-inspect (drill / crumb walk-back) rebinds the watch onto the new head and
  # releases the previous one — but only if no OTHER watcher (window or docked
  # pane) still needs that pid (so closing one of two windows on the same actor
  # doesn't silence the other).
  defp inspect_window(socket, w, label, term, crumbs) do
    if Workspace.inspectable?(term) do
      case Facade.dispatch(:inspect, %{term: term}, ctx(socket)) do
        # BT-2634: a supervisor renders its children / supervision tree (drillable
        # child handles), not actor instance vars — the float-window twin of the
        # docked supervisor case. `track_window/3` does not arm a watch for a
        # supervisor term (its no-track catch-all), so no field-flash / pid-stats.
        {:ok, {:supervisor_children, child_rows}} ->
          %{
            w
            | target: target_info(label, term),
              rows: supervisor_child_rows(child_rows),
              crumbs: crumbs,
              error: nil
          }
          |> track_window(socket, term)

        {:ok, fields} when is_map(fields) ->
          %{
            w
            | target: target_info(label, term),
              rows: field_rows(fields),
              crumbs: crumbs,
              error: nil
          }
          |> track_window(socket, term)

        {:ok, scalar} ->
          %{
            w
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
          |> track_window(socket, scalar)

        {:error, reason} ->
          %{
            w
            | target: nil,
              rows: [],
              crumbs: [],
              error: Workspace.render_error(reason)
          }
          |> track_window(socket, nil)
      end
    else
      %{
        w
        | target: target_info(label, term),
          rows: [],
          crumbs: crumbs,
          error: "#{label} is a #{scalar_kind(term)} — no fields to inspect"
      }
      |> track_window(socket, term)
    end
  end

  # Arm (or tear down) a window's per-object change subscription + pid-stats read
  # for `term`, called on every `inspect_window/5`. Mirrors the docked
  # `track_object/2` but scoped to one window's `watch`. The previously-watched
  # term is released first (guarded so a pid another watcher still needs is kept
  # subscribed), then a non-frozen pid-backed head re-subscribes + reads stats.
  defp track_window(w, socket, {:beamtalk_object, _c, _m, pid} = term) when is_pid(pid) do
    w = unwatch_window(w, socket)

    w =
      if w.frozen do
        %{w | watch: nil}
      else
        case Facade.dispatch(:subscribe_object, %{term: term, pid: self()}, ctx(socket)) do
          :ok -> %{w | watch: term}
          _ -> %{w | watch: nil}
        end
      end

    %{w | refresh_pending: false}
    |> refresh_window_stats(socket, term)
    |> Map.merge(%{poke_result: nil, poke_error: nil})
  end

  defp track_window(w, socket, _term) do
    w
    |> unwatch_window(socket)
    |> Map.merge(%{stats: nil, poke_result: nil, poke_error: nil})
  end

  # Drop a window's per-object subscription, unless the same pid is still watched
  # by another window or the docked pane (reference-aware unsubscribe). Idempotent.
  defp unwatch_window(%{watch: nil} = w, _socket), do: w

  defp unwatch_window(%{watch: term} = w, socket) do
    unless pid_watched_elsewhere?(socket, term, w.id) do
      Facade.dispatch(:unsubscribe_object, %{term: term, pid: self()}, ctx(socket))
    end

    %{w | watch: nil}
  end

  # True when the pid backing `term` is still watched by some watcher OTHER than
  # window `except_id` — another floating window, or the docked Inspector. Used to
  # avoid unsubscribing a pid a sibling watcher still depends on (the workspace
  # keys subscriptions by `(pid, subscriber)`, so one unsubscribe would silence all
  # this-pid watchers in this LiveView).
  defp pid_watched_elsewhere?(socket, {:beamtalk_object, _c, _m, pid}, except_id)
       when is_pid(pid) do
    docked = watched_pid?(socket.assigns[:inspect_watch], pid)

    windowed =
      Enum.any?(socket.assigns.windows, fn w ->
        w.id != except_id and watched_pid?(w.watch, pid)
      end)

    docked or windowed
  end

  defp pid_watched_elsewhere?(_socket, _term, _except_id), do: false

  # Read a window's inspected actor's live pid stats for its head chips. A failure
  # clears the chips rather than rendering stale numbers (same contract as the
  # docked `refresh_stats/2`).
  defp refresh_window_stats(w, socket, term) do
    case Facade.dispatch(:pid_stats, %{term: term}, ctx(socket)) do
      {:ok, stats} when is_map(stats) -> %{w | stats: stats}
      _ -> %{w | stats: nil}
    end
  end

  # Re-read a window's *already-watched* object after a change push WITHOUT
  # re-arming the subscription (the watch is still live). Bumps the window's
  # `flash_gen` so its FieldFlash hook flashes the changed cells. Mirrors the
  # docked `refresh_inspector/2`.
  defp refresh_window(w, socket, {:beamtalk_object, _c, _m, pid} = term) when is_pid(pid) do
    label = window_label(w)

    case Facade.dispatch(:inspect, %{term: term}, ctx(socket)) do
      {:ok, fields} when is_map(fields) ->
        %{
          w
          | target: target_info(label, term),
            rows: field_rows(fields),
            error: nil
        }
        |> refresh_window_stats(socket, term)
        |> bump_window_flash()

      {:ok, _scalar} ->
        w |> refresh_window_stats(socket, term) |> bump_window_flash()

      {:error, _reason} ->
        w
    end
  end

  # The label at a window's current head: its last drill crumb, falling back to its
  # target label.
  defp window_label(w) do
    case List.last(w.crumbs) do
      %{label: label} -> label
      _ -> (w.target || %{})[:label] || "value"
    end
  end

  defp bump_window_flash(w), do: %{w | flash_gen: w.flash_gen + 1}

  # Per-window freeze toggle (the docked `toggle_freeze/1`, scoped to one window).
  # Unfreeze re-arms the window's watch on its current head and catches up; freeze
  # drops its subscription (reference-aware) and holds the snapshot.
  defp toggle_window_freeze(socket, %{frozen: true} = w) do
    w = %{w | frozen: false, refresh_pending: false}

    case window_head_term(w) do
      {:beamtalk_object, _c, _m, pid} = term when is_pid(pid) ->
        w
        |> rearm_window_watch(socket, term)
        |> refresh_window(socket, term)

      _ ->
        w
    end
  end

  defp toggle_window_freeze(socket, w) do
    w
    |> unwatch_window(socket)
    |> Map.put(:frozen, true)
  end

  defp rearm_window_watch(w, socket, term) do
    case Facade.dispatch(:subscribe_object, %{term: term, pid: self()}, ctx(socket)) do
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

  # Send `message` to a window's inspected actor by eval'ing `<binding> <message>`
  # against the session (the docked `poke_object/2`, scoped to one window). The
  # actor is addressed by its window's single-crumb binding label; a window not at
  # a named-binding root can't be poked. On success the window re-reads so its
  # fields reflect the write synchronously (the docked `refresh_poked_inspector`).
  defp poke_window(socket, w, message) do
    message = String.trim(message)
    pid = socket.assigns[:session_pid]
    label = window_poke_label(w)

    cond do
      not is_pid(pid) ->
        %{w | poke_result: nil, poke_error: "not attached to workspace"}

      message == "" ->
        %{w | poke_result: nil, poke_error: "Enter a message to send."}

      is_nil(label) ->
        %{
          w
          | poke_result: nil,
            poke_error: "Can only send to a bound object — inspect a binding to poke it."
        }

      true ->
        send_window_poke(socket, w, pid, label, message)
    end
  end

  defp send_window_poke(socket, w, pid, label, message) do
    code = "#{label} #{message}"

    case Facade.dispatch(:eval, %{session_pid: pid, code: code}, ctx(socket)) do
      {:ok, term, _output, _warnings} ->
        w = %{w | poke_result: "→ #{Workspace.render_term(term)}", poke_error: nil}

        # Re-read only when this window is live (`watch` is a pid). A FROZEN window
        # shows the poke result but deliberately does NOT re-read its field rows
        # (BT-2527 #6, reviewed): frozen means "snapshot", and overriding it here
        # would diverge from the docked poke, which is governed by the same rule —
        # surface parity over a one-off exception. Unfreeze to see the new state.
        case w.watch do
          {:beamtalk_object, _c, _m, p} = watched when is_pid(p) ->
            refresh_window(w, socket, watched)

          _ ->
            w
        end

      {:error, reason, _output, _warnings} ->
        %{w | poke_result: nil, poke_error: Workspace.render_error(reason)}

      {:error, reason} ->
        %{w | poke_result: nil, poke_error: facade_error(reason)}
    end
  end

  # The binding name to address a window poke to (its single-crumb root label), or
  # nil — the same well-formedness gate as the docked `poke_label/1`.
  defp window_poke_label(%{crumbs: [%{label: label}]}) when is_binary(label) do
    if valid_receiver?(label), do: label, else: nil
  end

  defp window_poke_label(_w), do: nil

  # ── window-list manipulation (pure view state, no workspace round-trip) ──────

  # Mint the next unique window id (a string so it rides DOM ids + phx-value cleanly)
  # and advance the counter.
  defp mint_window_id(socket) do
    n = socket.assigns.next_window_id
    {"win-#{n}", assign(socket, :next_window_id, n + 1)}
  end

  # The cascade position for the next window: offset down-right by one step per
  # already-open window, wrapping after a few so a long-lived session doesn't march
  # windows off-screen. Returns {x, y, socket} (socket unchanged — kept symmetric
  # with `mint_window_id/1` for a tidy call site).
  defp next_window_pos(socket) do
    step = rem(length(socket.assigns.windows), 8)
    x = window_origin_x() + step * window_cascade()
    y = window_origin_y() + step * window_cascade()
    {x, y, socket}
  end

  # Look up a window by id, or nil.
  defp find_window(socket, id), do: Enum.find(socket.assigns.windows, &(&1.id == id))

  # Whitelist mapping a client-supplied dismiss key → the scalar status assign to
  # clear (BT-2612). This is the security boundary: only these exact strings
  # resolve; everything else returns nil and is ignored by `dismiss_notice`.
  # NEVER replace this with `String.to_atom/1` on the user-supplied key.
  defp dismiss_key_to_assign("browser_error"), do: :browser_error
  defp dismiss_key_to_assign("output"), do: :output
  defp dismiss_key_to_assign("changes_error"), do: :changes_error
  defp dismiss_key_to_assign("git_error"), do: :git_error
  defp dismiss_key_to_assign("tests_error"), do: :tests_error
  defp dismiss_key_to_assign("save_result"), do: :save_result
  defp dismiss_key_to_assign("save_error"), do: :save_error
  defp dismiss_key_to_assign("flush_result"), do: :flush_result
  defp dismiss_key_to_assign("flush_error"), do: :flush_error
  defp dismiss_key_to_assign("bindings_error"), do: :bindings_error
  defp dismiss_key_to_assign("inspect_error"), do: :inspect_error
  defp dismiss_key_to_assign(_unknown), do: nil

  # Replace the window `id` with `fun.(window)`, leaving the list order (and so the
  # DOM order) stable — z-order is carried by each window's `:z`, not list position,
  # so an update never reshuffles the windows and resets their drag positions.
  defp update_window(socket, id, fun) do
    windows =
      Enum.map(socket.assigns.windows, fn w ->
        if w.id == id, do: fun.(w), else: w
      end)

    assign(socket, :windows, windows)
  end

  # Close a window: release its watch (reference-aware) and drop it from the list.
  defp close_window(socket, id) do
    case find_window(socket, id) do
      nil ->
        socket

      w ->
        _ = unwatch_window(w, socket)
        assign(socket, :windows, Enum.reject(socket.assigns.windows, &(&1.id == id)))
    end
  end

  # Bring window `id` to the front: bump its z above the current max so it overlays
  # the others. No-op for an unknown id.
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

  # Clamp a client-reported drag coordinate to a non-negative integer (a crafted
  # payload could send a float/NaN/negative).
  defp clamp_coord(n) when is_integer(n) and n >= 0, do: n
  defp clamp_coord(n) when is_float(n) and n >= 0.0, do: trunc(n)

  defp clamp_coord(n) when is_binary(n) do
    case Integer.parse(n) do
      {i, _} when i >= 0 -> i
      _ -> 0
    end
  end

  defp clamp_coord(_), do: 0

  # Route an `{:object_changed, pid, …}` push to every open floating window whose
  # watched head is that pid (a non-frozen, currently-watching window). Each such
  # window coalesces its own burst via its `:refresh_pending` flag and schedules a
  # per-window deferred refresh. Windows watching a different pid (or frozen) are
  # untouched. Returns the updated socket.
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

  # Flip the freeze flag and (un)arm tracking accordingly. Freezing unsubscribes
  # the live object change stream (the pane now holds a snapshot). Unfreezing
  # re-subscribes the current head object and refreshes it so it catches up.
  defp toggle_freeze(%{assigns: %{inspect_frozen: true}} = socket) do
    # Unfreeze: re-arm tracking on the current head term (if any) and catch up.
    # Clear any stale `refresh_pending` too: a timer scheduled before the freeze
    # could otherwise fire a redundant second refresh (double flash) right after
    # this catch-up re-read.
    socket = assign(socket, inspect_frozen: false, refresh_pending: false)

    case head_term(socket) do
      {:beamtalk_object, _c, _m, pid} = term when is_pid(pid) ->
        socket
        |> rearm_watch(term)
        |> refresh_inspector(term)

      _ ->
        socket
    end
  end

  defp toggle_freeze(socket) do
    # Freeze: drop the subscription, keep the current rows/stats as the snapshot.
    socket
    |> unwatch()
    |> assign(inspect_frozen: true)
  end

  # Subscribe the current head object for change pushes without touching the rows
  # (used by unfreeze, which re-reads separately). A non-:ok result leaves the
  # watch nil rather than claiming a live subscription.
  defp rearm_watch(socket, term) do
    case Facade.dispatch(:subscribe_object, %{term: term, pid: self()}, ctx(socket)) do
      :ok -> assign(socket, inspect_watch: term)
      _ -> assign(socket, inspect_watch: nil)
    end
  end

  # The live term at the current inspector head: the last drill crumb carries it.
  # nil when nothing is inspected.
  defp head_term(socket) do
    case List.last(socket.assigns.inspect_crumbs) do
      %{term: term} -> term
      _ -> nil
    end
  end

  # Send `message` to the inspected actor by eval'ing `<binding> <message>` against
  # the session (the spike's poke). The actor is addressed by its binding name —
  # the head crumb's label — so a poke only makes sense when the head IS a named
  # binding (not a drilled field or the `→ result` of an inspectIt). A successful
  # send renders a terse confirmation and lets the change stream flash the updated
  # field; a failure renders the structured error.
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

    case Facade.dispatch(:eval, %{session_pid: pid, code: code}, ctx(socket)) do
      {:ok, term, _output, _warnings} ->
        socket
        |> assign(poke_result: "→ #{Workspace.render_term(term)}", poke_error: nil)
        # A poke is a user-initiated mutation — re-read the inspected object now so
        # its fields reflect the write immediately, rather than waiting on the async
        # `{:object_changed, …}` change-stream push (which is coalesced/delayed, and
        # is the live-tracking path tracked separately by BT-2524). No-op when the
        # pane is frozen or nothing is watched.
        |> refresh_poked_inspector()

      {:error, reason, _output, _warnings} ->
        assign(socket, poke_result: nil, poke_error: Workspace.render_error(reason))

      {:error, reason} ->
        assign(socket, poke_result: nil, poke_error: facade_error(reason))
    end
  end

  # Re-read the inspected object after a successful poke so the pane reflects the
  # mutation synchronously. Only when an object is actively watched (a live,
  # non-frozen pid-backed head) — a frozen pane holds its snapshot, and a
  # non-object head has nothing to re-read.
  defp refresh_poked_inspector(%{assigns: %{inspect_watch: term}} = socket)
       when not is_nil(term) do
    refresh_inspector(socket, term)
  end

  defp refresh_poked_inspector(socket), do: socket

  # The binding name to address a poke to. A poke eval's `<receiver> <message>`
  # against the session, so the receiver must be a *source-addressable* name — a
  # session binding. That holds only at the inspection root (a single crumb whose
  # label IS the binding the inspection started from): once you drill into a field
  # the head is a referenced object with no session binding to name, and an
  # inspectIt `→ result` has no name at all. So poke is offered only when there's a
  # single crumb with a valid-identifier label; otherwise nil (the bar reports it
  # can't send, and the markup can hide it). This keeps the eval well-formed and
  # honest rather than sending to a name that isn't bound.
  defp poke_target_label(socket), do: poke_label(socket.assigns)

  # Whether the current Inspector head can be poked: a pid-backed object at the
  # inspection root with a valid-identifier binding name. Takes the bare `assigns`
  # so the render template can gate the poke bar with it directly.
  defp pokeable?(assigns), do: poke_label(assigns) != nil

  # The session-binding name to address a poke to, or nil (see `poke_target_label`).
  defp poke_label(assigns) do
    case assigns[:inspect_crumbs] do
      [%{label: label}] when is_binary(label) ->
        if valid_receiver?(label), do: label, else: nil

      _ ->
        nil
    end
  end

  # A poke receiver must be a plain lowercase Beamtalk identifier (a binding name)
  # — not the `→ result` synthetic label or anything with spaces/punctuation that
  # would make `<label> <message>` ill-formed source. This is a well-formedness
  # gate, not an injection guard: the poke eval is RBAC-gated (owner-only) and the
  # label comes from the crumb the *server* set from the inspected binding name,
  # never raw browser input. A pseudo-keyword binding (`self`/`nil`/…) that slips
  # through just produces a normal eval that DNUs or no-ops — no escalation.
  defp valid_receiver?(label), do: Regex.match?(~r/\A[a-z_][A-Za-z0-9_]*\z/, label)

  # Build the Inspector head's target descriptor: the binding/field label, the
  # live printString header, and the class/pid type chips. For a live actor the
  # term is `{:beamtalk_object, class, _module, pid}` (over distribution), so the
  # class atom and pid render straight into the spike's `proc-chips`. Non-object
  # values carry no pid and report their scalar kind as the class chip.
  defp target_info(label, {:beamtalk_object, class, _module, pid} = term) when is_pid(pid) do
    %{
      label: to_string(label),
      header: Workspace.render_term(term),
      class_name: to_string(class),
      pid: inspect(pid)
    }
  end

  # Supervisor handles are pid-backed live refs too: chip the class + pid the same
  # way as objects so the head renders as a drillable target (content/children is
  # a follow-up — see `Workspace.inspect_value/1`).
  defp target_info(label, {:beamtalk_supervisor, class, _module, pid} = term) when is_pid(pid) do
    %{
      label: to_string(label),
      header: Workspace.render_term(term),
      class_name: to_string(class),
      pid: inspect(pid)
    }
  end

  defp target_info(label, term) do
    %{
      label: to_string(label),
      header: Workspace.render_term(term),
      class_name: scalar_kind(term),
      pid: nil
    }
  end

  # Turn an inspect fields map (live terms) into ordered display rows. Each row
  # keeps the live field `term` so a drill on an object-valued slot follows the
  # real reference; `drillable` marks the object-valued ones.
  defp field_rows(fields) do
    fields
    |> Enum.map(fn {key, term} ->
      %{
        name: to_string(key),
        value: Workspace.render_term(term),
        term: term,
        drillable: Workspace.inspectable?(term),
        kind: term_kind(term)
      }
    end)
    |> Enum.sort_by(& &1.name)
  end

  # BT-2634: turn the runtime's supervisor-child maps (binary-keyed, from
  # `beamtalk_process_navigation:child_handles/1`) into Inspector display rows. A
  # child with a live `handle` (a Beamtalk actor / supervisor child) is drillable —
  # its `term` is the live `{:beamtalk_supervisor, …}` / `{:beamtalk_object, …}`
  # reference the "drill" event follows (ADR 0095). A foreign / restarting child
  # has `handle => :null`: it renders (so the tree is complete) but is not
  # drillable. The value column shows kind + pid + child count so the supervision
  # structure is legible at a glance.
  defp supervisor_child_rows(child_rows) when is_list(child_rows) do
    Enum.map(child_rows, &supervisor_child_row/1)
  end

  defp supervisor_child_row(row) when is_map(row) do
    handle = Map.get(row, "handle", :null)
    drillable = handle != :null and handle != nil

    %{
      name: to_string(Map.get(row, "label", "child")),
      value: supervisor_child_value(row),
      term: if(drillable, do: handle, else: nil),
      drillable: drillable,
      kind: "ref"
    }
  end

  # The value-column text for a supervisor child row: "kind · pid · N children"
  # (child count only for supervisors), or "kind · restarting" for a child caught
  # mid-restart (no pid).
  defp supervisor_child_value(row) do
    kind = to_string(Map.get(row, "kind", "process"))
    pid = supervisor_child_pid_text(Map.get(row, "pid", :null))

    if Map.get(row, "isSupervisor", false) do
      count = Map.get(row, "childCount", 0)
      "#{kind} · #{pid} · #{count} #{pluralize_children(count)}"
    else
      "#{kind} · #{pid}"
    end
  end

  defp supervisor_child_pid_text(:null), do: "restarting"
  defp supervisor_child_pid_text(nil), do: "restarting"
  defp supervisor_child_pid_text(pid) when is_binary(pid), do: pid
  defp supervisor_child_pid_text(pid), do: to_string(pid)

  defp pluralize_children(1), do: "child"
  defp pluralize_children(_), do: "children"

  # Map a live term to the Inspector's "no fields to inspect" type word ("X is a
  # <scalar_kind>"). Derives from the single classifier `Workspace.term_class/1`
  # (BT-2635), so this is purely a presentation mapping of its `{:scalar, kind}`
  # output — it does not re-enumerate term shapes. `{:ref, _}` terms never reach
  # here from the non-inspectable else-branch; they can reach it from
  # `target_info/2`'s fallback, where they fall through to "value". The `:value`
  # / unmapped-scalar cases all render "value".
  defp scalar_kind(term) do
    case Workspace.term_class(term) do
      {:scalar, :integer} -> "number"
      {:scalar, :float} -> "number"
      {:scalar, :string} -> "string"
      {:scalar, :boolean} -> "boolean"
      {:scalar, :list} -> "collection"
      {:scalar, :map} -> "map"
      _ -> "value"
    end
  end

  # Map a live term to the spike Inspector's value-kind class (inspector.jsx
  # `valueClass`), driving the type-chip / value colour. Derives from the single
  # classifier `Workspace.term_class/1` (BT-2635): any live ref (object,
  # supervisor, future, pid) chips as the drillable `ref`; scalars map to their
  # CSS class. Booleans classify as `{:scalar, :boolean}` (matched before the
  # atom guard inside `term_class/1`), so they chip as "bool", not "symbol".
  # Lists/maps and any unrecognised term fall through to "value", exactly as
  # before centralisation.
  defp term_kind(term) do
    case Workspace.term_class(term) do
      {:ref, _kind} -> "ref"
      {:scalar, :boolean} -> "bool"
      {:scalar, :integer} -> "int"
      {:scalar, :float} -> "int"
      {:scalar, :string} -> "string"
      {:scalar, :atom} -> "symbol"
      _ -> "value"
    end
  end

  # ── pid-stats chip accessors (BT-2492) ──────────────────────────────────────
  #
  # The `pid_stats` op returns a binary-keyed map (`beamtalk_repl_ops_watch`):
  # `status`, `queue_depth`, `memory_bytes`, `reductions`, `current_function`, plus
  # `alive`. These read the head chips off that snapshot, returning nil when the
  # stat is absent (so the chip's `:if` hides it) — a dead pid reports only
  # `status: "dead"`, so the mailbox/reductions chips vanish rather than show 0.

  # Process scheduling status (`running`/`waiting`/`runnable`/`dead`/…), always
  # shown when stats are present — it's the live "is it ticking" tell.
  defp stat_status(stats) when is_map(stats), do: Map.get(stats, "status")
  defp stat_status(_), do: nil

  # Mailbox (message queue) depth — only when the pid is alive (a dead pid has no
  # queue, and the map omits it).
  defp stat_mailbox(stats) when is_map(stats), do: Map.get(stats, "queue_depth")
  defp stat_mailbox(_), do: nil

  # Reduction count (a coarse "how much work has it done" gauge), thousands-
  # separated for readability like the spike. Absent on a dead pid.
  defp stat_reductions(stats) when is_map(stats) do
    case Map.get(stats, "reductions") do
      n when is_integer(n) -> format_thousands(n)
      _ -> nil
    end
  end

  defp stat_reductions(_), do: nil

  # Group an integer with thousands separators (the spike's `toLocaleString()`),
  # e.g. 1234567 → "1,234,567".
  defp format_thousands(n) when is_integer(n) and n < 0, do: "-" <> format_thousands(-n)

  defp format_thousands(n) when is_integer(n) do
    n
    |> Integer.to_string()
    |> String.graphemes()
    |> Enum.reverse()
    |> Enum.chunk_every(3)
    |> Enum.map_join(",", &Enum.join/1)
    |> String.reverse()
  end

  # ── Tweaks panel (BT-2487) ──────────────────────────────────────────────────

  # The default appearance the cockpit ships with, mirroring the spike's
  # `useTweaks` initial state (spikes/cockpit-ux-spike/app.jsx). The `TweaksPanel`
  # JS hook reads these from `data-tweaks-defaults`, then a per-key localStorage
  # override (the user's last choice) wins — so this is just the first-run skin.
  @tweak_defaults %{
    theme: "paper",
    accent: "#b9711b",
    syntax: "warm",
    density: "cozy",
    uiFont: "Hanken Grotesk",
    codeFont: "Cascadia Code"
  }

  # The curated accent swatches (paper/squeak only — dusk keeps its built-in
  # accent), UI-font and code-font options — exactly the sets the spike offers.
  @tweak_accents ~w(#b9711b #a8324e #2c6e8e #5d7a2e #7a4ea8)
  @tweak_ui_fonts ["Hanken Grotesk", "Inter Tight", "Public Sans", "Schibsted Grotesk"]
  @tweak_code_fonts ["Cascadia Code", "Monaspace", "JetBrains Mono"]

  # The appearance panel: a pure-client control surface. The `TweaksPanel` hook
  # owns all behaviour — each control declares the tweak it drives via
  # `data-tweak` / `data-tweak-value`, and the hook flips the matching `:root`
  # CSS variable + persists to localStorage. The server never sees a change; this
  # is presentation only, so it carries no socket state beyond the static
  # defaults the hook restores on first run.
  defp tweaks_panel(assigns) do
    assigns =
      assigns
      |> assign(:defaults, @tweak_defaults)
      |> assign(:accents, @tweak_accents)
      |> assign(:ui_fonts, @tweak_ui_fonts)
      |> assign(:code_fonts, @tweak_code_fonts)

    ~H"""
    <div
      id="tweaks-panel"
      class="panel tweaks-panel"
      style="flex:none;"
      phx-hook="TweaksPanel"
      data-tweaks-defaults={Jason.encode!(@defaults)}
    >
      <div class="panel-head">Tweaks</div>
      <div class="panel-body">
        <%!-- Theme → data-theme on <html> (whole palette swap) --%>
        <div class="twk-row">
          <span class="twk-cap">Theme</span>
          <div class="twk-seg" role="radiogroup" aria-label="Theme">
            <button
              :for={theme <- ~w(paper squeak dusk)}
              type="button"
              role="radio"
              data-tweak="theme"
              data-tweak-value={theme}
            >
              {theme}
            </button>
          </div>
        </div>

        <%!-- Accent → --accent / --accent-2 (paper/squeak; dusk keeps its own) --%>
        <div class="twk-row">
          <span class="twk-cap">Accent</span>
          <div class="twk-swatches" role="radiogroup" aria-label="Accent colour">
            <button
              :for={hex <- @accents}
              type="button"
              role="radio"
              class="twk-swatch"
              style={"background: #{hex};"}
              title={hex}
              aria-label={hex}
              data-tweak="accent"
              data-tweak-value={hex}
            >
            </button>
          </div>
          <span class="twk-accent-note">Dusk uses its built-in accent</span>
        </div>

        <%!-- Syntax → the --t-* token palette (warm/mono/vivid) --%>
        <div class="twk-row">
          <span class="twk-cap">Syntax palette</span>
          <div class="twk-seg" role="radiogroup" aria-label="Syntax palette">
            <button
              :for={mode <- ~w(warm mono vivid)}
              type="button"
              role="radio"
              data-tweak="syntax"
              data-tweak-value={mode}
            >
              {mode}
            </button>
          </div>
        </div>

        <%!-- Density → data-density (--row-h / --pad / --gap) --%>
        <div class="twk-row">
          <span class="twk-cap">Density</span>
          <div class="twk-seg" role="radiogroup" aria-label="Density">
            <button
              :for={d <- ~w(cozy compact)}
              type="button"
              role="radio"
              data-tweak="density"
              data-tweak-value={d}
            >
              {d}
            </button>
          </div>
        </div>

        <%!-- UI font → --ui-font (the shell typeface) --%>
        <div class="twk-row">
          <span class="twk-cap">UI font</span>
          <select class="twk-select" data-tweak="uiFont" aria-label="UI font">
            <option :for={font <- @ui_fonts} value={font}>{font}</option>
          </select>
        </div>

        <%!-- Code font → --code-font (the editor / mono typeface) --%>
        <div class="twk-row">
          <span class="twk-cap">Code font</span>
          <select class="twk-select mono" data-tweak="codeFont" aria-label="Code font">
            <option :for={font <- @code_fonts} value={font}>{font}</option>
          </select>
        </div>

        <p class="twk-note">Appearance only — saved to this browser.</p>
      </div>
    </div>
    """
  end

  # ── System Browser panes (BT-2491) ──────────────────────────────────────────

  # The class tree pane: a Hierarchy / Category view toggle in the head, the
  # class rows in the body, and the instance/class side toggle in the footer
  # (the spike's ClassBrowser). Selecting a class fires `browser_select_class`;
  # the view + side toggles are `browser_view` / `browser_side`. Runtime-only
  # (image-diverged) classes carry a `runtime` badge.
  attr :browser_view, :string, required: true
  attr :browser_side, :string, required: true
  attr :browser_classes, :list, required: true
  # BT-2557: source-origin filter ("all" | "project" | "deps" | "stdlib").
  attr :browser_source, :string, default: "all"
  attr :selected_class, :string, default: nil
  attr :browser_error, :string, default: nil
  # Owner-only "New Class" affordance (BT-2293, BT-2645): `role` gates the ＋
  # button (only the owner can `newClass:at:`); `new_class_open` is the open/closed
  # state of the create-a-class modal. `new_class_name` / `new_class_super` retain
  # the in-flight field values; `new_class_error` is the in-modal validation /
  # create error. All default so read-only callers can omit.
  attr :role, :atom, default: :observer
  attr :new_class_open, :boolean, default: false
  attr :new_class_name, :string, default: ""
  attr :new_class_super, :string, default: "Object"
  attr :new_class_error, :string, default: nil
  # BT-2656: the two-mode panel state. `browser_mode` (:classes | :native) selects
  # whether the body shows the class tree or the separate Native browser;
  # `browser_native_modules` are the loaded packages' hand-written native Erlang
  # modules (no class to back them); `native_source` is the Native browser's own
  # source-origin filter; `native_module_shown` highlights an open module in the list.
  attr :browser_mode, :atom, default: :classes
  attr :browser_native_modules, :list, default: []
  attr :native_source, :string, default: "all"
  attr :native_module_shown, :string, default: nil

  defp system_browser_classes(assigns) do
    # BT-2557: filter the rows once, up front, so both the hierarchy and category
    # views (and the empty-state check) render the same source-scoped set.
    # BT-2656: the Native browser gets its own origin-scoped set (`visible_modules`).
    assigns =
      assigns
      |> assign(
        :visible_classes,
        filter_by_source(assigns.browser_classes, assigns.browser_source)
      )
      |> assign(
        :visible_modules,
        filter_by_source(assigns.browser_native_modules, assigns.native_source)
      )

    ~H"""
    <div id="system-browser" class="panel">
      <div class="panel-head">
        System Browser <span class="spacer"></span>
        <%!-- BT-2656: Classes | Native panel-mode toggle. Native (Erlang) modules
             are a distinct namespace from Beamtalk classes, so they live in their
             own scrollable, filterable browser rather than a collapsed in-tree
             section. Selecting "Native" replaces the class tree body with the
             native-module list (its own origin filter + count). --%>
        <div class="seg" role="tablist" aria-label="Browser mode">
          <button
            :for={{mode, label} <- [{"classes", "Classes"}, {"native", "Native"}]}
            type="button"
            role="tab"
            class={[to_string(@browser_mode) == mode && "on"]}
            aria-selected={to_string(to_string(@browser_mode) == mode)}
            phx-click="browser_mode"
            phx-value-mode={mode}
          >
            {label}
          </button>
        </div>
        <div :if={@browser_mode == :classes} class="seg" role="tablist" aria-label="Class tree view">
          <button
            :for={{view, label} <- [{"hierarchy", "Hier"}, {"category", "Cats"}]}
            type="button"
            role="tab"
            class={[@browser_view == view && "on"]}
            aria-selected={to_string(@browser_view == view)}
            phx-click="browser_view"
            phx-value-view={view}
          >
            {label}
          </button>
        </div>
        <%!-- BT-2557: source-origin filter — narrow the tree to project / deps /
             stdlib so a project's own classes aren't buried under the stdlib.
             BT-2603: a compact <select> rather than a segmented control so it
             keeps a fixed, small width and never overflows / clips off the side
             of a narrow panel head. The posted field is `src`, so the existing
             `handle_event("browser_source", %{"src" => src}, ...)` is unchanged;
             `phx-change` fires on each selection and the native <select> carries
             its own keyboard + listbox aria semantics. --%>
        <%!-- `onsubmit="return false"`: this form only carries `phx-change`; there
             is deliberately no submit path (selection drives the filter). The
             guard makes a stray native submit — `form.submit()`, or a future
             field added here — a no-op rather than an unhandled LiveView event. --%>
        <form
          :if={@browser_mode == :classes}
          phx-change="browser_source"
          onsubmit="return false"
          class="src-filter"
        >
          <select name="src" class="src-select" aria-label="Class source filter">
            <option
              :for={
                {src, label} <- [
                  {"all", "All"},
                  {"project", "Proj"},
                  {"deps", "Deps"},
                  {"stdlib", "Std"}
                ]
              }
              value={src}
              selected={@browser_source == src}
            >
              {label}
            </option>
          </select>
        </form>
        <%!-- BT-2656/BT-2661: the Native browser's own source-origin filter, mirroring
             the class tree's. Posts the `src` field to `native_source`; defaults to
             Project (with an All fallback when no native module is project-origin). --%>
        <form
          :if={@browser_mode == :native}
          phx-change="native_source"
          onsubmit="return false"
          class="src-filter"
        >
          <select name="src" class="src-select" aria-label="Native source filter">
            <option
              :for={
                {src, label} <- [
                  {"all", "All"},
                  {"project", "Proj"},
                  {"deps", "Deps"},
                  {"stdlib", "Std"}
                ]
              }
              value={src}
              selected={@native_source == src}
            >
              {label}
            </option>
          </select>
        </form>
        <%!-- New Class (BT-2293): owner-only ＋ toggle. Reveals the inline create
             form below; the new class then appears right in the tree under it. --%>
        <button
          :if={@role == :owner and @browser_mode == :classes}
          type="button"
          class={["panel-icon", @new_class_open && "on"]}
          phx-click="toggle_new_class"
          aria-haspopup="dialog"
          aria-expanded={to_string(@new_class_open)}
          aria-controls={if @new_class_open, do: "new-class-modal"}
          aria-label="New class"
          title="New class"
        >
          ＋
        </button>
        <button
          type="button"
          class="panel-close"
          phx-click="close_browser"
          aria-label="Close System Browser panel"
          title="Close panel"
        >
          ×
        </button>
      </div>
      <%!-- NEW CLASS modal (BT-2293, BT-2645, ADR 0082 Phase 5): create a
           brand-new class from two explicit fields — a plain PascalCase name and a
           superclass (default `Object`, a datalist typeahead over existing
           classes). The `<Superclass> subclass: <Name>` definition + `.bt` path
           are synthesized server-side, so the owner thinks in classes, not files
           or `subclass:` syntax. Validation errors render inline inside the modal
           (never the method editor). The new-class entry appears in the Changes
           pane and is written to disk on the next flush; the created class opens +
           is selected on success. --%>
      <div
        :if={@role == :owner and @new_class_open}
        id="new-class-modal"
        class="modal-scrim"
        phx-window-keydown="close_new_class"
        phx-key="escape"
      >
        <div
          class="modal-dialog"
          role="dialog"
          aria-modal="true"
          aria-label="New class"
          phx-click-away="close_new_class"
        >
          <div class="modal-head">
            <h2 class="modal-title">New Class</h2>
            <button
              type="button"
              class="panel-close"
              phx-click="close_new_class"
              aria-label="Close New Class dialog"
              title="Close"
            >
              ×
            </button>
          </div>
          <form id="new-class-form" phx-submit="new_class" class="new-class-modal-form">
            <label class="new-class-field-label" for="new-class-name">Class name</label>
            <input
              type="text"
              id="new-class-name"
              name="name"
              class="field"
              value={@new_class_name}
              autocomplete="off"
              spellcheck="false"
              placeholder="Greeter"
              aria-describedby={if @new_class_error, do: "new-class-error"}
              aria-invalid={to_string(@new_class_error != nil)}
              phx-mounted={Phoenix.LiveView.JS.focus()}
            />
            <label class="new-class-field-label" for="new-class-super">Superclass</label>
            <input
              type="text"
              id="new-class-super"
              name="superclass"
              class="field"
              value={@new_class_super}
              list="new-class-super-options"
              autocomplete="off"
              spellcheck="false"
              placeholder="Object"
            />
            <datalist id="new-class-super-options">
              <option :for={row <- @browser_classes} value={Map.get(row, "name")}></option>
            </datalist>
            <p :if={@new_class_error} id="new-class-error" class="new-class-error" role="alert">
              {@new_class_error}
            </p>
            <div class="modal-actions">
              <button type="button" class="btn ghost" phx-click="close_new_class">
                Cancel
              </button>
              <button class="btn primary" type="submit" phx-disable-with="Creating…">
                Create
              </button>
            </div>
          </form>
        </div>
      </div>
      <%!-- BT-2656: the class tree body — shown only in `:classes` mode. --%>
      <div
        :if={@browser_mode == :classes}
        class="panel-body"
        id="system-browser-tree"
        phx-hook="ScrollToSelected"
      >
        <.notice
          :if={@browser_error}
          variant={:err}
          message={@browser_error}
          dismiss_attrs={%{"phx-click" => "dismiss_notice", "phx-value-key" => "browser_error"}}
        />
        <%= cond do %>
          <% @browser_classes == [] -> %>
            <p :if={!@browser_error} class="muted-note">No classes in the image yet.</p>
          <% @visible_classes == [] -> %>
            <p class="muted-note">No classes match this source filter.</p>
          <% true -> %>
            <div class="tree">
              <%= if @browser_view == "category" do %>
                <div :for={{category, rows} <- category_groups(@visible_classes)} class="cat-group">
                  <div class="cat-row">{category}</div>
                  <.class_rows
                    rows={Enum.map(rows, &{&1, 1, false})}
                    selected_class={@selected_class}
                    browser_side={@browser_side}
                  />
                </div>
              <% else %>
                <%!-- BT-2649: build the Hierarchy tree from the *full* class set so
                     the superclass ancestors connecting filtered matches up to a
                     root survive as dimmed, non-interactive context rows. Under
                     `all`, every class is visible, so no context rows are emitted. --%>
                <.class_rows
                  rows={
                    hierarchy_rows_with_context(
                      @browser_classes,
                      MapSet.new(@visible_classes, &Map.get(&1, "name"))
                    )
                  }
                  selected_class={@selected_class}
                  browser_side={@browser_side}
                />
              <% end %>
            </div>
        <% end %>
      </div>
      <%!-- BT-2656: the separate Native browser body — shown only in `:native` mode.
           A loaded package's hand-written native Erlang modules (no `native:` class
           backs them — the beamtalk-http case) get their own scrollable, filterable
           list (fixing BT-2660's overflow by construction). A click on a module
           opens its `.erl` read-only as an editor tab (`browser_open_native_module`
           → `open_native_module_tab/2`). Each row carries the same package/origin
           badge vocabulary the class tree uses (DEP · <pkg> / STDLIB / project). --%>
      <div :if={@browser_mode == :native} class="panel-body" id="native-browser">
        <%= cond do %>
          <% @browser_native_modules == [] -> %>
            <p class="muted-note">No native modules in the workspace.</p>
          <% @visible_modules == [] -> %>
            <p class="muted-note">No native modules match this source filter.</p>
          <% true -> %>
            <div class="tree native-modules-list">
              <div
                :for={mod <- @visible_modules}
                class={["row", @native_module_shown == mod["module"] && "sel"]}
                phx-click="browser_open_native_module"
                phx-value-module={mod["module"]}
                title={mod["module"]}
              >
                <span class="twig">●</span>
                <span class="cls mono">{mod["module"]}</span>
                <span
                  :if={mod["source_origin"] && mod["source_origin"] != "project"}
                  class={"source-origin-tag #{source_origin_class(mod)}"}
                  title={source_origin_title(mod)}
                >
                  {source_origin_label(mod)}
                </span>
                <span
                  :if={mod["openable"] == false}
                  class="runtime-tag"
                  title="no source on disk (.beam-only)"
                >
                  ⚡
                </span>
              </div>
            </div>
        <% end %>
      </div>
      <%!-- BT-2656: the instance/class side toggle drives the class tree's method
           list, so it is only meaningful in `:classes` mode. --%>
      <div :if={@browser_mode == :classes} class="actionbar sb-side">
        <div class="seg" role="tablist" aria-label="Instance / class side">
          <button
            :for={side <- ~w(instance class)}
            type="button"
            role="tab"
            class={[@browser_side == side && "on"]}
            aria-selected={to_string(@browser_side == side)}
            phx-click="browser_side"
            phx-value-side={side}
          >
            {side}
          </button>
        </div>
      </div>
      <%!-- BT-2656: a count footer for the Native browser, mirroring the class
           tree's badge vocabulary. --%>
      <div :if={@browser_mode == :native} class="actionbar sb-side native-count">
        <span class="native-badge">Erlang</span>
        <span>Native modules</span>
        <span class="count">{length(@visible_modules)}</span>
      </div>
    </div>
    """
  end

  # Render a list of `{class_row, indent, context?}` tuples — shared by the
  # Hierarchy and Category views. The selected class is highlighted; an indented
  # row reads as a subclass. A runtime-only class is badged; the class-side
  # selection shows a `class` pill so the side is visible in the tree. A
  # `context? = true` row (BT-2649) is a filter's connecting superclass ancestor:
  # dimmed and non-interactive, with no selection/badges.
  #
  # Public (not `defp`) so the dimmed/non-interactive context rendering is
  # unit-testable via `render_component/2` in the non-workspace lane (BT-2649).
  attr :rows, :list, required: true
  attr :selected_class, :string, default: nil
  attr :browser_side, :string, required: true

  def class_rows(assigns) do
    ~H"""
    <%!-- BT-2649: a `context?` row is a superclass ancestor surfaced only to keep
         the filtered Hierarchy spine intact. It renders dimmed (`context` class),
         carries no `phx-click`/selection and no origin/runtime badges, and is
         skipped as a tab stop — a clearly secondary, non-interactive marker. --%>
    <div
      :for={{class, indent, context?} <- @rows}
      class={[
        "row",
        indent > 0 && "subclass",
        context? && "context",
        not context? && @selected_class == class["name"] && "sel"
      ]}
      style={class_row_indent(indent)}
      phx-click={not context? && "browser_select_class"}
      phx-value-class={not context? && class["name"]}
      aria-disabled={context? && "true"}
      tabindex={context? && "-1"}
      title={class["name"]}
    >
      <span class="twig">{if class["superclass"], do: "→", else: "●"}</span>
      <span class="cls">{class["name"]}</span>
      <span
        :if={not context? && class["source_origin"] && class["source_origin"] != "project"}
        class={"source-origin-tag #{source_origin_class(class)}"}
        title={source_origin_title(class)}
      >
        {source_origin_label(class)}
      </span>
      <span
        :if={not context? && runtime_only?(class)}
        class="runtime-tag"
        title="runtime-only (not on disk)"
      >
        ⚡
      </span>
      <span
        :if={(not context? && @selected_class == class["name"]) and @browser_side == "class"}
        class="pill"
      >
        class
      </span>
    </div>
    """
  end

  # Hierarchy indent → inline `padding-left` (BT-2637). Depth is uncapped, so the
  # indent scales with the true superclass depth rather than collapsing every deep
  # class onto the spike's single `.subclass2` level (which read as flat). Each
  # level adds 14px on top of the row's base 10px — matching the old fixed steps
  # (24px at depth 1, 38px at depth 2) and continuing past them. Depth 0 (roots)
  # keeps the base padding, so no inline override is emitted.
  defp class_row_indent(indent) when is_integer(indent) and indent > 0,
    do: "padding-left: #{10 + indent * 14}px"

  defp class_row_indent(_), do: nil

  # BT-2578/BT-2648: the read-only native source-view body, shared by the
  # class-definition tab's native pane (keyed by a `native:` class's backing
  # module) and the standalone "Native modules" pane (keyed by a module). `view`
  # is the fetched native_view map (`error`/`content`/`source_file`/
  # `source_origin`/`editable`/`clauses`/`selected_clause`/`requested_selector`);
  # `fallback_module` names the module in the "source not available" empty state;
  # `dismiss_event` clears the in-pane error. `content == nil` degrades to the
  # empty state, never an error.
  attr :view, :map, required: true
  attr :fallback_module, :string, default: nil
  attr :dismiss_event, :string, required: true

  defp native_source_body(assigns) do
    ~H"""
    <.notice
      :if={@view.error}
      variant={:err}
      message={@view.error}
      dismiss_attrs={%{"phx-click" => @dismiss_event}}
    />
    <%= if @view.content do %>
      <div class="native-meta mono">
        <span :if={@view.source_file}>{@view.source_file}</span>
        <span class="native-origin">
          {@view.source_origin}{if @view.editable, do: " · editable", else: " · read-only"}
        </span>
      </div>
      <ul :if={@view.clauses != []} class="native-clauses">
        <li
          :for={c <- @view.clauses}
          class={
            "mono" <>
              if(clause_active?(c, @view.selected_clause),
                do: " native-clause-active",
                else: ""
              )
          }
          aria-current={clause_active?(c, @view.selected_clause) && "true"}
        >
          {c["selector"]}<span class="muted-note"> · line {c["line"]}</span>
        </li>
      </ul>
      <%!-- A delegate the backend could not map to a `handle_call` clause (it
           replies from `handle_info` / a helper): say so rather than silently
           highlighting nothing. Only on a method→clause jump (requested_selector). --%>
      <div :if={@view[:requested_selector] && is_nil(@view.selected_clause)} class="muted-note">
        No direct <code class="mono">handle_call</code>
        clause for <code class="mono">{@view.requested_selector}</code>
        — this delegate completes in <code class="mono">handle_info</code>
        or a helper.
      </div>
      <pre class="native-pre"><code>{@view.content}</code></pre>
    <% else %>
      <div :if={is_nil(@view.error)} class="muted-note">
        Erlang source not available — the module <code class="mono">{@fallback_module}</code>
        shipped without source.
      </div>
    <% end %>
    """
  end

  # The protocol + method pane (the spike's MethodList): a protocol filter row
  # ("all" + one row per protocol, BT-2491) over the method list for the current
  # filter. Selecting a method fires `browser_select_method`; runtime-only methods
  # are badged. Empty until a class is selected.
  attr :browser_protocols, :list, required: true
  attr :selected_protocol, :string, default: nil
  attr :selected_class, :string, default: nil
  attr :browser_side, :string, required: true
  # The method open in the focused editor tab (`%{class, side, selector}`) or nil
  # for a class-definition tab — drives the "sel" highlight so the browser tracks
  # whatever the editor is showing.
  attr :active_method, :map, default: nil
  # The class whose *definition* tab is focused (or nil) — highlights the "class
  # definition" entry when the editor is showing this class's definition.
  attr :active_def, :string, default: nil
  # The viewer's role — the "Add a method…" authoring entry is owner-only (Observers
  # get a read-only browser).
  attr :role, :atom, required: true

  defp system_browser_methods(assigns) do
    assigns =
      assigns
      |> assign(:methods, filtered_methods(assigns.browser_protocols, assigns.selected_protocol))
      |> assign(:total_methods, protocol_method_count(assigns.browser_protocols))

    ~H"""
    <div class="panel">
      <div class="panel-head">
        <%= if @selected_class do %>
          {if @browser_side == "class", do: @selected_class <> " class", else: @selected_class}
        <% else %>
          Protocols &amp; Methods
        <% end %>
        <span class="spacer"></span>
        <span :if={@selected_class} class="count">{@total_methods} methods</span>
      </div>
      <div class="panel-body" style="display:flex; flex-direction:column;">
        <%= if @selected_class == nil do %>
          <div class="empty">Select a class to browse its methods.</div>
        <% else %>
          <%!-- class definition entry: opens (or focuses) the class-definition
               tab so the class shape is browsable, not just its methods. Saving
               that tab compiles the class (ADR 0082). --%>
          <div class="tree sb-classdef">
            <div
              class={["row", @active_def == @selected_class && "sel"]}
              phx-click="browser_open_definition"
              phx-value-class={@selected_class}
            >
              <span class="twig" style="color: var(--accent);">▸</span>
              <span class="mname mono">class definition</span>
            </div>
            <%!-- "Add a method…" entry (owner-only): opens a blank :method tab for
                 the selected class so a brand-new method can be authored on demand —
                 the role the starter tab used to play before the editor opened
                 empty. --%>
            <div
              :if={@role == :owner}
              class="row"
              phx-click="new_method"
              phx-value-class={@selected_class}
            >
              <span class="twig" style="color: var(--accent);">+</span>
              <span class="mname mono">Add a method…</span>
            </div>
          </div>
          <%!-- protocol filter row: ∗ "all" + one row per protocol --%>
          <div class="tree sb-protocols">
            <div
              class={["row", @selected_protocol == nil && "sel"]}
              phx-click="browser_select_protocol"
              phx-value-protocol=""
            >
              <span class="twig">∗</span>
              <span>all</span>
              <span class="meta">{@total_methods}</span>
            </div>
            <div
              :for={proto <- @browser_protocols}
              class={["row", @selected_protocol == proto["name"] && "sel"]}
              phx-click="browser_select_protocol"
              phx-value-protocol={proto["name"]}
            >
              <span class="twig">·</span>
              <span>{proto["name"]}</span>
              <span class="meta">{length(proto["selectors"] || [])}</span>
            </div>
          </div>
          <%!-- method list for the active protocol filter --%>
          <div class="tree">
            <div :if={@methods == []} class="empty">No methods on the {@browser_side} side.</div>
            <div
              :for={m <- @methods}
              class={[
                "row method-row",
                @active_method && @active_method.class == @selected_class &&
                  @active_method.side == @browser_side &&
                  @active_method.selector == m["selector"] && "sel"
              ]}
              phx-click="browser_select_method"
              phx-value-class={@selected_class}
              phx-value-side={@browser_side}
              phx-value-selector={m["selector"]}
              title={m["selector"]}
            >
              <span class="twig" style="color: var(--accent);">m</span>
              <span class="mname mono">{m["selector"]}</span>
              <span
                :if={m["source_origin"] && m["source_origin"] != "project"}
                class={"source-origin-tag #{source_origin_class(m)}"}
                title={source_origin_title(m)}
              >
                {source_origin_label(m)}
              </span>
              <span :if={runtime_only?(m)} class="runtime-tag" title="runtime-only">⚡</span>
              <span
                :if={synthetic?(m)}
                class="derived-tag"
                title="compiler-derived (auto-generated synthetic method)"
              >
                derived
              </span>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  # Senders / Implementors result popover (BT-2495). Rendered as a child of the
  # `.nav-actions` group (which is `position: relative`) so it anchors to — and
  # pops up directly above — the buttons that opened it, rather than floating in
  # a fixed panel corner. Shared by the owner and observer button rows via a
  # function component (one source of truth). Closes on click-away, the × button,
  # or Escape (`phx-window-keydown` is live only while the popover is mounted).
  attr :nav, :map, default: nil

  defp nav_popover(assigns) do
    ~H"""
    <div
      :if={@nav}
      class="nav-popover"
      phx-click-away="nav_close"
      phx-window-keydown="nav_close"
      phx-key="Escape"
    >
      <div class="nav-pop-head">
        <b>{nav_kind_label(@nav.kind)}</b>
        <span class="mono">{@nav.selector}</span>
        <span class="spacer"></span>
        <button class="x" type="button" phx-click="nav_close" title="Close">×</button>
      </div>
      <.notice
        :if={@nav[:error]}
        variant={:err}
        message={@nav.error}
        dismiss_attrs={%{"phx-click" => "dismiss_nav_error"}}
      />
      <div :if={!@nav[:error] and @nav.sites == []} class="nav-empty">
        No {nav_kind_label(@nav.kind)} found.
      </div>
      <%!-- Conforming classes (BT-2639): each row is a class — clicking opens its
           definition pane (`nav_open_class`), not a method tab. --%>
      <button
        :for={site <- @nav.sites}
        :if={@nav.kind == :conforming_classes}
        type="button"
        class="nav-site"
        phx-click="nav_open_class"
        phx-value-class={site["class"]}
      >
        <span class="nav-site-name mono">{site["class"]}</span>
      </button>
      <%!-- Required methods (BT-2639): each row is a required selector — clicking
           opens its Implementors (`nav_required_open`, reusing the BT-2495 nav
           path). The protocol owner column is the popover head; rows show the
           bare selector (with a class-side tag where applicable). --%>
      <button
        :for={site <- @nav.sites}
        :if={@nav.kind == :required_methods}
        type="button"
        class="nav-site"
        phx-click="nav_required_open"
        phx-value-selector={site["method"]}
        phx-value-side={if site["class_side"] == true, do: "class", else: "instance"}
      >
        <span class="nav-site-name mono">
          {site["method"]}<span :if={site["class_side"] == true} class="nav-side-tag">class</span>
        </span>
      </button>
      <%!-- Senders / Implementors (BT-2495) and native-module Callers (BT-2669):
           each row is a (class, side, selector) call/definition site — clicking
           opens that method tab + navigates the browser tree (`nav_open`). --%>
      <button
        :for={site <- @nav.sites}
        :if={@nav.kind in [:senders, :implementors, :callers_of_native_module]}
        type="button"
        class="nav-site"
        phx-click="nav_open"
        phx-value-class={site["class"]}
        phx-value-side={if site["class_side"] == true, do: "class", else: "instance"}
        phx-value-selector={site["method"]}
      >
        <span class="nav-site-name mono">
          {site["class"]}<span :if={site["class_side"] == true} class="nav-side-tag">class</span> » {site[
            "method"
          ]}
        </span>
      </button>
    </div>
    """
  end

  # ── dismissable status notices (BT-2612) ────────────────────────────────────
  #
  # A single reusable notice component for every `io-block` status banner
  # (err/ok/warn/plain). Each notice carries a keyboard-accessible `×` dismiss
  # control. The dismiss is wired by the *caller* via `dismiss_attrs` — a map of
  # `phx-*` attributes (e.g. `%{"phx-click" => "dismiss_notice", "phx-value-key"
  # => "git_error"}`) so top-level scalar assigns route through the generic
  # `dismiss_notice` event while nested/per-window notices route through their own
  # targeted events. Variant maps to the `io-block` modifier class.
  attr :variant, :atom, default: :plain, values: [:err, :ok, :warn, :plain]
  attr :message, :string, required: true
  # Map of phx-* attributes rendered onto the dismiss button. The caller decides
  # which event/values clear the right backing assign.
  attr :dismiss_attrs, :map, required: true

  defp notice(assigns) do
    ~H"""
    <div class={["io-block", notice_variant_class(@variant)]}>
      <span class="io-block-msg">{@message}</span>
      <button
        type="button"
        class="io-block-dismiss"
        aria-label="Dismiss notification"
        title="Dismiss"
        {@dismiss_attrs}
      >
        ×
      </button>
    </div>
    """
  end

  defp notice_variant_class(:err), do: "err"
  defp notice_variant_class(:ok), do: "ok"
  defp notice_variant_class(:warn), do: "warn"
  defp notice_variant_class(:plain), do: nil

  @impl true
  def render(assigns) do
    ~H"""
    <div class="bt-cockpit">
      <div class="app">
        <%!-- ── top bar (46px): brand + Attach-topology widget ───────────── --%>
        <div class="topbar">
          <div class="brand">
            <span class="mark"><b>Beam</b>talk</span>
            <span class="ver">Cockpit</span>
          </div>
          <%!-- Omni search (BT-2495): a symbol search over classes + selectors
               (the `nav-symbols` index, ADR 0096 `:read` op). Filtering/ranking
               is server-side; the OmniSearch hook drives the arrow/enter/escape
               keyboard nav over the results popover (a connected-render JS
               behaviour the e2e lane covers). Shown only once attached, since the
               index is read live from the workspace. `phx-click-away` dismisses
               the popover when focus moves elsewhere. --%>
          <div :if={@connected} class="omni" phx-click-away="omni_close">
            <input
              id="omni-search"
              class="omni-input mono"
              type="text"
              name="q"
              value={@omni_query}
              placeholder="Search classes & selectors…"
              autocomplete="off"
              spellcheck="false"
              phx-hook="OmniSearch"
              phx-keyup="omni_search"
              phx-debounce="120"
            />
            <div :if={@omni_open and @omni_results != []} class="omni-results" role="listbox">
              <button
                :for={{r, idx} <- Enum.with_index(@omni_results)}
                type="button"
                role="option"
                class={["omni-row", idx == 0 && "active"]}
                aria-selected={to_string(idx == 0)}
                data-kind={r.kind}
                data-class={r.class}
                data-side={r.side}
                data-selector={r.selector}
              >
                <span class={"omni-kind #{r.kind}"}>{if r.kind == "class", do: "C", else: "ƒ"}</span>
                <span class="omni-label mono">{r.label}</span>
              </button>
            </div>
            <div
              :if={@omni_open and @omni_results == [] and String.trim(@omni_query) != ""}
              class="omni-results"
            >
              <div class="omni-empty">No matches</div>
            </div>
          </div>
          <span class="spacer"></span>
          <%!-- Panel toggle buttons (BT-2559): show/hide side panels and dock. --%>
          <div :if={@connected} class="panel-toggles">
            <button
              type="button"
              class={["panel-toggle", @show_browser && "on"]}
              phx-click="toggle_browser"
              title="Toggle System Browser"
            >
              Browser
            </button>
            <%!-- `show_inspector` gates the whole right column (Bindings + Inspector),
                 so the label names both rather than just "Inspector" (BT-2559 review). --%>
            <button
              type="button"
              class={["panel-toggle", @show_inspector && "on"]}
              phx-click="toggle_inspector"
              title="Toggle the Bindings + Inspector column"
            >
              Inspector &amp; Bindings
            </button>
            <button
              type="button"
              class={["panel-toggle", @show_dock && "on"]}
              phx-click="toggle_dock"
              title="Toggle Workspace Dock"
            >
              Dock
            </button>
          </div>
          <%!-- Dock/Float toggle (BT-2493, the spike's mode switch): in Float mode
               a binding click / Inspect-it opens a floating, draggable inspector
               window instead of the docked pane. Docked is the default. Shown only
               once attached, since it governs the connected Inspector. --%>
          <div :if={@connected} class="seg insp-mode" role="tablist" aria-label="Inspector mode">
            <button
              :for={{mode, label} <- [{"docked", "Dock"}, {"float", "Float"}]}
              type="button"
              role="tab"
              class={[@inspector_mode == mode && "on"]}
              aria-selected={to_string(@inspector_mode == mode)}
              phx-click="set_inspector_mode"
              phx-value-mode={mode}
            >
              {label}
            </button>
          </div>
          <%!-- Appearance settings (BT-2487): the Tweaks panel moved off the
               sidebar into a top-bar gear dropdown — it reads as settings, not a
               primary workspace pane. The panel stays mounted (so the TweaksPanel
               hook applies the saved theme on load); the gear only toggles the
               dropdown. Click-away / Escape close it. --%>
          <div
            :if={@connected}
            class="settings-menu"
            phx-click-away={if @show_settings, do: "close_settings"}
          >
            <button
              type="button"
              class="settings-gear"
              phx-click="toggle_settings"
              aria-haspopup="true"
              aria-expanded={to_string(@show_settings)}
              title="Appearance settings"
            >
              ⚙
            </button>
            <%!-- Escape listener: phx-window-keydown works even on display:none elements --%>
            <div
              :if={@show_settings}
              phx-window-keydown="close_settings"
              phx-key="Escape"
              style="display:none"
            >
            </div>
            <div class={["settings-popover", @show_settings && "open"]}>
              <.tweaks_panel />
            </div>
          </div>
          <%= if @connected do %>
            <div class="attach">
              <span class="dot live"></span>
              <span class="att-label">attached</span>
              <b class="att-node mono">{@node}</b>
              <span class="att-sep">·</span>
              <span class="att-sess mono">{@session_id}</span>
              <span class={"role-badge #{@role}"}>{@role}</span>
              <span :if={@role == :observer} class="att-sess">· read-only (Observer)</span>
            </div>
          <% else %>
            <div class="attach"><span class="att-sess">connecting…</span></div>
          <% end %>
        </div>

        <%= if @connected do %>
          <%!-- ── three-column cockpit grid (BT-2559: collapsible panels) --%>
          <div class={[
            "cockpit",
            !@show_browser && "browser-hidden",
            !@show_inspector && "inspector-hidden"
          ]}>
            <%!-- Column-width dividers (BT-2576): drag a seam to widen/narrow the
                 System Browser (left) or Inspector (right) column. Absolutely
                 positioned over the grid seams so the 3-column template and the
                 collapse rules stay intact; each hides when its column is
                 collapsed (`.cockpit.browser-hidden` / `.inspector-hidden`). --%>
            <div
              id="col-gutter-left"
              class="split-gutter split-gutter-x col-gutter-left"
              phx-hook="SplitDrag"
              phx-update="ignore"
              role="separator"
              aria-orientation="vertical"
              aria-label="Resize the System Browser column"
              data-split="browser-w"
              data-axis="x"
              data-edge="start"
              data-var="--browser-w"
              data-min="160"
              data-min-other="420"
            >
            </div>
            <div
              id="col-gutter-right"
              class="split-gutter split-gutter-x col-gutter-right"
              phx-hook="SplitDrag"
              phx-update="ignore"
              role="separator"
              aria-orientation="vertical"
              aria-label="Resize the Inspector column"
              data-split="inspector-w"
              data-axis="x"
              data-edge="end"
              data-var="--inspector-w"
              data-min="200"
              data-min-other="420"
            >
            </div>
            <%!-- LEFT — System Browser (BT-2491, 286px).
                 A class tree (Hierarchy / Category views, instance/class side
                 toggle) over a protocol-grouped method list, driven by the
                 BT-2488 browse ops (ADR 0096). The Tweaks panel that used to sit
                 below it now lives in the top-bar settings dropdown. --%>
            <div class="col" inert={!@show_browser}>
              <div class="browser-split">
                <.system_browser_classes
                  browser_view={@browser_view}
                  browser_source={@browser_source}
                  browser_side={@browser_side}
                  browser_classes={@browser_classes}
                  selected_class={@selected_class}
                  browser_error={@browser_error}
                  role={@role}
                  new_class_open={@new_class_open}
                  new_class_name={@new_class_name}
                  new_class_super={@new_class_super}
                  new_class_error={@new_class_error}
                  browser_mode={@browser_mode}
                  browser_native_modules={@browser_native_modules}
                  native_source={@native_source}
                  native_module_shown={active_native_module(assigns)}
                />
                <%!-- Draggable divider (BT-2576): rebalances the class tree vs.
                     the method list ("more class, less method"). phx-update="ignore"
                     (BT-2591): the gutter div is empty and hook-owned, so LiveView
                     should never patch it. The async mount load DOES strip the
                     hook-set --browser-split var off the PARENT .browser-split (it
                     re-renders the class tree inside it), but the SplitDrag hook's
                     own MutationObserver — not updated() — re-applies the saved size
                     when that happens. --%>
                <div
                  id="browser-split-gutter"
                  class="split-gutter split-gutter-y"
                  phx-hook="SplitDrag"
                  phx-update="ignore"
                  role="separator"
                  aria-orientation="horizontal"
                  aria-label="Resize the class tree and method list"
                  data-split="browser"
                  data-axis="y"
                  data-edge="start"
                  data-var="--browser-split"
                  data-min="80"
                  data-min-other="120"
                >
                </div>
                <.system_browser_methods
                  browser_protocols={@browser_protocols}
                  selected_protocol={@selected_protocol}
                  selected_class={@selected_class}
                  browser_side={@browser_side}
                  active_method={selected_method_ref(assigns)}
                  active_def={selected_def_ref(assigns)}
                  role={@role}
                />
              </div>
            </div>

            <%!-- CENTER — editor placeholder + workspace dock --%>
            <%!-- DOM order note: the Workspace dock (eval form) is emitted
                 BEFORE the Method Editor so the eval `<form>` is the first form
                 on the page — `form("form")` in the e2e tests resolves to it.
                 CSS `order` keeps the editor visually on top per the spike. --%>
            <div class="col">
              <%!-- workspace dock (BT-2490): tabbed Workspace / Transcript /
                   Changes. The three tab bodies are ALL rendered (toggled with
                   `hidden`, not removed) so the `#transcript` stream container is
                   always in the DOM for `stream_insert` regardless of the active
                   tab. --%>
              <div class={["dock", !@show_dock && "collapsed"]} style="order:3;" inert={!@show_dock}>
                <div id="workspace-dock" class="panel">
                  <div class="panel-head">
                    <span class="dock-tabs" role="tablist">
                      <button
                        :for={
                          {tab, label} <- [
                            {"workspace", "Workspace"},
                            {"repl", "REPL"},
                            {"transcript", "Transcript"},
                            {"changes", "Changes"},
                            {"git", "Git"},
                            {"tests", "Tests"}
                          ]
                        }
                        type="button"
                        role="tab"
                        class={["dock-tab", @dock_tab == tab && "on"]}
                        aria-selected={to_string(@dock_tab == tab)}
                        phx-click="dock_tab"
                        phx-value-tab={tab}
                      >
                        {label}<span :if={tab == "changes" and @changes != []} class="tab-count">{length(@changes)}</span>
                      </button>
                    </span>
                    <span class="spacer"></span>
                    <span :if={@dock_tab == "workspace"} class="count">
                      {if ws_selection?(assigns), do: "evaluates selection", else: "evaluates buffer"}
                    </span>
                    <button
                      type="button"
                      class="panel-close"
                      phx-click="toggle_dock"
                      aria-label="Collapse workspace dock"
                      title="Collapse dock"
                    >
                      ▾
                    </button>
                  </div>

                  <%!-- WORKSPACE tab: highlighted editor + doIt/printIt/inspectIt --%>
                  <div class="dock-pane ws-pane" hidden={@dock_tab != "workspace"}>
                    <%= if @role == :owner do %>
                      <%!-- eval form: the FIRST <form> on the page. The CmEditor
                           (CodeMirror, BT-2538) highlights the entered code; the
                           hidden textarea keeps name="expr" so the existing `eval`
                           handler (and the e2e `render_submit(%{expr: …})`) read
                           it. The three actions ride the SAME submit via the
                           hidden `action` field — Print it is the plain submit
                           (default), Do it / Inspect it set the field. ⌘D/⌘P/⌘I
                           do the same through the KeyboardShortcuts hook. --%>
                      <form
                        id="eval-form"
                        phx-submit="eval"
                        phx-hook="KeyboardShortcuts"
                        data-shortcuts={
                          Jason.encode!(%{
                            "mod+d" => "submit:do_it",
                            "mod+p" => "submit:print_it",
                            "mod+i" => "submit:inspect_it"
                          })
                        }
                        style="display:flex; flex-direction:column; height:100%;"
                      >
                        <input type="hidden" name="action" value="print_it" />
                        <%!-- CodeMirror 6 editor (BT-2538). The CmEditor hook
                             mounts CodeMirror into the ignored #workspace-editor-cm
                             host and mirrors its doc into the hidden textarea, which
                             stays the posted field (name="expr") so the `eval`
                             handler and render_submit(%{expr: …}) read it unchanged.
                             The textarea is phx-update="ignore" too: it's hook-owned,
                             so an unrelated re-render can't make morphdom revert it
                             to the last server @expr (which would wipe the editor and
                             submit a stale value). ⌘D/⌘P/⌘I ride the form's
                             KeyboardShortcuts hook (keydown bubbles out). --%>
                        <div
                          id={workspace_editor_id()}
                          class="cm-wrap ws-wrap"
                          phx-hook="CmEditor"
                          data-select-event="select_workspace"
                          data-inline-results="true"
                          data-autocomplete="true"
                        >
                          <textarea
                            id="workspace-editor-source"
                            class="cm-field"
                            name="expr"
                            spellcheck="false"
                            autocomplete="off"
                            phx-update="ignore"
                            hidden
                          ><%= @expr %></textarea>
                          <div class="cm-host" id="workspace-editor-cm" phx-update="ignore"></div>
                        </div>

                        <div class="actionbar">
                          <button class="btn" type="submit" name="action" value="do_it">
                            Do it <span class="k">⌘D</span>
                          </button>
                          <button class="btn primary" type="submit" name="action" value="print_it">
                            Print it <span class="k">⌘P</span>
                          </button>
                          <button class="btn" type="submit" name="action" value="inspect_it">
                            Inspect it <span class="k">⌘I</span>
                          </button>
                          <span class="spacer"></span>
                          <span class="kbdhint">select an expression, or evaluate all</span>
                        </div>
                      </form>
                    <% else %>
                      <p class="muted-note">
                        Your role is read-only — evaluation is disabled. You can still watch the
                        live Transcript and review pending Changes in the tabs above.
                      </p>
                    <% end %>

                    <%!-- Result / output / error render REGARDLESS of role: a
                         crafted eval from an Observer is refused by the facade and
                         its "Not authorized" message must still show (the form
                         itself is owner-gated away). --%>
                    <.notice
                      :if={@output}
                      variant={:plain}
                      message={@output}
                      dismiss_attrs={%{"phx-click" => "dismiss_notice", "phx-value-key" => "output"}}
                    />
                    <%!-- Print it / Do it / Inspect it confirmation (BT-2542): a
                         THIN, self-clearing status line — not the old growing
                         `.ws-result` bubble (which squeezed the editor). The full
                         result lands inline in the buffer (print_it) or the
                         Inspector (inspect_it); this is the momentary echo.

                         The `aria-live` region is a STABLE outer wrapper: screen
                         readers announce mutations WITHIN a persistent live region,
                         not a freshly-inserted one. The inner div is keyed by
                         `eval_seq` so it re-mounts each eval (restarting the fade);
                         its reappearance inside the stable region is the announced
                         change. --%>
                    <div id="eval-status" class="eval-status-region" aria-live="polite">
                      <div :if={@result} id={"eval-status-#{@eval_seq}"} class="eval-status">
                        <span class="val">{@result}</span>
                      </div>
                    </div>
                    <div :if={@error} class="ws-result err">
                      <span class="arrow">→</span>
                      <span class="val">{@error}</span>
                    </div>
                  </div>

                  <%!-- REPL tab (BT-2543): a classic TUI request→response
                       scrollback ABOVE a bottom-pinned input. The scrollback
                       stream is always in the DOM (like the transcript) so
                       stream_insert lands regardless of the active tab. The
                       input form is emitted AFTER the Workspace eval form, so
                       `form("#eval-form")` / `form("form")` still resolve to the
                       Workspace form the existing e2e tests submit. --%>
                  <div class="dock-pane repl-pane" hidden={@dock_tab != "repl"}>
                    <%!-- empty-state hint: shown until the first entry is
                         appended (`repl_seq` bumps per entry). Kept OUTSIDE the
                         stream container, which must hold only stream items. --%>
                    <p :if={@repl_seq == 0} class="muted-note repl-empty">
                      Evaluate an expression below — Enter runs it, ↑/↓ recall history.
                    </p>
                    <div id="repl-scrollback" class="repl-scrollback" phx-update="stream">
                      <div
                        :for={{dom_id, entry} <- @streams.repl}
                        id={dom_id}
                        class={[
                          "repl-entry",
                          entry.kind == :error && "err",
                          entry.kind == :info && "meta"
                        ]}
                      >
                        <div class="repl-req">
                          <span class="repl-mark">›</span>
                          <span class="repl-expr">{entry.request}</span>
                        </div>
                        <div class="repl-res">
                          <span class="repl-arrow">→</span>
                          <%= if entry.long do %>
                            <details class="repl-collapse">
                              <summary class="repl-summary">{repl_preview(entry.response)}</summary>
                              <span class="repl-val">{entry.response}</span>
                            </details>
                          <% else %>
                            <span class="repl-val">{entry.response}</span>
                          <% end %>
                          <button
                            :if={entry.inspectable}
                            type="button"
                            class="repl-inspect"
                            phx-click="repl_inspect"
                            phx-value-id={entry.id}
                            title="Inspect this result in the Inspector"
                          >
                            Inspect
                          </button>
                        </div>
                      </div>
                    </div>

                    <%= if @role == :owner do %>
                      <%!-- bottom-pinned composer: a thin, growing CodeMirror
                           input (the ReplInput hook — a CmEditor variant where
                           Enter submits and ↑/↓ recall history at the edges). The
                           hidden textarea keeps name="expr" so the `repl_eval`
                           handler and `render_submit(%{expr: …})` read it, exactly
                           like the Workspace eval form. --%>
                      <form id="repl-form" class="repl-input-form" phx-submit="repl_eval">
                        <div
                          id={repl_input_id()}
                          class="cm-wrap repl-wrap"
                          phx-hook="ReplInput"
                          data-placeholder="Evaluate an expression…"
                        >
                          <textarea
                            id="repl-input-source"
                            class="cm-field"
                            name="expr"
                            spellcheck="false"
                            autocomplete="off"
                            phx-update="ignore"
                            hidden
                          ></textarea>
                          <div class="cm-host" id="repl-input-cm" phx-update="ignore"></div>
                        </div>
                      </form>
                    <% else %>
                      <p class="muted-note">
                        Your role is read-only — REPL evaluation is disabled. You can still watch the
                        live Transcript and review pending Changes in the tabs above.
                      </p>
                    <% end %>
                  </div>

                  <%!-- TRANSCRIPT tab: the live stream (always in the DOM so
                       stream_insert lands regardless of the active tab). --%>
                  <div class="dock-pane" hidden={@dock_tab != "transcript"}>
                    <div id="transcript" class="transcript" phx-update="stream">
                      <div :for={{dom_id, line} <- @streams.transcript} id={dom_id}>{line.text}</div>
                    </div>
                  </div>

                  <%!-- CHANGES tab: the workspace ChangeLog (ADR 0082). --%>
                  <div class="dock-pane panel-body" hidden={@dock_tab != "changes"}>
                    <.notice
                      :if={@changes_error}
                      variant={:err}
                      message={@changes_error}
                      dismiss_attrs={
                        %{"phx-click" => "dismiss_notice", "phx-value-key" => "changes_error"}
                      }
                    />
                    <%= if @changes == [] do %>
                      <p class="muted-note">No pending changes. Save a method to record one.</p>
                    <% else %>
                      <table class="bt-table bt-changes-table">
                        <thead>
                          <tr>
                            <%!-- BT-2636: leading disclosure column (the diff
                                 caret), before Class. The old in-`Change`-column
                                 `diff` summary is gone — the caret now expands a
                                 structured, coloured diff beneath the row. --%>
                            <th class="diff-toggle-col"></th>
                            <th>Class</th>
                            <th>Selector</th>
                            <th>Intent</th>
                            <th>Flushable</th>
                            <th>Author</th>
                            <%!-- revert column (BT-2293): owner-only --%>
                            <th :if={@role == :owner}></th>
                          </tr>
                        </thead>
                        <tbody>
                          <%= for c <- @changes do %>
                            <% expanded = MapSet.member?(@expanded_changes, {c.class, c.selector}) %>
                            <tr>
                              <%!-- Leading disclosure caret (BT-2636): toggles
                                   this row's structured net-vs-disk diff. Only
                                   rendered when the entry carries a diff (the same
                                   defensive `:if={c[:diff]}` guard the old
                                   in-column disclosure used — a method reverted to
                                   its on-disk body has no net change and never
                                   reaches this pane). --%>
                              <td class="diff-toggle-col">
                                <button
                                  :if={c[:diff]}
                                  type="button"
                                  class="diff-toggle"
                                  phx-click="toggle_change_diff"
                                  phx-value-class={c.class}
                                  phx-value-selector={c.selector}
                                  aria-expanded={to_string(expanded)}
                                  title={if expanded, do: "Hide diff", else: "Show diff"}
                                >
                                  {if expanded, do: "▼", else: "›"}
                                </button>
                              </td>
                              <td class="k">{c.class}</td>
                              <td>{c.selector}</td>
                              <td>{c.intent}</td>
                              <td>{if c.flushable, do: "yes", else: "no"}</td>
                              <td>{c.author_kind}</td>
                              <%!-- Revert one pending change (ADR 0082 Phase 5,
                                   completeness BT-2663/BT-2664/BT-2665). Owner-only
                                   (`revert` is an :execute op). Instance-side and
                                   class-side method patches are revertable (a modify
                                   re-installs the prior body; an add removes the
                                   method), and a new-class entry is revertable (it
                                   removes the just-created class). Gate the button on
                                   a positive kind assertion so an unanticipated future
                                   kind hides the affordance rather than offering one
                                   that errors. New-class rows carry no selector, so
                                   send the `new-class` placeholder the workspace maps
                                   back to the class's new-class entry. --%>
                              <td :if={@role == :owner}>
                                <button
                                  :if={c.kind in ["instance", "class", "new-class"]}
                                  class="btn-link"
                                  type="button"
                                  phx-click="revert"
                                  phx-value-class={c.class}
                                  phx-value-selector={
                                    if(c.kind == "new-class", do: "new-class", else: c.selector)
                                  }
                                  phx-disable-with="Reverting…"
                                >
                                  revert
                                </button>
                              </td>
                            </tr>
                            <%!-- The net change vs disk (ADR 0082 Phase 5,
                                 BT-2575), now a structured, gutter-aligned,
                                 coloured diff (BT-2636) rendered full-width
                                 beneath the row while expanded. Reuses the shared
                                 `unified_diff/1` renderer with the Git pane. --%>
                            <tr :if={c[:diff] && expanded} class="diff-row">
                              <td colspan={if @role == :owner, do: 7, else: 6}>
                                <.unified_diff diff={c[:diff]} />
                              </td>
                            </tr>
                          <% end %>
                        </tbody>
                      </table>
                    <% end %>
                  </div>
                  <%!-- GIT tab (ADR 0082 Amendment 1, BT-2586): the post-flush,
                       human-facing VCS surface — disk↔HEAD, distinct from the
                       Changes pane's memory↔disk dirty view. status/diff/log are
                       :read (Observer-visible); stage/unstage/commit/revert are
                       :execute (Owner-gated) and the controls are hidden for the
                       Observer role. Degrades to an error note when the project
                       is not a git repo or `git` is absent. --%>
                  <div class="dock-pane panel-body git-pane" hidden={@dock_tab != "git"}>
                    <div class="git-toolbar">
                      <button class="btn-link" type="button" phx-click="git_refresh">
                        refresh
                      </button>
                      <span :if={@git_status} class="muted-note">
                        <%= if @git_status.branch do %>
                          on <span class="k">{@git_status.branch}</span>
                        <% else %>
                          detached HEAD
                        <% end %>
                        <span :if={@git_status.upstream}>
                          · ↑{@git_status.ahead} ↓{@git_status.behind}
                        </span>
                      </span>
                    </div>
                    <.notice
                      :if={@git_error}
                      variant={:err}
                      message={@git_error}
                      dismiss_attrs={
                        %{"phx-click" => "dismiss_notice", "phx-value-key" => "git_error"}
                      }
                    />
                    <%= cond do %>
                      <% is_nil(@git_status) and is_nil(@git_error) -> %>
                        <p class="muted-note">Loading git status…</p>
                      <% is_nil(@git_status) -> %>
                        <%!-- error already shown above --%>
                      <% @git_status.files == [] -> %>
                        <p class="muted-note">Working tree clean — nothing to commit.</p>
                      <% true -> %>
                        <table class="bt-table">
                          <thead>
                            <tr>
                              <th>Path</th>
                              <th>Staged</th>
                              <th>Working</th>
                              <th></th>
                              <th :if={@role == :owner}></th>
                            </tr>
                          </thead>
                          <tbody>
                            <tr :for={f <- @git_status.files}>
                              <td class="k">{f.path}</td>
                              <td>{git_state_label(f.index)}</td>
                              <td>{git_state_label(f.worktree)}</td>
                              <td>
                                <button
                                  class="btn-link"
                                  type="button"
                                  phx-click="git_diff"
                                  phx-value-path={f.path}
                                >
                                  {if @git_diff_path == f.path, do: "hide diff", else: "diff"}
                                </button>
                              </td>
                              <td :if={@role == :owner}>
                                <%!-- Staged files unstage; everything else stages.
                                     Tracked changes can also be reverted (discard
                                     working-tree edits) — the human counterpart to
                                     the agent ChangeLog `revert:`. --%>
                                <button
                                  :if={f.index == :unmodified}
                                  class="btn-link"
                                  type="button"
                                  phx-click="git_stage"
                                  phx-value-path={f.path}
                                  phx-disable-with="Staging…"
                                >
                                  stage
                                </button>
                                <button
                                  :if={f.index != :unmodified}
                                  class="btn-link"
                                  type="button"
                                  phx-click="git_unstage"
                                  phx-value-path={f.path}
                                  phx-disable-with="Unstaging…"
                                >
                                  unstage
                                </button>
                                <button
                                  :if={f.worktree not in [:unmodified, :untracked]}
                                  class="btn-link"
                                  type="button"
                                  phx-click="git_revert"
                                  phx-value-path={f.path}
                                  data-confirm={"Discard working-tree changes to #{f.path}?"}
                                  phx-disable-with="Reverting…"
                                >
                                  revert
                                </button>
                              </td>
                            </tr>
                          </tbody>
                        </table>
                        <%!-- BT-2636: the git staged/worktree diffs reuse the
                             shared structured `unified_diff/1` renderer (coloured,
                             gutter-aligned) the Changes pane uses. The empty
                             "No textual diff" state is preserved for binary /
                             mode-only changes. --%>
                        <div :if={@git_diff} class="git-diff-view">
                          <p class="muted-note">{@git_diff_path}</p>
                          <div :if={@git_diff.staged != ""}>
                            <p class="muted-note">staged</p>
                            <.unified_diff diff={@git_diff.staged} />
                          </div>
                          <div :if={@git_diff.worktree != ""}>
                            <p class="muted-note">working tree</p>
                            <.unified_diff diff={@git_diff.worktree} />
                          </div>
                          <p
                            :if={@git_diff.staged == "" and @git_diff.worktree == ""}
                            class="muted-note"
                          >
                            No textual diff (binary or mode-only change).
                          </p>
                        </div>
                        <%!-- Commit the staged index (Owner only). System `git`
                             applies hooks/signing/config. --%>
                        <form :if={@role == :owner} class="git-commit-form" phx-submit="git_commit">
                          <input
                            type="text"
                            name="message"
                            placeholder="Commit message"
                            autocomplete="off"
                          />
                          <button type="submit" phx-disable-with="Committing…">commit</button>
                        </form>
                    <% end %>
                    <%!-- Recent history (last commits). Read-only, always shown. --%>
                    <div :if={@git_log != []} class="git-log">
                      <h4 class="muted-note">Recent commits</h4>
                      <table class="bt-table">
                        <tbody>
                          <tr :for={c <- @git_log}>
                            <td class="k">{c.short_sha}</td>
                            <td>{c.subject}</td>
                            <td class="muted-note">{c.author}</td>
                          </tr>
                        </tbody>
                      </table>
                    </div>
                  </div>
                  <%!-- TESTS tab (BT-2557): the cockpit Test Runner. Discovery
                       (`list_tests`, :read) lists the live image's TestCase
                       subclasses; running (`run_tests`, :execute) is owner-gated
                       like the eval form. The last run's per-case results render
                       above the catalogue; a failing case opens in the method
                       editor. --%>
                  <div class="dock-pane panel-body test-pane" hidden={@dock_tab != "tests"}>
                    <div class="test-toolbar">
                      <%!-- BT-2597: disabled while a run/load is in flight off-socket
                           (`@tests_running`) — `phx-disable-with` alone reverts the
                           moment the async event handler re-renders. --%>
                      <button
                        :if={@role == :owner}
                        type="button"
                        class="btn"
                        phx-click="run_tests"
                        phx-disable-with="Running…"
                        disabled={@tests_running}
                      >
                        Run all
                      </button>
                      <%!-- Load the project's test/ files into the image (BT-2557).
                           A freshly-opened project loads only src/, so the
                           catalogue is empty until tests are loaded. Owner-only
                           (it compiles + loads user code). --%>
                      <button
                        :if={@role == :owner}
                        type="button"
                        class="btn ghost"
                        phx-click="load_tests"
                        phx-disable-with="Loading…"
                        disabled={@tests_running}
                      >
                        Load tests
                      </button>
                      <%!-- BT-2597/BT-2599: also gated by `@tests_running` — a
                           manual refresh mid-load kicks off a `:test_discover`
                           probe (`tests_refresh`) that would race the in-flight
                           load's own re-discovery (`apply_test_load`) and could
                           flash a stale catalogue. (Discovery itself is now
                           off-socket via `start_async`, BT-2599.) --%>
                      <button
                        type="button"
                        class="btn ghost"
                        phx-click="tests_refresh"
                        disabled={@tests_running}
                      >
                        Refresh
                      </button>
                      <span :if={@tests_running} class="muted-note">Running…</span>
                      <span
                        :if={@test_results}
                        class={["test-summary", @test_results["failed"] > 0 && "fail"]}
                      >
                        {@test_results["passed"]}/{@test_results["total"]} passed<span :if={
                          @test_results["failed"] > 0
                        }>, {@test_results["failed"]} failed</span>
                        <span :if={@test_results["skipped"] > 0}>
                          , {@test_results["skipped"]} skipped
                        </span>
                        <span :if={@test_results["duration"]} class="test-duration">
                          · {format_test_duration(@test_results["duration"])}
                        </span>
                      </span>
                    </div>
                    <.notice
                      :if={@tests_error}
                      variant={:err}
                      message={@tests_error}
                      dismiss_attrs={
                        %{"phx-click" => "dismiss_notice", "phx-value-key" => "tests_error"}
                      }
                    />

                    <table
                      :if={@test_results && @test_results["tests"] != []}
                      class="bt-table test-results"
                    >
                      <thead>
                        <tr>
                          <th>Status</th>
                          <th>Class</th>
                          <th>Test</th>
                          <th>Detail</th>
                        </tr>
                      </thead>
                      <tbody>
                        <tr
                          :for={t <- @test_results["tests"]}
                          class={["test-row", test_status_class(t["status"])]}
                        >
                          <td class={["test-status", test_status_class(t["status"])]}>
                            {test_status_label(t["status"])}
                          </td>
                          <td class="k">{t["class"]}</td>
                          <td>
                            <%!-- Open a failing/any case in the method editor
                                 (owner-only — the editor is a write surface). --%>
                            <button
                              :if={t["class"] != "" and @role == :owner}
                              type="button"
                              class="btn-link"
                              phx-click="open_test_method"
                              phx-value-class={t["class"]}
                              phx-value-selector={t["name"]}
                            >
                              {t["name"]}
                            </button>
                            <span :if={t["class"] == "" or @role != :owner}>{t["name"]}</span>
                          </td>
                          <td class="test-detail">{t["detail"]}</td>
                        </tr>
                      </tbody>
                    </table>

                    <%= cond do %>
                      <% is_nil(@test_classes) -> %>
                        <p class="muted-note">Loading tests…</p>
                      <% @test_classes == [] -> %>
                        <p class="muted-note">
                          No TestCase subclasses loaded. Press <strong>Load tests</strong>
                          to load your project's <code>test/</code>
                          files, or define one
                          (<code>TestCase subclass: …</code>).
                        </p>
                      <% true -> %>
                        <table class="bt-table test-catalogue">
                          <thead>
                            <tr>
                              <th>Class</th>
                              <th>Tests</th>
                              <th>Last run</th>
                              <th :if={@role == :owner}></th>
                            </tr>
                          </thead>
                          <tbody>
                            <tr :for={tc <- @test_classes}>
                              <td class="k">{tc["class"]}</td>
                              <td>{length(tc["selectors"])}</td>
                              <td class="test-tally">
                                <%= case test_class_tally(@test_results, tc["class"]) do %>
                                  <% nil -> %>
                                    <span class="muted-note">—</span>
                                  <% tally -> %>
                                    <span :if={tally.passed > 0} class="st-pass">
                                      {tally.passed}✓
                                    </span>
                                    <span :if={tally.failed > 0} class="st-fail">
                                      {tally.failed}✗
                                    </span>
                                    <span :if={tally.skipped > 0} class="st-skip">
                                      {tally.skipped}○
                                    </span>
                                <% end %>
                              </td>
                              <td :if={@role == :owner}>
                                <button
                                  type="button"
                                  class="btn-link"
                                  phx-click="run_test_class"
                                  phx-value-class={tc["class"]}
                                  phx-disable-with="Running…"
                                  disabled={@tests_running}
                                >
                                  run
                                </button>
                              </td>
                            </tr>
                          </tbody>
                        </table>
                    <% end %>
                  </div>
                </div>
              </div>
              <%!-- Dock restore bar: shown when dock is collapsed. A real <button>
                   (not a <div>) so it is keyboard-reachable and announced as
                   interactive; `.dock-bar` styling is class-driven either way. --%>
              <button
                :if={!@show_dock}
                type="button"
                class="dock-bar"
                phx-click="toggle_dock"
                aria-label="Expand workspace dock"
                title="Expand dock"
                style="order:3;"
              >
                Workspace ▴
              </button>

              <%!-- Draggable divider (BT-2576) between the method editor and the
                   workspace dock. The SplitDrag hook writes `--dock-h` on this
                   `.col` parent; the dock reads it. Hidden when the dock is
                   collapsed.

                   Unlike the other gutters this one is NOT `phx-update="ignore"`
                   (BT-2638). Its `.col` parent is the center column that holds
                   the editor + dock and re-renders on every diff/new-method tab;
                   morphdom strips the JS-set `--dock-h` inline style off that
                   `.col`, snapping the dock back to its 230px default. The empty
                   gutter div has no hook-owned inner DOM to protect, so dropping
                   `ignore` is safe and lets the hook's `updated()` callback fire
                   after each patch to re-apply the persisted size. --%>
              <div
                :if={@show_dock}
                id="dock-split-gutter"
                class="split-gutter split-gutter-y"
                phx-hook="SplitDrag"
                role="separator"
                aria-orientation="horizontal"
                aria-label="Resize the editor and workspace dock"
                data-split="dock"
                data-axis="y"
                data-edge="end"
                data-var="--dock-h"
                data-min="100"
                data-min-other="120"
                style="order:2;"
              >
              </div>

              <%!-- TABBED METHOD EDITOR (BT-2494): the spike's write-surface.
                   A tab strip (methods + class definitions) over a breadcrumb
                   and the BT-2485 highlighted editor. The single save_method
                   form is preserved (id, phx-submit, ⌘S, name="class"/"selector"/
                   "source") so the BT-2409 e2e flows keep working; the active tab
                   rides as a hidden `tab` field so a compile clears its dirty dot
                   and a class-definition tab compiles the class via `eval`. --%>
              <div id="method-editor" class="panel editor-panel" style="order:1;">
                <%!-- tab strip: one tab per open method / class definition, with
                     a per-tab dirty dot and a close × (the last tab stays open). --%>
                <div class="tabstrip" role="tablist">
                  <button
                    :for={t <- @tabs}
                    type="button"
                    role="tab"
                    class={["tab", @active_tab == t.id && "on"]}
                    aria-selected={to_string(@active_tab == t.id)}
                    phx-click="tab_select"
                    phx-value-id={t.id}
                  >
                    <span :if={t.dirty} class="modot" title="unsaved edits"></span>
                    <span class="tab-label mono">
                      {cond do
                        t.kind == :def -> t.class <> " ▸ def"
                        # BT-2667: a read-only native module tab — labelled by the
                        # module name with an `.erl` suffix so it reads as native
                        # source, not a Beamtalk class/method.
                        t.kind == :native -> t.class <> ".erl"
                        # Placeholder label for an unsaved new-method tab. Parens
                        # keep it from being read as a real `new` selector tab,
                        # which renders as plain "new" via the branch below
                        # (BT-2613).
                        t.new -> t.class <> " ▸ (new method)"
                        true -> t.selector
                      end}
                    </span>
                    <span class="x" title="Close tab" phx-click="tab_close" phx-value-id={t.id}>
                      ×
                    </span>
                  </button>
                  <span class="spacer"></span>
                  <%!-- "+ def" opens (or focuses) the active class's definition
                       tab — saving it compiles the class (ADR 0082). Hidden when
                       nothing is open: it takes its class from the active tab, so
                       there is no class to open a definition for. --%>
                  <button
                    :if={
                      @role == :owner and not is_nil(@active_tab) and
                        not match?(%{kind: :native}, active_tab(assigns))
                    }
                    type="button"
                    class="tab tab-add"
                    title="Open class definition"
                    phx-click="open_definition"
                  >
                    + def
                  </button>
                </div>

                <%!-- Operation feedback (save / compile / new-class / revert /
                     flush) — rendered above the editor/empty-state split so it
                     shows whether or not a tab is focused. A method or class save
                     posted from the empty-state form (no active tab) still surfaces
                     its "Saved …" / error banner here. --%>
                <.notice
                  :if={@save_result}
                  variant={:ok}
                  message={@save_result}
                  dismiss_attrs={%{"phx-click" => "dismiss_notice", "phx-value-key" => "save_result"}}
                />
                <.notice
                  :if={@save_error}
                  variant={:err}
                  message={@save_error}
                  dismiss_attrs={%{"phx-click" => "dismiss_notice", "phx-value-key" => "save_error"}}
                />
                <.notice
                  :if={@flush_result}
                  variant={:warn}
                  message={@flush_result}
                  dismiss_attrs={
                    %{"phx-click" => "dismiss_notice", "phx-value-key" => "flush_result"}
                  }
                />
                <.notice
                  :if={@flush_error}
                  variant={:err}
                  message={@flush_error}
                  dismiss_attrs={%{"phx-click" => "dismiss_notice", "phx-value-key" => "flush_error"}}
                />

                <%= cond do %>
                  <% match?(%{kind: :native}, active_tab(assigns)) -> %>
                    <%!-- BT-2667: a standalone native module opened as an editor TAB
                         (not the retired single-slot overlay). It scrolls inside the
                         editor pane (BT-2658) and shows a clean, project-relative
                         source path (BT-2668) — never the absolute host path.
                         BT-2670: a *project-owned* native (`view.editable == true`)
                         is editable for the Owner — edit → compile → reload →
                         write-back via `native_save`. Deps/stdlib natives, and every
                         non-Owner role, stay strictly read-only. --%>
                    <% nt = active_tab(assigns) %>
                    <% nt_editable = nt.native_view.editable and @role == :owner %>
                    <div class="editor-meta">
                      <span class="crumb">
                        <span class="native-badge">Erlang module</span>
                        <b class="mono">{nt.class}</b>
                        <span :if={nt.native_view.source_file} class="sep">›</span>
                        <span :if={nt.native_view.source_file} class="mono native-path">
                          {nt.native_view.source_file}
                        </span>
                      </span>
                      <span class="spacer"></span>
                      <%!-- BT-2669: the reverse of "go to native source" — list the
                           Beamtalk class>>method sites that call into this native
                           module via `(Erlang <module>) …`. Reuses the
                           Senders/Implementors popover (BT-2495); each row opens the
                           calling method. --%>
                      <div class="nav-actions">
                        <button class="btn" type="button" phx-click="native_callers">
                          Callers
                        </button>
                        <.nav_popover nav={@nav_popover} />
                      </div>
                      <span
                        :if={nt_editable}
                        class="meta-note editable"
                        title="project-owned native (.erl) source — editable"
                      >
                        editable
                      </span>
                      <span
                        :if={not nt_editable}
                        class="runtime-tag"
                        title="read-only native (.erl) source"
                      >
                        read-only
                      </span>
                    </div>
                    <%= if nt_editable do %>
                      <%!-- BT-2670: editable native (.erl) buffer. The CodeMirror
                           editor (Erlang mode) is re-keyed on `@active_tab` +
                           `@editor_rev` so switching/refreshing the tab remounts the
                           hook with the right source. ⌘S submits this form (same
                           chord as the method editor); the hidden `source` textarea
                           is the posted field, mirrored by the hook. On a clean
                           compile the dirty dot clears and the success banner shows;
                           a compile error renders inline via the shared @save_error
                           notice (rendered above). --%>
                      <div class="panel-body native-tab-body">
                        <form
                          id="native-editor-form"
                          phx-submit="native_save"
                          phx-change="edit_source"
                          phx-hook="KeyboardShortcuts"
                          data-scope="window"
                          data-shortcuts={Jason.encode!(%{"mod+s" => "submit"})}
                        >
                          <div
                            id={"native-editor-overlay-" <> @active_tab <> "-" <> to_string(@editor_rev)}
                            class="cm-wrap field"
                            phx-hook="CmEditor"
                            data-select-event="select_source"
                            data-tab-id={@active_tab}
                            data-lint-mode="erlang"
                          >
                            <textarea
                              id={"native-editor-source-" <> @active_tab <> "-" <> to_string(@editor_rev)}
                              class="cm-field"
                              name="source"
                              spellcheck="false"
                              autocomplete="off"
                              phx-debounce="300"
                              phx-update="ignore"
                              hidden
                            ><%= @edit_source %></textarea>
                            <div
                              class="cm-host"
                              id={"native-editor-cm-" <> @active_tab <> "-" <> to_string(@editor_rev)}
                              phx-update="ignore"
                            >
                            </div>
                          </div>
                          <div class="editor-actions">
                            <span class="spacer"></span>
                            <button class="btn btn-sm primary" type="submit">
                              Compile &amp; Reload <span class="k">⌘S</span>
                            </button>
                          </div>
                        </form>
                      </div>
                    <% else %>
                      <div class="panel-body native-tab-body">
                        <.native_source_body
                          view={nt.native_view}
                          fallback_module={nt.class}
                          dismiss_event="dismiss_native_module_error"
                        />
                      </div>
                    <% end %>
                  <% match?(%{}, active_tab(assigns)) -> %>
                    <%!-- breadcrumb: Class › side › selector for the active tab. --%>
                    <% {bc_class, bc_side, bc_sel} = breadcrumb(active_tab(assigns)) %>
                    <div class="editor-meta">
                      <span class="crumb">
                        <b>{bc_class}</b>
                        <span :if={bc_side} class="sep">›</span>
                        <span :if={bc_side} class="mono">{bc_side}</span>
                        <span class="sep">›</span>
                        <span class="mono">{bc_sel}</span>
                      </span>
                      <%!-- BT-2605: class/method modifier badges (Class / Sealed /
                       Abstract / Native) derived from the active tab's side +
                       reflected class modifiers. Sit next to the breadcrumb, left
                       of the spacer, so the image-divergence badges keep the right
                       edge. Empty list → nothing rendered. --%>
                      <span
                        :for={badge <- modifier_badges(active_tab(assigns))}
                        class={"modifier-tag #{badge.class}"}
                        title={badge.title}
                        aria-label={badge.title}
                      >
                        {badge.label}
                      </span>
                      <%!-- BT-2642: package/origin badge for the active tab, shown
                       for every tab kind (method / class-definition) incl. project.
                       Reuses BT-2641's vocabulary (STDLIB / DEP · <pkg>) and adds
                       the bare project package name; colored by origin via
                       `header_origin_class/1`. Sits with the modifier badges, left
                       of the spacer, so the right-edge divergence badges are clear.
                       Empty label (unknown origin / packageless project) → hidden. --%>
                      <% pkg_tab = active_tab(assigns) %>
                      <% pkg_label = header_package_label(pkg_tab) %>
                      <span
                        :if={pkg_label != ""}
                        class={"source-origin-tag header #{header_origin_class(pkg_tab)}"}
                        title={header_origin_title(pkg_tab)}
                        aria-label={header_origin_title(pkg_tab)}
                      >
                        {pkg_label}
                      </span>
                      <%!-- Doc toggle inline on the breadcrumb line (BT-2604). --%>
                      <% doc_tab = active_tab(assigns) %>
                      <%!-- BT-2714: a compiler-derived tab has no editor competing
                       for space, so its doc block is forced open below and the
                       collapse toggle is hidden (nothing to collapse into). --%>
                      <button
                        :if={doc_tab.doc != nil and not synthetic_tab?(doc_tab)}
                        type="button"
                        class="doc-toggle-inline"
                        phx-click="toggle_doc"
                        aria-expanded={to_string(@doc_expanded)}
                        title={
                          if @doc_expanded, do: "Collapse documentation", else: "Expand documentation"
                        }
                      >
                        <span class="doc-caret">{if @doc_expanded, do: "▾", else: "▸"}</span>
                        <span class="doc-label">{doc_summary_label(doc_tab)}</span>
                      </button>
                      <span class="spacer"></span>
                      <%!-- image-divergence badges carried from the browse snapshot
                       (the indicators the old read-only Method Source pane showed):
                       an unflushed live `>>` patch, or a sourceless runtime
                       method. A runtime-only method (no on-disk body) is
                       suppressed from the `unflushed` badge (BT-2550): there is
                       no disk counterpart for the image to "differ" from, so the
                       'image differs from disk' tooltip would be misleading — the
                       ⚡ runtime badge below is the honest signal there. --%>
                      <span
                        :if={
                          active_tab(assigns).disk_differs and not active_tab(assigns).runtime_only
                        }
                        class="runtime-tag"
                        title="unflushed live patch (image differs from disk)"
                      >
                        unflushed
                      </span>
                      <span
                        :if={active_tab(assigns).runtime_only}
                        class="runtime-tag"
                        title="runtime-only (no source on disk)"
                      >
                        ⚡
                      </span>
                      <span :if={@role != :owner} class="meta-note read-only">
                        read-only
                      </span>
                      <span :if={@role == :owner and active_tab(assigns).new} class="meta-note edited">
                        new
                      </span>
                      <span
                        :if={
                          @role == :owner and not active_tab(assigns).new and
                            active_tab(assigns).dirty
                        }
                        class="meta-note edited"
                      >
                        edited
                      </span>
                      <%!-- BT-2714: suppressed on a synthetic tab — a compiler-derived
                       method is always "in image" (generated at compile time, never
                       flushed), so the note would be meaningless noise there. --%>
                      <span
                        :if={
                          @role == :owner and not active_tab(assigns).new and
                            not active_tab(assigns).dirty and
                            not synthetic_tab?(active_tab(assigns))
                        }
                        class="meta-note"
                      >
                        in image
                      </span>
                    </div>

                    <div class="panel-body">
                      <%!-- Read-only documentation body (BT-2558, BT-2604): the toggle
                       now lives on the breadcrumb line; here we only render the
                       expanded doc body when the user has opened it. `doc_tab` is
                       already bound in the breadcrumb section above. --%>
                      <div
                        :if={doc_tab.doc != nil and (@doc_expanded or synthetic_tab?(doc_tab))}
                        id="doc-body-content"
                        class="doc-body-inline"
                      >
                        {BtAttach.DocFormat.to_html(doc_tab.doc)}
                      </div>
                      <%!-- BT-2578: on a `self delegate` method (ADR 0056), a jump
                       to its Erlang implementation. Opens the class-definition
                       tab's native pane with this selector's `handle_call` clause
                       highlighted. Every role sees it (the op is `:read`). --%>
                      <div
                        :if={doc_tab.kind == :method and doc_tab.native_delegate}
                        class="native-delegate-link"
                      >
                        <span class="native-badge">Native delegate</span>
                        <button
                          type="button"
                          class="native-toggle"
                          phx-click="browser_jump_native"
                          phx-value-class={doc_tab.class}
                          phx-value-selector={doc_tab.selector}
                        >
                          → Erlang source
                        </button>
                      </div>
                      <%!-- BT-2578: read-only native backing-source pane. On a
                       class-definition tab for a native: class (ADR 0056) it
                       badges the backing gen_server module and, on toggle, shows
                       that module's `.erl` read-only — the real logic lives in
                       its `handle_call` clauses, not the `self delegate` facade
                       methods. The `browse-native-source` op is `:read`, so every
                       role sees it. `content == nil` degrades to a clear empty
                       state, not an error. --%>
                      <section
                        :if={doc_tab.kind == :def and doc_tab.native_module}
                        class="native-block"
                        aria-label="Native implementation"
                      >
                        <div class="native-head">
                          <span class="native-badge">Erlang backend</span>
                          <code class="native-module mono">{doc_tab.native_module}</code>
                          <button
                            type="button"
                            class="native-toggle"
                            phx-click="browser_open_native"
                            phx-value-class={doc_tab.class}
                            aria-expanded={to_string(native_shown?(assigns, doc_tab.class))}
                          >
                            {if native_shown?(assigns, doc_tab.class),
                              do: "Hide Erlang source",
                              else: "View Erlang source"}
                          </button>
                          <%!-- BT-2659: direct navigation from a native: class to its
                           backing Erlang module's full source. Unlike the inline
                           "View Erlang source" pane above (BT-2578), this opens the
                           module's complete `.erl` as its own read-only editor tab
                           (reusing `open_native_module_tab/2`), so the whole module —
                           not just the matched clauses — is reachable in one click. --%>
                          <button
                            type="button"
                            class="native-toggle"
                            phx-click="browser_open_native_module"
                            phx-value-module={doc_tab.native_module}
                            title={"Open #{doc_tab.native_module}.erl in a source tab"}
                          >
                            Open native source →
                          </button>
                        </div>
                        <div :if={native_shown?(assigns, doc_tab.class)} class="native-body">
                          <.native_source_body
                            view={@native_view}
                            fallback_module={doc_tab.native_module}
                            dismiss_event="dismiss_native_error"
                          />
                        </div>
                      </section>
                      <%= cond do %>
                        <% synthetic_tab?(active_tab(assigns)) -> %>
                          <%!-- BT-2714: a compiler-derived method (value accessor,
                           `with<Field>:` setter, actor `new`/`spawn`) has no editable
                           source. Render read-only where the editor would be: the
                           resolved signature + doc already show in the doc block above
                           (forced open for synthetic tabs), so this panel states the
                           read-only reason and keeps Senders/Implementors navigation.
                           Shown for every role, owner included — there is nothing to
                           edit, so the blank editable buffer (the old bug) is gone. --%>
                          <div class="synthetic-note">
                            <div :if={active_tab(assigns).signature} class="mono synthetic-sig">
                              {active_tab(assigns).signature}
                            </div>
                            <p class="muted-note">
                              Compiler-derived method — auto-generated, with no editable
                              source. Any documentation is shown above.
                            </p>
                            <div :if={active_tab(assigns).kind == :method} class="nav-actions">
                              <button class="btn" type="button" phx-click="senders">
                                Senders
                              </button>
                              <button class="btn" type="button" phx-click="implementors">
                                Implementors
                              </button>
                              <.nav_popover nav={@nav_popover} />
                            </div>
                          </div>
                        <% @role == :owner -> %>
                          <%!-- ⌘S submits this editor form via the KeyboardShortcuts
                         hook (BT-2485): the chord request-submits the form so
                         the class/selector/source/tab ride the normal phx-submit,
                         exactly as clicking "Compile" would. `phx-change` reports
                         live edits so the active tab's dirty dot tracks them. --%>
                          <form
                            id="method-editor-form"
                            phx-submit="save_method"
                            phx-change="edit_source"
                            phx-hook="KeyboardShortcuts"
                            data-scope="window"
                            data-shortcuts={Jason.encode!(%{"mod+s" => "submit"})}
                          >
                            <%!-- the active tab id rides every compile so the handler
                           knows which tab to clean and whether it's a class
                           definition. --%>
                            <input type="hidden" name="tab" value={@active_tab} />
                            <%!-- Class + selector ride the form as hidden fields — the
                           breadcrumb above is the canonical display of "which
                           class › selector this tab edits", so the old editable
                           inputs are redundant: the author types the full method
                           (signature + body) in the CodeMirror body, exactly like
                           editing an existing method. A "new method" tab has no
                           selector yet, so it posts an empty hidden field and the
                           save handler derives the selector by parsing the body's
                           signature (BT-2606). The save_method payload (class +
                           selector + source) shape is identical in every case. --%>
                            <input type="hidden" name="class" value={@edit_class} />
                            <% tab = active_tab(assigns) %>
                            <%= cond do %>
                              <% tab.kind == :def -> %>
                                <input type="hidden" name="selector" value="▸ class definition" />
                              <% true -> %>
                                <input type="hidden" name="selector" value={@edit_selector} />
                            <% end %>
                            <%!-- CodeMirror 6 editor (BT-2539). Re-keyed on the active
                           tab id (`@active_tab`) so switching tabs remounts the
                           CmEditor hook and the editor picks up the new tab's
                           source. BT-2655: the key also carries `@editor_rev`, bumped
                           when the active tab's body is re-read in place (a git
                           revert / push reconcile) so the editor remounts and shows
                           the new source even though `@active_tab` is unchanged — the
                           `phx-update="ignore"` host otherwise never re-seeds.
                           The hidden <textarea name="source"> stays the
                           posted form field (so save_method reads it) and is
                           phx-update="ignore" (hook-owned): the hook mirrors the
                           doc into it and fires `input`, driving the
                           phx-change="edit_source" dirty-dot tracking with the
                           300 ms debounce. Selection reports ride select_source
                           via data-select-event, kept in `:edit_selection`. --%>
                            <div
                              id={"method-editor-overlay-" <> @active_tab <> "-" <> to_string(@editor_rev)}
                              class="cm-wrap field"
                              phx-hook="CmEditor"
                              data-select-event="select_source"
                              data-tab-id={@active_tab}
                              data-lint-mode={if active_tab(assigns).kind == :method, do: "method"}
                            >
                              <textarea
                                id={"method-editor-source-" <> @active_tab <> "-" <> to_string(@editor_rev)}
                                class="cm-field"
                                name="source"
                                spellcheck="false"
                                autocomplete="off"
                                phx-debounce="300"
                                phx-update="ignore"
                                hidden
                              ><%= @edit_source %></textarea>
                              <div
                                class="cm-host"
                                id={"method-editor-cm-" <> @active_tab <> "-" <> to_string(@editor_rev)}
                                phx-update="ignore"
                              >
                              </div>
                            </div>
                            <%!-- Single action row (BT-2495): Senders / Implementors on
                           the left, Compile / Save All pushed to the right. The nav
                           buttons are type="button" (they fire phx-click, never
                           submit) and only show on a method tab. Observers get the
                           same nav row in the read-only branch below. --%>
                            <div class="editor-actions">
                              <div :if={active_tab(assigns).kind == :method} class="nav-actions">
                                <button class="btn" type="button" phx-click="senders">
                                  Senders
                                </button>
                                <button class="btn" type="button" phx-click="implementors">
                                  Implementors
                                </button>
                                <.nav_popover nav={@nav_popover} />
                              </div>
                              <%!-- Protocol action row (BT-2639): the protocol
                             equivalent of Senders/Implementors — only on a
                             class-definition tab whose class is a Protocol.
                             Observers get the same row in the read-only branch. --%>
                              <div
                                :if={
                                  active_tab(assigns).kind == :def and
                                    active_tab(assigns)[:is_protocol]
                                }
                                class="nav-actions"
                              >
                                <button class="btn" type="button" phx-click="required_methods">
                                  Required methods
                                </button>
                                <button class="btn" type="button" phx-click="conforming_classes">
                                  Conforming classes
                                </button>
                                <.nav_popover nav={@nav_popover} />
                              </div>
                              <span class="spacer"></span>
                              <button class="btn btn-sm primary" type="submit">
                                Compile <span class="k">⌘S</span>
                              </button>
                              <button class="btn btn-sm" type="button" phx-click="flush">
                                Save All to Disk
                              </button>
                            </div>
                          </form>
                          <%!-- NEW CLASS (BT-2293, ADR 0082 Phase 5): the create-a-class
                         affordance now lives in the System Browser head (class-
                         oriented, collapsed by default), not here under the method
                         editor — see `system_browser_classes`. --%>
                        <% true -> %>
                          <p class="muted-note">
                            Your role is read-only — evaluation and editing are disabled. You can
                            still browse bindings, follow references in the Inspector, and watch
                            the live Transcript.
                          </p>
                          <%!-- Observers still get Senders / Implementors navigation
                         (BT-2495); both ride the read-only `nav-query` op. --%>
                          <div :if={active_tab(assigns).kind == :method} class="nav-actions">
                            <button class="btn" type="button" phx-click="senders">
                              Senders
                            </button>
                            <button class="btn" type="button" phx-click="implementors">
                              Implementors
                            </button>
                            <.nav_popover nav={@nav_popover} />
                          </div>
                          <%!-- Observers also get the protocol action row (BT-2639);
                         both ride the read-only `nav-query` op. --%>
                          <div
                            :if={
                              active_tab(assigns).kind == :def and active_tab(assigns)[:is_protocol]
                            }
                            class="nav-actions"
                          >
                            <button class="btn" type="button" phx-click="required_methods">
                              Required methods
                            </button>
                            <button class="btn" type="button" phx-click="conforming_classes">
                              Conforming classes
                            </button>
                            <.nav_popover nav={@nav_popover} />
                          </div>
                      <% end %>
                    </div>
                  <% true -> %>
                    <%!-- Empty state: the cockpit opens with no tab and lands here
                     after the last tab is closed. No CodeMirror placeholder
                     (which read as fake content); a plain hint instead. The
                     `save_method` form is still rendered (hidden, no editor) so
                     the BT-2409 e2e save flow — which posts class/selector/source
                     directly — keeps working without a focused tab; the handler
                     tolerates an absent `tab` and validates an empty class. --%>
                    <div class="panel-body">
                      <div class="empty">
                        Nothing open. Pick a method, or open a <span class="mono">▸ class definition</span>, from the System
                        Browser to start editing.
                      </div>
                      <form
                        :if={@role == :owner}
                        id="method-editor-form"
                        phx-submit="save_method"
                        phx-change="edit_source"
                        hidden
                      >
                        <input type="hidden" name="tab" value="" />
                        <input type="hidden" name="class" value="" />
                        <input type="hidden" name="selector" value="" />
                        <input type="hidden" name="source" value="" />
                      </form>
                    </div>
                <% end %>
              </div>
            </div>

            <%!-- RIGHT — Bindings + Inspector (348px), with ChangeLog + Transcript --%>
            <div class="col" inert={!@show_inspector}>
              <div class="right-split">
                <div id="bindings-panel" class="panel bindings-panel">
                  <div class="panel-head">
                    Bindings <span class="spacer"></span>
                    <span class="count">{length(@bindings)} in session</span>
                  </div>
                  <div class="panel-body">
                    <.notice
                      :if={@bindings_error}
                      variant={:err}
                      message={@bindings_error}
                      dismiss_attrs={
                        %{"phx-click" => "dismiss_notice", "phx-value-key" => "bindings_error"}
                      }
                    />
                    <%= if @bindings == [] do %>
                      <p class="empty">No bindings yet. Try <code>x := 42</code>.</p>
                    <% else %>
                      <%!-- Spike Bindings list (inspector.jsx `BindingsList`): each row is
                           `name := printString` with a type/kind chip. An object-valued
                           binding is drillable — clicking the row fires the existing
                           `inspect` event by name, and the explicit "Inspect →" affordance
                           carries the same phx-value-name the e2e test (BT-2408) clicks. --%>
                      <div class="obj-list">
                        <div
                          :for={b <- @bindings}
                          class={["obj-row", b.inspectable && "drillable"]}
                          phx-click={b.inspectable && "inspect"}
                          phx-value-name={b.inspectable && b.name}
                        >
                          <span class="bname mono">{b.name}</span>
                          <span class="bassign mono">:=</span>
                          <span class="ps mono">{b.value}</span>
                          <span class={["kind", b.kind]}>{b.kind}</span>
                          <button
                            :if={b.inspectable}
                            class="btn ghost obj-inspect"
                            type="button"
                            phx-click="inspect"
                            phx-value-name={b.name}
                          >
                            Inspect →
                          </button>
                        </div>
                      </div>
                    <% end %>
                  </div>
                </div>

                <%!-- Draggable divider (BT-2576): rebalances Bindings vs. the
                     Inspector. phx-update="ignore" (BT-2591): the gutter div is
                     empty and hook-owned, so LiveView should never patch it. The
                     async mount load DOES strip the hook-set --right-split var off
                     the PARENT .right-split (it re-renders the Bindings pane inside
                     it), but the SplitDrag hook's own MutationObserver — not
                     updated() — re-applies the saved size when that happens. --%>
                <div
                  id="right-split-gutter"
                  class="split-gutter split-gutter-y"
                  phx-hook="SplitDrag"
                  phx-update="ignore"
                  role="separator"
                  aria-orientation="horizontal"
                  aria-label="Resize the Bindings and Inspector panels"
                  data-split="right"
                  data-axis="y"
                  data-edge="start"
                  data-var="--right-split"
                  data-min="80"
                  data-min-other="120"
                >
                </div>

                <div id="inspector-panel" class="panel insp inspector-panel">
                  <div class="panel-head">
                    Inspector <span class="spacer"></span>
                    <%!-- Freeze toggle (BT-2492, spike `iw-freeze`): live tracking
                         subscribes to the object's change stream and flashes
                         changed fields; freezing holds the current snapshot. Shown
                         only for a pid-backed (watchable) target. --%>
                    <button
                      :if={@inspect_target && @inspect_target.pid}
                      type="button"
                      class={["insp-freeze", (@inspect_frozen && "frozen") || "live"]}
                      phx-click="freeze_toggle"
                      title={
                        if @inspect_frozen,
                          do: "Frozen snapshot — click to track live",
                          else: "Tracking live (subscribed to changes) — click to freeze a snapshot"
                      }
                    >
                      <span class="iwf-dot"></span>{(@inspect_frozen && "frozen") || "live"}
                    </button>
                    <span :if={@inspect_target} class="count">following references</span>
                    <button
                      type="button"
                      class="panel-close"
                      phx-click="close_inspector"
                      aria-label="Close the Inspector (the Bindings pane stays open)"
                      title="Close the Inspector (the Bindings pane stays open)"
                    >
                      ×
                    </button>
                  </div>
                  <%= if @inspect_target do %>
                    <%!-- Spike Inspector head (inspector.jsx `InspectorContent`): a
                         drill breadcrumb of the references followed so far, the live
                         printString, and class/pid/stats chips. Each crumb re-inspects
                         that level via the existing read-surface inspect path. The word
                         "Inspecting" is retained for the BT-2408 e2e assertion. --%>
                    <div class="insp-head">
                      <div :if={length(@inspect_crumbs) > 1} class="insp-crumbs">
                        <%= for {crumb, i} <- Enum.with_index(@inspect_crumbs) do %>
                          <span :if={i > 0} class="sep">›</span>
                          <span class="c" phx-click="crumb" phx-value-index={i}>{crumb.label}</span>
                        <% end %>
                      </div>
                      <div class="ps mono">
                        Inspecting <strong>{@inspect_target.label}</strong>
                        <span class="ps-header">{@inspect_target.header}</span>
                      </div>
                      <%!-- class/pid chips plus the live pid-stats chips (BT-2492):
                           process status, mailbox depth, reductions — read via the
                           `pid_stats` op and refreshed on every change push, the
                           spike's process-health line. --%>
                      <div class="proc-chips">
                        <span class="chip">class <b>{@inspect_target.class_name}</b></span>
                        <span :if={@inspect_target.pid} class="chip">
                          pid <b>{@inspect_target.pid}</b>
                        </span>
                        <span :if={stat_status(@inspect_stats)} class="chip pid-stat">
                          <span class="dot"></span>{stat_status(@inspect_stats)}
                        </span>
                        <%!-- not is_nil, not truthiness: a mailbox depth of 0 (the
                             actor drained) is the most reassuring reading and must
                             still show, but 0 is falsy in a HEEx `:if`. --%>
                        <span :if={not is_nil(stat_mailbox(@inspect_stats))} class="chip pid-stat">
                          mailbox <b>{stat_mailbox(@inspect_stats)}</b>
                        </span>
                        <span :if={not is_nil(stat_reductions(@inspect_stats))} class="chip pid-stat">
                          reductions <b>{stat_reductions(@inspect_stats)}</b>
                        </span>
                      </div>
                    </div>
                  <% end %>
                  <div class="panel-body">
                    <.notice
                      :if={@inspect_error}
                      variant={:warn}
                      message={@inspect_error}
                      dismiss_attrs={
                        %{"phx-click" => "dismiss_notice", "phx-value-key" => "inspect_error"}
                      }
                    />
                    <%= if @inspect_target && @inspect_rows != [] do %>
                      <%!-- The FieldFlash hook (assets/js/hooks/field_flash.js) reads
                           each cell's `data-flash-key`+`data-flash-val` and, when a
                           value changes on a live refresh (`data-flash-gen` bumps),
                           flashes only the changed cells — debounced so a burst can't
                           storm. Server-side the change push is already coalesced. --%>
                      <table
                        id="inspector-fields"
                        class="ivar-table"
                        phx-hook="FieldFlash"
                        data-flash-gen={@flash_gen}
                      >
                        <tbody>
                          <tr
                            :for={{row, i} <- Enum.with_index(@inspect_rows)}
                            class={row.drillable && "drillable"}
                            phx-click={row.drillable && "drill"}
                            phx-value-index={row.drillable && i}
                          >
                            <td class="k">{row.name}</td>
                            <td
                              class={["v", row.kind]}
                              data-flash-key={row.name}
                              data-flash-val={row.value}
                            >
                              {row.value}
                            </td>
                            <td class="follow">
                              <span :if={row.drillable} class="follow-link">follow →</span>
                            </td>
                          </tr>
                        </tbody>
                      </table>
                    <% else %>
                      <p :if={@inspect_target == nil && @inspect_error == nil} class="empty">
                        Spawn an object (<code>Counter spawn</code>), bind it, then Inspect it to
                        follow its live references.
                      </p>
                      <%!-- BT-2634: a live target with no rows and no error — an empty
                           supervisor (no running children) or an actor with no
                           user-visible state. Show a clear empty state rather than a
                           blank body. --%>
                      <p :if={@inspect_target != nil && @inspect_error == nil} class="empty">
                        No inspectable content — this object has no fields or children.
                      </p>
                    <% end %>
                    <%!-- Owner-only poke bar (BT-2492, spike PokeBar): send a Beamtalk
                         message to the inspected actor. Rendered only for a pid-backed
                         target at a pokeable root (a single named-binding crumb) AND
                         the owner role — an Observer's eval is refused by RBAC, so the
                         bar is hidden for them (a crafted poke is still refused
                         server-side), and a drilled field has no session binding to
                         address. Sends `<binding> <message>` via eval. --%>
                    <div
                      :if={
                        @inspect_target && @inspect_target.pid && @role == :owner &&
                          pokeable?(assigns)
                      }
                      class="poke"
                    >
                      <div class="poke-label">Send a message to {@inspect_target.label}</div>
                      <form class="poke-row" phx-submit="poke">
                        <span class="poke-recv mono">‹recv›</span>
                        <input
                          class="field mono"
                          name="message"
                          autocomplete="off"
                          placeholder="increment   ·   incrementBy: 10"
                        />
                        <button type="submit" class="btn">Send</button>
                      </form>
                      <div :if={@poke_result} class="poke-out ok mono">{@poke_result}</div>
                      <div :if={@poke_error} class="poke-out warn mono">{@poke_error}</div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <%!-- Floating inspector windows (BT-2493): the overlay layer of draggable,
               stackable inspector windows opened in Float mode. Rendered whenever
               windows are open — even if the user has since flipped back to Docked,
               so their state + positions persist — but pointer-inert (the layer is
               `pointer-events:none`; each window re-enables its own) so it never
               eats clicks on the cockpit beneath when empty. Each window reuses the
               docked Inspector's content (target/crumbs/rows/chips/poke) keyed by
               its id, with its own drag handle, close button and z-order. --%>
          <.inspector_windows :if={@windows != []} windows={@windows} role={@role} />
          <%!-- The Changes (ChangeLog) viewer and the live Transcript stream now
               live in the tabbed Workspace dock above (BT-2490), not a separate
               full-width footer. --%>
        <% else %>
          <div class="cockpit" style="grid-template-columns: minmax(0, 1fr);">
            <div class="col">
              <div class="panel" style="flex:1;">
                <div class="panel-head">Workspace</div>
                <div class="panel-body">
                  <%= if @error do %>
                    <div class="io-block err">
                      Not attached. {@error} Start a workspace and export its node + cookie:
                      beamtalk workspace create spike --background --persistent
                      export BT_WORKSPACE_NODE=beamtalk_workspace_spike@localhost
                      export BT_WORKSPACE_COOKIE=$(sed 's/-setcookie //;s/ //g' ~/.beamtalk/workspaces/spike/vm.args)
                      then restart this server.
                    </div>
                  <% else %>
                    <p class="muted-note">Connecting to workspace…</p>
                  <% end %>
                </div>
              </div>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  # ── floating inspector windows (BT-2493, epic BT-2482 Phase 3) ───────────────
  #
  # The overlay layer: one draggable, stackable inspector window per `:windows`
  # entry. The layer itself is pointer-inert so it never eats clicks on the
  # cockpit beneath; each window re-enables pointer events on itself. Window
  # position (`left`/`top`) and stacking (`z-index`) come straight from the
  # per-window state the WindowDrag hook reports on drop / focus, so they survive an
  # unrelated re-render. Each window's content reuses the docked Inspector's markup
  # (head + breadcrumb + chips + ivar table + poke), parameterised by window id so
  # its drill/crumb/close/freeze/poke events carry the id back to the right window.

  attr :windows, :list, required: true
  attr :role, :atom, required: true

  defp inspector_windows(assigns) do
    ~H"""
    <div class="insp-overlay" id="inspector-overlay">
      <%!-- Off-screen recovery (BT-2527 #4): re-cascade every window back onto the
           visible ladder. Shown only when a desk is open so it never floats over
           an empty cockpit. --%>
      <button
        :if={@windows != []}
        type="button"
        class="insp-tidy"
        phx-click="window_reset_positions"
        title="Bring all inspector windows back on-screen"
      >
        Tidy windows
      </button>
      <.inspector_window :for={w <- @windows} win={w} role={@role} />
    </div>
    """
  end

  attr :win, :map, required: true
  attr :role, :atom, required: true

  defp inspector_window(assigns) do
    ~H"""
    <section
      class="insp-window"
      id={"inspector-window-#{@win.id}"}
      style={"left:#{@win.x}px;top:#{@win.y}px;z-index:#{@win.z};"}
      phx-hook="WindowDrag"
      data-window-id={@win.id}
    >
      <%!-- Title bar: the drag handle (`.iw-title`, grabbed by the WindowDrag hook)
           plus the live freeze toggle and the close button. Dragging the bar moves
           the window client-side; the hook reports the final x/y on drop. --%>
      <header class="iw-bar" data-window-drag-handle>
        <span class="iw-title mono">
          {(@win.target && @win.target.label) || @win.label}
        </span>
        <span class="spacer"></span>
        <button
          :if={@win.target && @win.target.pid}
          type="button"
          class={["insp-freeze", (@win.frozen && "frozen") || "live"]}
          phx-click="window_freeze"
          phx-value-id={@win.id}
          title={
            if @win.frozen,
              do: "Frozen snapshot — click to track live",
              else: "Tracking live (subscribed to changes) — click to freeze a snapshot"
          }
        >
          <span class="iwf-dot"></span>{(@win.frozen && "frozen") || "live"}
        </button>
        <button
          type="button"
          class="iw-close"
          phx-click="window_close"
          phx-value-id={@win.id}
          title="Close window"
          aria-label="Close window"
        >
          ×
        </button>
      </header>
      <div class="iw-body">
        <%= if @win.target do %>
          <div class="insp-head">
            <div :if={length(@win.crumbs) > 1} class="insp-crumbs">
              <%= for {crumb, i} <- Enum.with_index(@win.crumbs) do %>
                <span :if={i > 0} class="sep">›</span>
                <span class="c" phx-click="window_crumb" phx-value-id={@win.id} phx-value-index={i}>
                  {crumb.label}
                </span>
              <% end %>
            </div>
            <div class="ps mono">
              Inspecting <strong>{@win.target.label}</strong>
              <span class="ps-header">{@win.target.header}</span>
            </div>
            <div class="proc-chips">
              <span class="chip">class <b>{@win.target.class_name}</b></span>
              <span :if={@win.target.pid} class="chip">
                pid <b>{@win.target.pid}</b>
              </span>
              <span :if={stat_status(@win.stats)} class="chip pid-stat">
                <span class="dot"></span>{stat_status(@win.stats)}
              </span>
              <span :if={not is_nil(stat_mailbox(@win.stats))} class="chip pid-stat">
                mailbox <b>{stat_mailbox(@win.stats)}</b>
              </span>
              <span :if={not is_nil(stat_reductions(@win.stats))} class="chip pid-stat">
                reductions <b>{stat_reductions(@win.stats)}</b>
              </span>
            </div>
          </div>
        <% end %>
        <div class="iw-content">
          <.notice
            :if={@win.error}
            variant={:warn}
            message={@win.error}
            dismiss_attrs={%{"phx-click" => "dismiss_window_error", "phx-value-id" => @win.id}}
          />
          <%= if @win.target && @win.rows != [] do %>
            <table
              id={"inspector-window-fields-#{@win.id}"}
              class="ivar-table"
              phx-hook="FieldFlash"
              data-flash-gen={@win.flash_gen}
            >
              <tbody>
                <tr
                  :for={{row, i} <- Enum.with_index(@win.rows)}
                  class={row.drillable && "drillable"}
                  phx-click={row.drillable && "window_drill"}
                  phx-value-id={row.drillable && @win.id}
                  phx-value-index={row.drillable && i}
                >
                  <td class="k">{row.name}</td>
                  <td class={["v", row.kind]} data-flash-key={row.name} data-flash-val={row.value}>
                    {row.value}
                  </td>
                  <td class="follow">
                    <span :if={row.drillable} class="follow-link">follow →</span>
                  </td>
                </tr>
              </tbody>
            </table>
          <% else %>
            <p :if={@win.target == nil && @win.error == nil} class="empty">
              Nothing to inspect.
            </p>
          <% end %>
          <%!-- Owner-only poke bar (BT-2492), per-window: pokeable only at a single
               named-binding crumb root (the same well-formedness gate the docked
               pane uses), and only for the owner role. --%>
          <div
            :if={@win.target && @win.target.pid && @role == :owner && window_pokeable?(@win)}
            class="poke"
          >
            <div class="poke-label">Send a message to {@win.target.label}</div>
            <form class="poke-row" phx-submit="window_poke" phx-value-id={@win.id}>
              <span class="poke-recv mono">‹recv›</span>
              <input
                class="field mono"
                name="message"
                autocomplete="off"
                placeholder="increment   ·   incrementBy: 10"
              />
              <button type="submit" class="btn">Send</button>
            </form>
            <div :if={@win.poke_result} class="poke-out ok mono">{@win.poke_result}</div>
            <div :if={@win.poke_error} class="poke-out warn mono">{@win.poke_error}</div>
          </div>
        </div>
      </div>
    </section>
    """
  end

  # Whether a floating window's head can be poked: a pid-backed object at the
  # inspection root with a valid-identifier binding name (the same contract as the
  # docked `pokeable?/1`, scoped to one window's crumbs).
  defp window_pokeable?(%{crumbs: [%{label: label}]}) when is_binary(label),
    do: valid_receiver?(label)

  defp window_pokeable?(_w), do: false
end

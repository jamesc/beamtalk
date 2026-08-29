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
    * `KeyboardShortcuts` — maps Cmd/Ctrl chords (and bare keys like Escape)
      to actions from a `data-shortcuts` JSON map. The method-editor form binds
      ⌘S → `submit` (request-submits the form so class/selector/source ride the
      normal `save_method` `phx-submit`); the Workspace dock binds ⌘D/⌘P/⌘I →
      `submit:<action>` (BT-2490), riding the eval form's hidden `action` field;
      the cockpit root binds Esc/⌘W → `tab_close_active` (close the focused
      editor tab — ⌘W only reaches the page in the desktop shell, browsers
      reserve it) and ⌘/ → `toggle_doc` (documentation disclosure).

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
  alias BtAttachWeb.ClassTree
  alias BtAttachWeb.Live.Dock
  alias BtAttachWeb.Live.FacadeError
  alias BtAttachWeb.Live.Inspector
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.RequestContext
  alias BtAttachWeb.Live.SystemBrowser

  # All RBAC-relevant workspace ops go through the curated facade (ADR 0091
  # Decision 3) — never a raw Workspace/:rpc call from an event handler. Pure
  # transport/display/lifecycle helpers (connect, render_term, session start)
  # go through the injectable workspace client so the mount and session-start
  # paths are testable without a live node (BT-2554). `ctx/1` carries the
  # request identity the facade audits / RBAC gates on (BT-2421).
  defp ctx(socket), do: RequestContext.build(socket)

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
      # ADR 0105 Phase 1 (BT-2779): subscribe to the reload-check push stream
      # so a `ReloadCheckCompleted` announcement (a re-check ran, or a
      # caller's stale findings were cleared) updates the reload-findings
      # panel live. Best-effort, same rationale as `subscribe_classes`.
      subscribe_reload_check_best_effort(socket)

      socket
      |> assign(:connected, true)
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
        Workspace.unsubscribe_reload_check(self())
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

  # ADR 0105 Phase 1 (BT-2779): best-effort subscribe to the reload-check push
  # stream. Same rationale as `subscribe_classes_best_effort/1` — an older
  # workspace without the wiring, or a transient dist hiccup, degrades the
  # reload-findings panel to whatever `initial_reload_findings/1` read at
  # mount (stale until next remount) rather than failing the whole session.
  defp subscribe_reload_check_best_effort(socket) do
    case Facade.dispatch(:subscribe_reload_check, %{pid: self()}, ctx(socket)) do
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
    case Facade.dispatch(:reload_findings, %{}, ctx(socket)) do
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
    {:noreply, MethodEditor.open_method_tab(socket, class, "instance", selector)}
  end

  # Fallback clauses for the guarded test handlers: a crafted WebSocket message
  # with a missing / non-binary `class`/`selector` must be ignored, not crash the
  # socket on a FunctionClauseError before RBAC is reached (matching `save_method`,
  # `revert`, `browser_select_class`, etc.).
  def handle_event("run_test_class", _params, socket), do: {:noreply, socket}
  def handle_event("open_test_method", _params, socket), do: {:noreply, socket}

  # Workspace dock events (BT-3295, epic BT-3290): the eval/Workspace tab,
  # REPL tab, Transcript tab (push-only, no events), and Changes/Git tabs all
  # delegate to `BtAttachWeb.Live.Dock`, extracted out of this module so its
  # `handle_event/3`/`handle_async/3` branches are directly unit-testable.
  # See that module's docs for the full per-event behaviour (each clause used
  # to run here).
  @dock_events ~w(
    dock_tab eval select_workspace repl_eval repl_history_prev
    repl_history_next repl_inspect git_refresh git_diff git_stage
    git_unstage git_revert git_commit toggle_change_diff revert flush
    flush_destructive
  )

  @impl true
  def handle_event(event, params, socket) when event in @dock_events do
    Dock.handle_event(event, params, socket)
  end

  # Inspector + object-window events (BT-3291, epic BT-3290): the docked
  # Inspector's inspect/drill/crumb/freeze/poke navigation and the floating
  # window overlay's own copies of the same actions all delegate to
  # `BtAttachWeb.Live.Inspector`, extracted out of this module so they're
  # directly unit-testable. See that module's `handle_event/3` for the full
  # per-event behaviour (each clause below is exactly what used to run here).
  #
  # Unlike `@dock_events`/`@method_editor_events` below, this is NOT a
  # separately hand-maintained list of event names: BT-3291 left two
  # independent copies of the same 16 strings (this attribute, and
  # `Inspector.handle_event/3`'s own clause heads) with nothing tying them
  # together, so a name added/renamed/removed on one side without the other
  # failed silently at runtime instead of at compile/test time (BT-3301).
  # Reading `Inspector.__inspector_events__/0` here — the module that owns
  # the clauses — eliminates the second copy entirely.
  @inspector_events Inspector.__inspector_events__()

  @impl true
  def handle_event(event, params, socket) when event in @inspector_events do
    Inspector.handle_event(event, params, socket)
  end

  # Tabbed Method Editor events (BT-3296, epic BT-3290): the tab strip,
  # compile (⌘S)/save routing, dirty tracking, doc-block disclosure, and the
  # native (`.erl`) editable-source pane all delegate to
  # `BtAttachWeb.Live.MethodEditor`, extracted out of this module so its
  # `handle_event/3` branches and the tab data model they drive are directly
  # unit-testable. See that module's docs for the full per-event behaviour
  # (each clause used to run here).
  @method_editor_events ~w(
    tab_select tab_close tab_close_active open_definition edit_source
    save_method native_source native_save dismiss_native_error
    dismiss_native_module_error toggle_doc select_source
  )

  @impl true
  def handle_event(event, params, socket) when event in @method_editor_events do
    MethodEditor.handle_event(event, params, socket)
  end

  # System Browser events (BT-3297, epic BT-3290): the class tree, instance/
  # class side toggle, protocol + method list, method source opening, and the
  # source-navigation affordances (hover/goto-definition/senders/implementors)
  # all delegate to `BtAttachWeb.Live.SystemBrowser`, extracted out of this
  # module so its `handle_event/3` clauses are directly unit-testable. See
  # that module's docs for the full per-event behaviour (each clause used to
  # run here). Reads its event list from `__system_browser_events__/0` —
  # mirroring the BT-3301 fix that keeps this list from becoming a second,
  # hand-maintained copy of the event names (unlike `@dock_events`/
  # `@method_editor_events` above).
  @system_browser_events SystemBrowser.__system_browser_events__()

  @impl true
  def handle_event(event, params, socket) when event in @system_browser_events do
    SystemBrowser.handle_event(event, params, socket)
  end

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
  # `toggle_browser`/`close_browser` (the System Browser panel) delegate to
  # `BtAttachWeb.Live.SystemBrowser` via the `@system_browser_events` guard
  # clause above.
  def handle_event("toggle_inspector", _params, socket) do
    {:noreply, assign(socket, show_inspector: !socket.assigns.show_inspector)}
  end

  def handle_event("toggle_dock", _params, socket) do
    {:noreply, assign(socket, show_dock: !socket.assigns.show_dock)}
  end

  # ── method editor (Wave 3, write-surface ADR 0082) ──────────────────────────

  # Remove the active method tab's method from its class (ADR 0112 Phase 4,
  # BT-3189). Wired the same way Save is: an owner-only editor action that
  # drives a write-surface primitive and refreshes the Changes pane. Unlike
  # `save_method`, there is no dedicated workspace-side op — this just builds
  # `Class removeSelector: #selector` (or `Class class removeSelector:
  # #selector` for a class-side tab) and submits it through the existing
  # `evaluate` op, exactly like the REPL `:remove-method` meta-command, the
  # MCP `remove_method` tool, and the LSP `beamtalk.removeMethod` command
  # (ADR 0112's "Beamtalk-level surface to add" — no new dispatcher op).
  def handle_event("remove_method", _params, %{assigns: %{role: :owner}} = socket) do
    {:noreply, remove_active_method(socket)}
  end

  # Non-owner (Observer) or a crafted event with no matching role: a no-op —
  # the button is rendered only for `:owner`, mirroring `new_method` above.
  def handle_event("remove_method", _params, socket), do: {:noreply, socket}

  # Remove the active class-definition tab's class from the running system
  # (ADR 0113 Phase 4, BT-3210). Wired the same way "Remove Method" is above
  # (ADR 0112 Phase 4, BT-3189): an owner-only editor action, `data-confirm`
  # -gated in the template, that drives a write-surface primitive and
  # refreshes the Changes pane. There is no dedicated workspace-side op —
  # this just builds `Class removeFromSystem` and submits it through the
  # existing `evaluate` op, exactly like the REPL `:remove-class` meta-
  # command and the MCP `remove_class` tool (ADR 0113's "Surface" table).
  #
  # This is the memory-mutating gesture only, matching `autoflush: true`'s
  # existing "the memory step is not gated" behaviour for ordinary patches
  # (ADR 0113 Surface, browser row) — the resulting `remove-class` entry
  # does NOT get silently written to disk by autoflush; it renders in the
  # Changes pane with a distinct destructive-dirty affordance requiring its
  # own explicit "Delete file" click (`confirmDestructive: true`) to reach
  # disk. Two gestures for two genuinely separate decisions, same shape as
  # the REPL's two independently-confirmed `:remove-class` / `:flush-
  # destructive` commands.
  def handle_event("remove_class", _params, %{assigns: %{role: :owner}} = socket) do
    {:noreply, remove_active_class(socket)}
  end

  # Non-owner (Observer) or a crafted event with no matching role: a no-op —
  # the button is rendered only for `:owner`, mirroring `remove_method` above.
  def handle_event("remove_class", _params, socket), do: {:noreply, socket}

  # Open the Rename modal for the active tab (ADR 0114 Phase 5, BT-3277): a
  # `:def` tab renames its class (`renameTo:`), an existing `:method` tab
  # renames its selector (`renameSelector:to:`). Mirrors `remove_class`/
  # `remove_method` above in reading the target from the active tab rather
  # than trusting client-supplied params, but — unlike those, which act
  # immediately — this only opens the modal; the actual rename waits for
  # `rename_submit` once the owner has typed the new name.
  def handle_event("open_rename", _params, %{assigns: %{role: :owner}} = socket) do
    {:noreply, open_rename(socket)}
  end

  # Non-owner (Observer) or a crafted event with no matching role: a no-op —
  # the buttons opening this modal are rendered only for `:owner`.
  def handle_event("open_rename", _params, socket), do: {:noreply, socket}

  # Dismiss the Rename modal without renaming anything — mirrors
  # `close_new_class` (Escape / click-away / the modal's own × button all
  # route here).
  def handle_event("close_rename", _params, socket) do
    {:noreply, assign(socket, rename_open: false, rename_error: nil)}
  end

  # Submit the Rename modal's new name (ADR 0114 Phase 5, BT-3277). Dispatches
  # to `renameTo:` or `renameSelector:to:` depending on which kind of rename
  # was opened; a rejected submit re-renders the modal with the in-flight
  # value and an inline error, mirroring `new_class`'s validation-failure
  # shape.
  def handle_event(
        "rename_submit",
        %{"new_name" => new_name},
        %{assigns: %{role: :owner}} = socket
      )
      when is_binary(new_name) do
    {:noreply, submit_rename(socket, new_name)}
  end

  # Non-owner (Observer) or a crafted event with a missing/non-binary name: a
  # no-op — the form is rendered only for `:owner` while the modal is open.
  def handle_event("rename_submit", _params, socket), do: {:noreply, socket}

  # ── tabbed method editor (BT-2494, epic BT-2482 Phase 2) ────────────────────
  #
  # `tab_select`/`tab_close`/`tab_close_active`/`open_definition`/`edit_source`/
  # `save_method`/`native_source`/`native_save`/`dismiss_native_error`/
  # `dismiss_native_module_error`/`toggle_doc`/`select_source` all delegate to
  # `BtAttachWeb.Live.MethodEditor` (BT-3296, epic BT-3290) — see the
  # `@method_editor_events` guard clause above.
  #
  # `browser_open_definition`/`browser_open_native`/`browser_mode`/
  # `browser_open_native_module`/`browser_jump_native` all delegate to
  # `BtAttachWeb.Live.SystemBrowser` (BT-3297, epic BT-3290) — see the
  # `@system_browser_events` guard clause above.

  # Open a blank "new method" tab for the *selected* class (the System Browser's
  # "new method" entry). A new-method tab is a `:method` tab with no selector yet —
  # the only editor surface that still shows a selector input (the breadcrumb can't
  # name a selector that doesn't exist), so the author types the selector + body.
  # The starter tab used to fill this role on startup; it now opens on demand only.
  def handle_event("new_method", %{"class" => class}, %{assigns: %{role: :owner}} = socket)
      when is_binary(class) and class != "" do
    # Author on whichever side the browser is showing (instance/class), so "new
    # method" while viewing the class side opens a class-side tab.
    {:noreply, MethodEditor.open_new_method(socket, class, socket.assigns.browser_side)}
  end

  # Non-owner (Observer) or malformed payload: a no-op. Authoring is owner-only —
  # the entry is rendered only for `:owner`, and a crafted event from a read-only
  # role must not even open a (non-savable) scratch tab in their strip.
  def handle_event("new_method", _params, socket), do: {:noreply, socket}

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
    {:noreply, socket |> SystemBrowser.open_class(class) |> close_omni()}
  end

  def handle_event(
        "omni_open",
        %{"kind" => "selector", "class" => class, "side" => side, "selector" => selector},
        socket
      )
      when is_binary(class) and is_binary(side) and is_binary(selector) do
    {:noreply, socket |> MethodEditor.open_method_tab(class, side, selector) |> close_omni()}
  end

  def handle_event("omni_open", _params, socket), do: {:noreply, close_omni(socket)}

  # Dismiss the omni-search popover (Escape / blur, reported by the hook) without
  # opening anything.
  def handle_event("omni_close", _params, socket), do: {:noreply, close_omni(socket)}

  # `senders`/`implementors`/`native_callers`/`required_methods`/
  # `conforming_classes`/`nav_open_class`/`nav_required_open`/`nav_open`/
  # `nav_close`/`goto_definition` (the Senders/Implementors/protocol-action
  # popover + Ctrl/Cmd-click go-to-definition, BT-2495/BT-2639/BT-2666/BT-2669)
  # all delegate to `BtAttachWeb.Live.SystemBrowser` (BT-3297, epic BT-3290) —
  # see the `@system_browser_events` guard clause above.

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

  # `dismiss_nav_error` (dismiss the error inside the Senders/Implementors
  # popover without closing it) delegates to `BtAttachWeb.Live.SystemBrowser`
  # via the `@system_browser_events` guard clause above.

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

  # ADR 0105 Phase 1 (BT-2779): a reload-induced re-check outcome. Unlike the
  # `bindings`/`classes` streams (refresh triggers only — the LiveView
  # re-pulls the data on the signal), this announcement already carries the
  # new state (`changedClass` + `checkedOwners` + `findings`), so no extra
  # round-trip is needed: apply clearing-by-replacement (ADR 0105 §Mechanism
  # step 4) directly.
  #
  # The rejection predicate is NOT simply "owner in checked_owners" — that
  # would let one changed class's reload silently discard a *different*
  # changed class's still-valid finding for the same caller (a caller
  # broken by two independently-reloading dependencies must keep both until
  # each is fixed on its own schedule; see
  # `beamtalk_workspace_findings_store`'s moduledoc). The server applies two
  # different clears depending on WHY an owner is in `checked_owners`:
  #   * `owner == changed_class` — this owner's OWN source just changed, so
  #     the server unconditionally full-wipes EVERY origin for it
  #     (`clear_owner/1`), not just the `changed_class` one.
  #   * any other owner — only THIS changed class's contribution was
  #     scoped-replaced (`put_owner_origin/3`); a different changed class's
  #     finding for the same owner is untouched.
  # Mirroring that: reject a cached finding when its owner is in
  # `checked_owners` AND (its own owner IS the changed class — full wipe —
  # OR its `changed_class` matches this event's — scoped wipe). An owner
  # listed with no findings (a clean re-check, or a plain edit that cleared
  # its own stale findings) is exactly how a stale row disappears from the
  # panel without anyone touching it.
  def handle_info(
        {:beamtalk_announcement, _sub_ref, :ReloadCheckCompleted, _handler, event},
        socket
      ) do
    {changed_class, checked_owners, findings} = Workspace.normalize_reload_event(event)

    updated =
      socket.assigns.reload_findings
      |> Enum.reject(fn f ->
        f.owner in checked_owners and
          (f.owner == changed_class or f.changed_class == changed_class)
      end)
      |> Kernel.++(findings)

    {:noreply, assign(socket, :reload_findings, updated)}
  end

  # BT-2600: the coalesced source refresh fired by a `ClassLoaded`/`ClassRemoved`
  # burst — re-pull the source-dependent surfaces ONCE for the whole burst, then
  # clear the pending flag so the next burst schedules afresh.
  def handle_info(:do_source_refresh, socket) do
    socket =
      socket
      |> assign(source_refresh_pending: false)
      |> MethodEditor.refresh_after_source_change()
      |> mark_source_loaded()
      # BT-2588: this is the one place `:save_echo_pending` is cleared — see
      # `resync_active_tab/2`'s docs for why it must NOT self-clear (a
      # synchronous caller with more than one `resync_active_tab/2` call in its
      # own pipeline, like `git_revert_event/2`, would otherwise spend the flag
      # before the call that decides the rendered state ever sees it).
      |> assign(:save_echo_pending, false)

    {:noreply, socket}
  end

  # Inspector/object-window live-tracking pushes (BT-3291, epic BT-3290):
  # the per-object change push and its coalesced-refresh follow-ups delegate
  # to `BtAttachWeb.Live.Inspector.handle_info/2` — see that module for the
  # full behaviour (each clause below is exactly what used to run here).
  def handle_info({:object_changed, _pid, _slots} = msg, socket),
    do: Inspector.handle_info(msg, socket)

  def handle_info(:do_object_refresh = msg, socket), do: Inspector.handle_info(msg, socket)

  def handle_info({:do_window_refresh, _pid} = msg, socket),
    do: Inspector.handle_info(msg, socket)

  def handle_info(_msg, socket), do: {:noreply, socket}

  # Git panel async load (BT-2590 S1, extracted BT-3295): the off-socket
  # `assign_git/1` `start_async` result forwards to
  # `BtAttachWeb.Live.Dock.handle_async/3` unchanged, mirroring the
  # `handle_event`/`handle_info` delegation above.
  @impl true
  def handle_async(:git_load, result, socket), do: Dock.handle_async(:git_load, result, socket)

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
      |> fold_mount_read(
        :source_loaded,
        result.browser_classes,
        &SystemBrowser.apply_browser_classes/2
      )
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
      for term <- Inspector.window_watched_terms(socket.assigns[:windows]) do
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
          SessionRegistry.stash_windows(token, Inspector.build_window_stash(socket))
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

  # `facade_error/1` moved to `BtAttachWeb.Live.FacadeError` (BT-3291) so
  # extracted panes (e.g. `BtAttachWeb.Live.Inspector`) render the same
  # facade-error copy instead of keeping their own.
  defp facade_error(reason), do: FacadeError.render(reason)

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

  # Normalise a client-supplied selection offset to a non-negative integer (or
  # nil). The CmEditor hook sends integer offsets, but the payload is
  # untrusted, so a missing / negative / non-integer value collapses to nil.
  # Public: shared by `select_source` here and `BtAttachWeb.Live.Dock`'s
  # `select_workspace` (BT-3295).
  def clamp_offset(n) when is_integer(n) and n >= 0, do: n
  def clamp_offset(_), do: nil

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

  # Remove the active method tab's method from its class (ADR 0112 Phase 4,
  # BT-3189). A new (unsaved) method tab has nothing to remove, and a crafted
  # event against a non-method tab is a graceful no-op — surfaced as a local
  # validation error rather than evaluating a malformed expression.
  defp remove_active_method(socket) do
    case MethodEditor.active_tab(socket.assigns) do
      %{kind: :method, new: true} ->
        status_error(socket, "This method hasn't been saved yet — nothing to remove.")

      %{kind: :method, class: class, side: side, selector: selector} = tab
      when is_binary(selector) and selector != "" ->
        remove_method(socket, tab, class, side, selector)

      _ ->
        status_error(socket, "Open an existing method to remove it.")
    end
  end

  # `Class removeSelector: #selector` (or `Class class removeSelector:
  # #selector` for a class-side tab), submitted through the same generic
  # `evaluate` op the REPL `:remove-method` meta-command, the MCP
  # `remove_method` tool, and the LSP `beamtalk.removeMethod` command all use —
  # no dedicated workspace-side op (ADR 0112's "Beamtalk-level surface to
  # add"). On success the tab is closed (its method no longer exists) and the
  # Changes pane refreshes, mirroring `save_method_body/5`'s success path; a
  # raised `selector_not_found` (or any other structured error) renders inline
  # via the shared status area.
  defp remove_method(socket, tab, class, side, selector) do
    receiver = if side == "class", do: "#{class} class", else: class
    expr = "#{receiver} removeSelector: ##{selector}"
    pid = socket.assigns[:session_pid]

    if not is_pid(pid) do
      status_error(socket, "not attached to workspace")
    else
      remove_method_eval(socket, tab, receiver, selector, expr, pid)
    end
  end

  defp remove_method_eval(socket, tab, receiver, selector, expr, pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, ctx(socket)) do
      {:ok, _term, _output, _warnings} ->
        # `close_tab/2` wipes `save_result`/`save_error` whenever the closed
        # tab was the active one — via `clear_active/1` if it was the last
        # open tab, or via `sync_active/2` when focus moves to the next
        # remaining tab — so the success message must be assigned AFTER
        # closing the tab, not before, or closing the active tab would
        # silently swallow it.
        socket
        |> MethodEditor.close_tab(tab.id)
        |> assign(
          save_result: "Removed #{selector} from #{receiver}",
          save_error: nil,
          flush_result: nil,
          flush_error: nil
        )
        |> assign_changes()

      {:error, reason, _output, _warnings} ->
        status_error(socket, Workspace.render_error(reason))

      {:error, reason} ->
        status_error(socket, facade_error(reason))
    end
  end

  # Remove the active class-definition tab's class from the system (ADR 0113
  # Phase 4, BT-3210). A `:def` tab always names an already-existing class
  # (unlike a `:method` tab, there is no "new, unsaved class" draft state —
  # class creation is a separate System Browser form, see the "NEW CLASS"
  # comment in the template below), so there is no `new: true` guard to
  # mirror `remove_active_method/1`'s; a crafted event against a non-`:def`
  # tab is still a graceful no-op, surfaced as a local validation error
  # rather than evaluating a malformed expression.
  defp remove_active_class(socket) do
    case MethodEditor.active_tab(socket.assigns) do
      %{kind: :def, class: class} = tab when is_binary(class) and class != "" ->
        remove_class(socket, tab, class)

      _ ->
        status_error(socket, "Open a class definition to remove it.")
    end
  end

  # `Class removeFromSystem`, submitted through the same generic `evaluate`
  # op the REPL `:remove-class` meta-command and the MCP `remove_class` tool
  # use — no dedicated workspace-side op (ADR 0113's "Surface" table: "every
  # surface constructs one of the Beamtalk expressions above and submits via
  # the existing `evaluate` op"). Memory-mutating only: this never flushes.
  defp remove_class(socket, tab, class) do
    expr = "#{class} removeFromSystem"
    pid = socket.assigns[:session_pid]

    if not is_pid(pid) do
      status_error(socket, "not attached to workspace")
    else
      remove_class_eval(socket, tab, class, expr, pid)
    end
  end

  defp remove_class_eval(socket, tab, class, expr, pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, ctx(socket)) do
      {:ok, _term, _output, _warnings} ->
        # `close_tab/2` wipes `save_result`/`save_error` whenever the closed
        # tab was the active one — mirrors `remove_method_eval/6`'s success
        # path, so the confirmation message must be assigned AFTER closing
        # the tab. The message names the still-open second step (ADR 0113's
        # two-gesture flow) so the owner isn't surprised the file survives
        # this click.
        socket
        |> MethodEditor.close_tab(tab.id)
        |> assign(
          save_result:
            "Removed #{class} from memory — not yet flushed to disk. Delete its file from the Changes pane to finish.",
          save_error: nil,
          flush_result: nil,
          flush_error: nil
        )
        |> assign_changes()

      {:error, reason, _output, _warnings} ->
        status_error(socket, Workspace.render_error(reason))

      {:error, reason} ->
        status_error(socket, facade_error(reason))
    end
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

  # Whether `name` is a bare PascalCase class-name identifier
  # (`^[A-Z][A-Za-z0-9_]*$`) — the shape the New Class modal enforces. Public:
  # `BtAttachWeb.Live.Dock`'s `flush_destructive/3` (BT-3295) validates a
  # client-controlled `class` value against this same rule before
  # interpolating it into a raw `evaluate` expression, so a crafted event
  # can't inject arbitrary source.
  def valid_class_name?(name), do: Regex.match?(@new_class_name_re, name)

  defp validate_new_class_name(socket, name) do
    cond do
      name == "" ->
        {:error, "Enter a class name to create a class."}

      not valid_class_name?(name) ->
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

  # Deliberately NOT a selector grammar (unary/keyword/binary, whitespace
  # rules, the lexer's exact binary-operator character set) — duplicating
  # that here would be exactly the un-enforced "keep in sync" copy
  # `docs/development/architecture-principles.md`'s Duplication section
  # warns against, since nothing would catch this regex drifting from
  # `crates/beamtalk-core/src/source_analysis/lexer.rs`'s
  # `is_binary_selector_char/1` if the language ever adds an operator
  # character. Instead this only guards the one property that actually
  # matters here: the value is embedded as `##{new_name}` inside a
  # textually-interpolated `evaluate` expression, so it must contain no
  # character that could end the symbol literal early or splice in a second
  # expression (whitespace, `#`, quotes, backslash). Anything that passes
  # this narrow check but isn't actually a well-formed selector fails the
  # same way any other malformed `evaluate` expression does — a normal
  # compiler error surfaced via `rename_method_eval/6`'s `{:error, ...}`
  # branch — so the compiler stays the sole source of truth for "is this a
  # valid selector", never re-implemented here.
  @selector_injection_re ~r/^[^[:space:]#"'\\]+$/

  # Open the Rename modal against the active tab (ADR 0114 Phase 5, BT-3277):
  # a `:def` tab renames its class, an existing (already-saved) `:method` tab
  # renames its selector — mirroring `remove_active_class`/`remove_active_method`'s
  # identical tab-kind dispatch, including `remove_active_method`'s `new: true`
  # guard (a brand-new, not-yet-compiled method has no selector to rename).
  # The target is captured into `rename_class`/`rename_side`/`rename_old_selector`
  # now, at open time, rather than re-read from the active tab on submit — so
  # switching tabs while the modal is open can't retarget the rename mid-flight.
  defp open_rename(socket) do
    case MethodEditor.active_tab(socket.assigns) do
      %{kind: :def, class: class} when is_binary(class) and class != "" ->
        assign(socket,
          rename_open: true,
          rename_kind: :class,
          rename_class: class,
          rename_side: nil,
          rename_old_selector: nil,
          rename_new_name: class,
          rename_error: nil
        )

      %{kind: :method, new: true} ->
        status_error(socket, "This method hasn't been saved yet — nothing to rename.")

      %{kind: :method, class: class, side: side, selector: selector}
      when is_binary(class) and class != "" and is_binary(selector) and selector != "" ->
        assign(socket,
          rename_open: true,
          rename_kind: :method,
          rename_class: class,
          rename_side: side,
          rename_old_selector: selector,
          rename_new_name: selector,
          rename_error: nil
        )

      _ ->
        status_error(socket, "Open a class definition or an existing method to rename it.")
    end
  end

  # Dispatch the Rename modal's submitted name to whichever primitive
  # `rename_kind` selected. A rejected submit keeps the modal open with the
  # in-flight value and an inline error (never the method editor's shared
  # `save_error`), mirroring `new_class/3`'s validation-failure shape.
  defp submit_rename(socket, new_name) do
    new_name = String.trim(new_name)

    case socket.assigns.rename_kind do
      :class ->
        rename_class(socket, socket.assigns.rename_class, new_name)

      :method ->
        rename_method(
          socket,
          socket.assigns.rename_class,
          socket.assigns.rename_side,
          socket.assigns.rename_old_selector,
          new_name
        )

      _ ->
        assign(socket, rename_open: false, rename_error: nil)
    end
  end

  # `OldClass renameTo: #NewName` (ADR 0114 Phase 2, `Behaviour>>renameTo:`,
  # BT-3278), submitted through the same generic `evaluate` op the REPL
  # `:rename-class` meta-command and the MCP `rename_class` tool use — no
  # dedicated workspace-side op (ADR 0114's "Surface" table, reusing ADR
  # 0113's "every surface constructs one of the Beamtalk expressions above
  # and submits via the existing `evaluate` op"). Memory-mutating only: this
  # never flushes — the resulting `rename-class` entry needs its own
  # confirmed "apply rename" click in the Changes pane to reach disk (ADR
  # 0114's two-gesture flow, reusing ADR 0113's `confirmDestructive` tier).
  #
  # `new_name` is validated locally against the same bare-PascalCase-identifier
  # shape the New Class modal enforces (`@new_class_name_re`) before it is
  # textually interpolated into the `evaluate` expression — this field is
  # owner-typed but still raw client input reaching a raw Beamtalk expression,
  # exactly the injection concern `flush_destructive/2`'s class-name check
  # guards against.
  defp rename_class(socket, old_name, new_name) do
    cond do
      new_name == "" ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_name,
          rename_error: "Enter a new class name."
        )

      not Regex.match?(@new_class_name_re, new_name) ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_name,
          rename_error:
            "Class name must be PascalCase — start with an uppercase letter, e.g. `Accumulator`."
        )

      class_name_taken?(socket, new_name) ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_name,
          rename_error: "A class named #{new_name} already exists."
        )

      true ->
        expr = "#{old_name} renameTo: ##{new_name}"
        pid = socket.assigns[:session_pid]

        if not is_pid(pid) do
          assign(socket,
            rename_open: true,
            rename_new_name: new_name,
            rename_error: "not attached to workspace"
          )
        else
          rename_class_eval(socket, old_name, new_name, expr, pid)
        end
    end
  end

  defp rename_class_eval(socket, old_name, new_name, expr, pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, ctx(socket)) do
      {:ok, _term, _output, _warnings} ->
        # The class's identity changed, so every tab keyed on the old name
        # (its own `:def` tab plus any open `:method` tabs) is stale — close
        # them all, mirroring `remove_class_eval/4`'s tab-closing success
        # path, then refresh the class tree (a new name to browse) and the
        # Changes pane (the new `rename-class` entry). `open_definition/2`
        # re-syncs the active-tab editor assigns (clearing `save_result`,
        # same note as `dispatch_new_class/5` above), so the success status is
        # assigned AFTER it opens the renamed class's definition tab.
        socket
        |> close_tabs_for_class(old_name)
        |> SystemBrowser.assign_browser_classes()
        |> assign_changes()
        |> MethodEditor.open_definition(new_name)
        |> reselect_renamed_class(old_name, new_name)
        |> assign(
          rename_open: false,
          rename_error: nil,
          save_result:
            "Renamed #{old_name} to #{new_name} — not yet flushed to disk. Apply the rename from the Changes pane to finish.",
          save_error: nil,
          flush_result: nil,
          flush_error: nil
        )

      {:error, reason, _output, _warnings} ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_name,
          rename_error: Workspace.render_error(reason)
        )

      {:error, reason} ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_name,
          rename_error: facade_error(reason)
        )
    end
  end

  # Only follow the rename into `selected_class` when the System Browser
  # tree's own current selection WAS the renamed class (or nothing was
  # selected) — unlike `dispatch_new_class/5`'s unconditional select-the-
  # new-class (there's no prior selection to conflict with when creating a
  # class), a rename can be triggered from an open `:def` tab while the
  # tree has a DIFFERENT, unrelated class selected. Reassigning
  # unconditionally would move the tree highlight to the renamed class
  # while leaving `browser_protocols`/`browser_categories` showing the
  # untouched previously-selected class — the same "ghost selection"
  # mismatch `selected_class_visible?/1` (BT-2597) exists to avoid for the
  # browser-source-filter case. Leaving an unrelated selection alone is
  # simpler and correct here; there's no stale-name problem to fix in that
  # case, since the unrelated class's own identity didn't change.
  defp reselect_renamed_class(socket, old_name, new_name) do
    if socket.assigns.selected_class in [old_name, nil] do
      assign(socket, selected_class: new_name, selected_protocol: nil)
    else
      socket
    end
  end

  # `Class renameSelector: #old to: #new` (or `Class class renameSelector: #old
  # to: #new` for a class-side tab) — `Behaviour>>renameSelector:to:` (ADR
  # 0114 Phase 3, BT-3279), submitted through the same generic `evaluate` op
  # the REPL `:rename-method` meta-command and the MCP `rename_method` tool
  # use. Memory-mutating only, same two-gesture flow as `rename_class/3`
  # above: reaching disk needs the Changes pane's "apply rename" click.
  #
  # `new_name` is only checked against `@selector_injection_re` (no
  # whitespace/`#`/quotes/backslash) before interpolation — see that
  # attribute's own comment for why this stops short of validating full
  # selector syntax. A value that passes this check but isn't actually a
  # well-formed selector, or collides with an already-defined local
  # selector, is refused server-side by `renameSelector:to:` itself
  # (mirroring `removeSelector:`'s existing error surface) and surfaces
  # through `rename_method_eval/6`'s ordinary error branch.
  defp rename_method(socket, class, side, old_selector, new_name) do
    cond do
      new_name == "" ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_name,
          rename_error: "Enter a new selector."
        )

      not Regex.match?(@selector_injection_re, new_name) ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_name,
          rename_error: "Selector cannot contain spaces, #, quotes, or backslashes."
        )

      true ->
        receiver = if side == "class", do: "#{class} class", else: class
        expr = "#{receiver} renameSelector: ##{old_selector} to: ##{new_name}"
        pid = socket.assigns[:session_pid]

        if not is_pid(pid) do
          assign(socket,
            rename_open: true,
            rename_new_name: new_name,
            rename_error: "not attached to workspace"
          )
        else
          rename_method_eval(socket, class, receiver, old_selector, new_name, expr, pid)
        end
    end
  end

  defp rename_method_eval(socket, class, receiver, old_selector, new_selector, expr, pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, ctx(socket)) do
      {:ok, _term, _output, _warnings} ->
        # The selector changed, so the active method tab (keyed on the old
        # selector) is stale — close it, mirroring `remove_method_eval/6`'s
        # tab-closing success path.
        socket
        |> close_active_tab_if(
          &match?(%{kind: :method, class: ^class, selector: ^old_selector}, &1)
        )
        |> assign(
          rename_open: false,
          rename_error: nil,
          save_result:
            "Renamed #{receiver} #{old_selector} to #{new_selector} — not yet flushed to disk. Apply the rename from the Changes pane to finish.",
          save_error: nil,
          flush_result: nil,
          flush_error: nil
        )
        |> assign_changes()

      {:error, reason, _output, _warnings} ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_selector,
          rename_error: Workspace.render_error(reason)
        )

      {:error, reason} ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_selector,
          rename_error: facade_error(reason)
        )
    end
  end

  # Close every open tab (def or method) belonging to `class` — used after a
  # successful class rename, since every one of them is keyed on the
  # now-stale old name. Iterates `close_tab/2` (rather than a bulk filter)
  # so each close runs its normal active-tab-reassignment bookkeeping.
  defp close_tabs_for_class(socket, class) do
    socket.assigns.tabs
    |> Enum.filter(&(&1.kind in [:def, :method] and Map.get(&1, :class) == class))
    |> Enum.reduce(socket, fn tab, acc -> MethodEditor.close_tab(acc, tab.id) end)
  end

  # Close the active tab only when `pred` matches it — used after a
  # successful method rename so a rename triggered from a *different* tab
  # (not reachable today, since `open_rename/1` only ever targets the active
  # tab, but kept explicit rather than assumed) never closes the wrong one.
  defp close_active_tab_if(socket, pred) do
    case MethodEditor.active_tab(socket.assigns) do
      %{} = tab -> if pred.(tab), do: MethodEditor.close_tab(socket, tab.id), else: socket
      _ -> socket
    end
  end

  defp dispatch_new_class(socket, name, superclass, source, path) do
    case Facade.dispatch(:new_class, %{source: source, path: path}, ctx(socket)) do
      {:ok, created_path} ->
        socket
        |> SystemBrowser.assign_browser_classes()
        |> assign_changes()
        # Open + select the NEW class (`name`), not the superclass: the def tab
        # focuses it and the tree highlights it (BT-2645). `open_definition`
        # re-syncs the active-tab editor assigns (clearing `save_result`), so the
        # success status is assigned *after* it — otherwise the "Created …" banner
        # would be wiped by the very tab it opens.
        |> MethodEditor.open_definition(name)
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

  # Human-readable Kind/Side label for one Changes-pane row (BT-3195): before
  # this, the table had no column distinguishing an instance-side patch from a
  # class-side patch/removal of the same selector, so the two rows — both
  # possible simultaneously since BT-3187's `(class, selector, side)`
  # shadow-key fix — were visually identical apart from their (also-identical)
  # Class/Selector/Intent/Flushable/Author cells. `kind: "instance"`/`"class"`
  # already implies its own side, so only `"remove-method"` (whose kind alone
  # doesn't say which method table it targets) appends the explicit `side`;
  # an unrecognised future kind falls back to the raw value rather than
  # crashing, matching the ChangeLog's own `kind() :: … | unknown` fallback
  # (`beamtalk_workspace_changelog.erl`).
  defp change_kind_label(%{kind: "remove-method"} = c), do: "remove (#{c[:side] || "?"})"
  defp change_kind_label(%{kind: "new-class"}), do: "new class"
  # ADR 0082 extension (BT-3248): redefining an *existing* class's whole
  # definition (the cockpit `:def` tab's "Compile" action) — kept distinct
  # from "new class" above so the dock doesn't misreport a redefinition as a
  # brand-new class.
  defp change_kind_label(%{kind: "class-def"}), do: "class definition"
  # ADR 0113 Phase 4 (BT-3210): a Tier-2 (destructive, file-deleting)
  # `removeFromSystem` entry — kept a distinct label from "remove (...)"
  # above (Tier 1's `removeSelector:`) since the two carry very different
  # flush-time consequences; the row's own `.destructive-badge` (see the
  # template) is the primary visual cue, this is the textual one.
  defp change_kind_label(%{kind: "remove-class"}), do: "remove class"
  # ADR 0114 Phase 5 (BT-3277): the sibling Tier-2 kinds to "remove class"
  # above — a `renameTo:`/`renameSelector:to:` entry, which also needs its
  # own confirmed flush to reach disk (same `.destructive-row`/`.destructive-
  # badge` marker as remove-class, extended in the template below).
  defp change_kind_label(%{kind: "rename-class"}), do: "rename class"
  defp change_kind_label(%{kind: "rename-method"} = c), do: "rename (#{c[:side] || "?"})"
  defp change_kind_label(%{kind: kind}), do: kind

  # Set the active error line, clearing the other three status assigns so only
  # the most recent New Class / revert outcome shows in the shared status area
  # (BT-2293). Keeps the validation/error branches from leaving a stale flush
  # banner visible — the success branches already clear all four inline.
  # Public: shared with `BtAttachWeb.Live.Dock`'s revert/git-revert paths
  # (BT-3295).
  def status_error(socket, message) do
    assign(socket,
      save_result: nil,
      save_error: message,
      flush_result: nil,
      flush_error: nil
    )
  end

  # The `data-confirm` prompt for the "apply rename" button (ADR 0114 Phase
  # 5, BT-3277) — mirrors "delete file"'s prompt above, worded per kind since
  # a class rename also moves the `.bt` file while a method rename only
  # rewrites spans within existing files.
  defp rename_flush_confirm_message(%{kind: "rename-class", class: class}) do
    "Rename to #{class} on disk? This moves and edits one or more files and cannot be undone from here (use \"revert\" first if you change your mind)."
  end

  defp rename_flush_confirm_message(%{kind: "rename-method"} = c) do
    "Rename #{c.selector} on #{c.class}#{if c[:side] == "class", do: " class", else: ""} on disk? This edits one or more files and cannot be undone from here (use \"revert\" first if you change your mind)."
  end

  # Refresh the git panel only when it is the active dock tab. A flush that
  # happens while the user is on another tab leaves git untouched (it reloads
  # lazily on next open), so the common edit loop never pays for an extra git
  # shell-out it can't see. Used by the flush path, which always changes disk.
  # Public: shared with `BtAttachWeb.Live.Dock`'s flush paths (BT-3295) — the
  # git panel's own `assign_git/1` now lives there.
  def maybe_refresh_git(socket) do
    if socket.assigns.dock_tab == "git", do: Dock.assign_git(socket), else: socket
  end

  # BT-2590 (S2): refresh the git panel after a *save* only when autoflush is on.
  # With autoflush off a per-method save patches the live image only — the on-disk
  # working tree is unchanged, so the git shell-out would return an identical
  # result and is pure waste. When autoflush is on the save wrote through to disk,
  # so the panel must reflect it (gated, as ever, on the Git tab being active).
  # Public: `BtAttachWeb.Live.MethodEditor`'s save-definition path (BT-3296)
  # calls it directly.
  def maybe_refresh_git_after_save(socket) do
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

  # Read the active ChangeLog ("Workspace changes", ADR 0082) and assign display
  # rows. A workspace that is unreachable or returns an unexpected shape renders an
  # error rather than crashing the pane.
  #
  # Split into the off-socket read (`read_changes/1`) + the pure on-socket fold
  # (`apply_changes/2`) so the mount-time async load (BT-2591) and the post-action
  # refresh callers share one fold. The sync helper keeps its old signature.
  # Public: shared with `BtAttachWeb.Live.Dock`'s eval/REPL/Changes/git paths
  # (BT-3295).
  def assign_changes(socket), do: apply_changes(socket, read_changes(socket))

  # The raw `:changes` dispatch result — runs off the LiveView process in the
  # mount-load task, so it captures only `ctx`, never `socket`.
  defp read_changes(socket), do: Facade.dispatch(:changes, %{}, ctx(socket))

  # Fold a completed `:changes` read into the socket. Pure (no dispatch); shared by
  # `handle_async(:mount_load, …)` and the sync refresh path.
  defp apply_changes(socket, rows) when is_list(rows) do
    # Prune expanded-diff carets for rows that have left @changes (flush /
    # revert), so a re-saved method doesn't re-appear already-expanded. Keyed
    # by `{class, selector, side}` (BT-3195), matching the toggle handler and
    # the template's `expanded` lookup below.
    live_keys = MapSet.new(rows, &{&1.class, &1.selector, &1[:side]})
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

  # ADR 0105 Phase 1 (BT-2779): flatten `reload_findings` (one entry per
  # finding, each carrying a list of call sites) into one row per site, for
  # the Reload Checks table. A finding with no recorded sites (defensive —
  # `beamtalk_recheck` always attaches at least the xref sites it found the
  # finding against, but a malformed push should still render *something*
  # rather than silently dropping the row) still renders one row with
  # `method`/`line` as `nil`, which the template shows as `—`.
  defp reload_finding_rows(findings) do
    Enum.flat_map(findings, fn f ->
      case f.sites do
        [] ->
          [
            %{
              owner: f.owner,
              method: nil,
              line: nil,
              severity: f.severity,
              message: f.message,
              note: f.note
            }
          ]

        sites ->
          Enum.map(sites, fn site ->
            %{
              owner: f.owner,
              method: site.method,
              line: site.line,
              severity: f.severity,
              message: f.message,
              note: f.note
            }
          end)
      end
    end)
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

  # The coalescing window (ms): independently tunable from the Inspector's own
  # `{:object_changed, …}` debounce (`BtAttachWeb.Live.Inspector`) even though
  # both currently use the same 60ms value — the two features debounce
  # unrelated push streams.
  defp schedule_source_refresh(socket) do
    Process.send_after(self(), :do_source_refresh, 60)
    assign(socket, source_refresh_pending: true)
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
  # Public: `BtAttachWeb.Live.Dock`'s `ensure_test_classes/1` (BT-3295, the
  # `dock_tab`/`:test` meta-command lazy-load) calls it directly — the Tests
  # pane's own events haven't been extracted out of this module yet.
  def discover_test_classes(socket, keep_error? \\ false) do
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

  # ── navigation aids: omni search (BT-2495) ──────────────────────────────────
  #
  # The System Browser data source, protocol/category loaders, and the
  # senders/implementors/go-to-definition navigation aids below all now live
  # in `BtAttachWeb.Live.SystemBrowser` (BT-3297, epic BT-3290).

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
  # RBAC denial yields an empty index rather than crashing the search. Public:
  # `BtAttachWeb.Live.Dock`'s REPL `:help <Class>` meta-command (BT-3295)
  # calls it directly — the System Browser hasn't been extracted out of this
  # module yet (BT-3297).
  def symbol_rows(socket) do
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

  # The System Browser class/protocol/category data source, the native-source
  # panes, the senders/implementors/go-to-definition navigation aids, and the
  # class-tree/method-list view helpers all now live in
  # `BtAttachWeb.Live.SystemBrowser` (BT-3297, epic BT-3290); `open_class/2`
  # there is the "point the System Browser at this class" primitive this
  # module's `omni_open` handler and `BtAttachWeb.Live.Dock`'s REPL `:help`
  # cross-call.
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
          kind: Inspector.term_kind(term)
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
      <div class="panel-head"><span class="panel-title">Tweaks</span></div>
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
  # BT-2903 (ADR 0108 Phase 8): the third panel mode's data — every loaded
  # package's declared `type` aliases (AliasRow: name, expansion, doc,
  # source_file, internal, package, source_origin). Already fully filtered
  # server-side (seeding-boundary exclusion), so there is no companion
  # client-side filter attr the way `native_source` is for Native.
  attr :browser_type_aliases, :list, default: []

  defp system_browser_classes(assigns) do
    # BT-2557: filter the rows once, up front, so both the hierarchy and category
    # views (and the empty-state check) render the same source-scoped set.
    # BT-2656: the Native browser gets its own origin-scoped set (`visible_modules`).
    assigns =
      assigns
      |> assign(
        :visible_classes,
        SystemBrowser.filter_by_source(assigns.browser_classes, assigns.browser_source)
      )
      |> assign(
        :visible_modules,
        SystemBrowser.filter_by_source(assigns.browser_native_modules, assigns.native_source)
      )

    ~H"""
    <div id="system-browser" class="panel">
      <div class="panel-head">
        <span class="panel-title">System Browser</span> <span class="spacer"></span>
        <%!-- BT-2656: Classes | Native panel-mode toggle. Native (Erlang) modules
             are a distinct namespace from Beamtalk classes, so they live in their
             own scrollable, filterable browser rather than a collapsed in-tree
             section. Selecting "Native" replaces the class tree body with the
             native-module list (its own origin filter + count). --%>
        <%!-- BT-3256: icon-only buttons (the seg's text labels were part of the
             ~119px horizontal deficit at the 286px default — see the BT-3247
             follow-up comment on `.seg button` above). Each button keeps its
             full-word `aria-label`/`title` so the meaning that used to be the
             visible label is still available to screen readers and on hover;
             only the visible glyph changes. --%>
        <div class="seg seg-icon" role="tablist" aria-label="Browser mode">
          <button
            :for={
              {mode, label, icon} <- [
                {"classes", "Classes", "▣"},
                {"native", "Native", "⚛"},
                {"aliases", "Type Aliases", "≈"}
              ]
            }
            type="button"
            role="tab"
            class={[to_string(@browser_mode) == mode && "on"]}
            aria-selected={to_string(to_string(@browser_mode) == mode)}
            aria-label={label}
            title={label}
            phx-click="browser_mode"
            phx-value-mode={mode}
          >
            {icon}
          </button>
        </div>
        <div
          :if={@browser_mode == :classes}
          class="seg seg-icon"
          role="tablist"
          aria-label="Class tree view"
        >
          <button
            :for={
              {view, label, icon} <- [
                {"hierarchy", "Hierarchy", "≡"},
                {"category", "Categories", "▦"}
              ]
            }
            type="button"
            role="tab"
            class={[@browser_view == view && "on"]}
            aria-selected={to_string(@browser_view == view)}
            aria-label={label}
            title={label}
            phx-click="browser_view"
            phx-value-view={view}
          >
            {icon}
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
                <div
                  :for={{category, rows} <- SystemBrowser.category_groups(@visible_classes)}
                  class="cat-group"
                >
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
                    ClassTree.hierarchy_rows_with_context(
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
                  class={"source-origin-tag #{SystemBrowser.source_origin_class(mod)}"}
                  title={SystemBrowser.source_origin_title(mod)}
                >
                  {SystemBrowser.source_origin_label(mod)}
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
      <%!-- BT-2903 (ADR 0108 Phase 8): the "Type Aliases" panel body — shown only
           in `:aliases` mode. A `type Name = ...` declaration produces no BEAM
           module (aliases erase entirely), so unlike Native there is nothing to
           open as a source tab — each row is a flat, read-only entry showing the
           alias's expansion inline, with a package/origin badge (mirroring the
           class tree/Native vocabulary) and an "internal" tag when set. --%>
      <div :if={@browser_mode == :aliases} class="panel-body" id="type-aliases-browser">
        <%= cond do %>
          <% @browser_type_aliases == [] -> %>
            <p class="muted-note">No type aliases in the workspace.</p>
          <% true -> %>
            <div class="tree type-aliases-list">
              <div :for={alias_row <- @browser_type_aliases} class="row" title={alias_row["name"]}>
                <span class="twig">●</span>
                <span class="cls mono">{alias_row["name"]}</span>
                <span class="alias-expansion mono">= {alias_row["expansion"]}</span>
                <span
                  :if={alias_row["source_origin"] && alias_row["source_origin"] != "project"}
                  class={"source-origin-tag #{SystemBrowser.source_origin_class(alias_row)}"}
                  title={SystemBrowser.source_origin_title(alias_row)}
                >
                  {SystemBrowser.source_origin_label(alias_row)}
                </span>
                <span :if={alias_row["internal"] == true} class="runtime-tag" title="internal alias">
                  internal
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
      <%!-- BT-2903: a count footer for the Type Aliases panel, mirroring Native's. --%>
      <div :if={@browser_mode == :aliases} class="actionbar sb-side native-count">
        <span class="native-badge">type</span>
        <span>Type aliases</span>
        <span class="count">{length(@browser_type_aliases)}</span>
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
        class={"source-origin-tag #{SystemBrowser.source_origin_class(class)}"}
        title={SystemBrowser.source_origin_title(class)}
      >
        {SystemBrowser.source_origin_label(class)}
      </span>
      <span
        :if={not context? && SystemBrowser.runtime_only?(class)}
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
              if(SystemBrowser.clause_active?(c, @view.selected_clause),
                do: " native-clause-active",
                else: ""
              )
          }
          aria-current={SystemBrowser.clause_active?(c, @view.selected_clause) && "true"}
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
  # BT-3238: the divider-grouped method view — `%{"has_dividers" => bool,
  # "categories" => [%{"name" => name_or_nil, "methods" => [...]}]}` — plus
  # the group-mode toggle and any in-progress section edit.
  attr :browser_categories, :map, required: true
  attr :browser_group_mode, :string, default: "protocol"
  attr :editing_section, :any, default: nil
  attr :section_form_error, :string, default: nil

  defp system_browser_methods(assigns) do
    assigns =
      assigns
      |> assign(
        :methods,
        SystemBrowser.filtered_methods(assigns.browser_protocols, assigns.selected_protocol)
      )
      |> assign(:total_methods, SystemBrowser.protocol_method_count(assigns.browser_protocols))
      |> assign(:has_dividers, assigns.browser_categories["has_dividers"] || false)
      |> assign(
        :section_methods,
        SystemBrowser.category_methods_for_side(
          assigns.browser_categories["categories"],
          assigns.browser_side
        )
      )
      |> assign(
        :insertable_methods,
        SystemBrowser.insertable_methods_for_side(
          assigns.browser_categories["categories"],
          assigns.browser_side
        )
      )

    ~H"""
    <div class="panel">
      <div class="panel-head">
        <%!-- BT-3247 review nit: the title can now ellipsis-truncate at a
             narrow `--browser-w`, so give it a `title=` attribute (same text
             as the rendered content) — a hover reveals the full class name
             even once the visible label is cut short. --%>
        <span
          class="panel-title"
          title={
            if @selected_class,
              do:
                if(@browser_side == "class", do: @selected_class <> " class", else: @selected_class),
              else: "Protocols & Methods"
          }
        >
          <%= if @selected_class do %>
            {if @browser_side == "class", do: @selected_class <> " class", else: @selected_class}
          <% else %>
            Protocols &amp; Methods
          <% end %>
        </span>
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
          <%!-- BT-3238: group-mode toggle (only meaningful once the class has
               at least one `// === Name ===` divider — a divider-free class
               never shows this row, so its method list renders exactly as
               before) + the owner-only "add a section" affordance. --%>
          <div
            :if={@has_dividers or (@role == :owner and @insertable_methods != [])}
            class="tree sb-section-toolbar"
            style="display:flex; align-items:center; gap:6px; padding:2px 8px;"
          >
            <span :if={@has_dividers} style="display:flex; gap:4px;">
              <button
                type="button"
                class={["btn-link", @browser_group_mode == "protocol" && "sel"]}
                phx-click="browser_group_mode"
                phx-value-mode="protocol"
              >
                Protocol
              </button>
              <button
                type="button"
                class={["btn-link", @browser_group_mode == "section" && "sel"]}
                phx-click="browser_group_mode"
                phx-value-mode="section"
              >
                Sections
              </button>
            </span>
            <span class="spacer"></span>
            <button
              :if={@role == :owner and @insertable_methods != []}
              type="button"
              class="btn-link"
              phx-click="browser_edit_section"
              phx-value-name=""
            >
              + Section
            </button>
          </div>
          <%!-- BT-3238: inline "add a new section" form — appears above the
               method list, in either group mode, once opened. --%>
          <form
            :if={@editing_section == :new}
            class="tree sb-section-form"
            style="padding:4px 8px;"
            phx-submit="browser_add_section"
          >
            <input
              type="text"
              name="new_name"
              class="field"
              placeholder="Section name"
              autocomplete="off"
              spellcheck="false"
              phx-mounted={Phoenix.LiveView.JS.focus()}
            />
            <select name="before_selector">
              <option :for={m <- @insertable_methods} value={m["selector"]}>
                before {m["selector"]}
              </option>
            </select>
            <input type="hidden" name="before_side" value={@browser_side} />
            <p :if={@section_form_error} class="new-class-error" role="alert">
              {@section_form_error}
            </p>
            <div class="modal-actions">
              <button type="button" class="btn ghost" phx-click="browser_cancel_section">
                Cancel
              </button>
              <button class="btn primary" type="submit">Add</button>
            </div>
          </form>
          <%= if @browser_group_mode == "section" do %>
            <%!-- BT-3238: the divider-grouped method list — one header per
                 category (in source order), its methods for the current
                 side nested underneath. Mirrors the LSP `documentSymbol`
                 outline shape (BT-2601): the implicit leading group (`name`
                 is `nil`) renders with no header, just its methods. --%>
            <div class="tree sb-sections">
              <div :if={@section_methods == []} class="empty">
                No methods on the {@browser_side} side.
              </div>
              <%= for category <- @browser_categories["categories"] || [] do %>
                <% category_methods =
                  Enum.filter(category["methods"] || [], &(&1["side"] == @browser_side)) %>
                <div :if={category["name"]} class="row section-row">
                  <span class="twig" style="color: var(--accent);">§</span>
                  <span class="mname">{category["name"]}</span>
                  <span class="meta">{length(category_methods)}</span>
                  <button
                    :if={@role == :owner}
                    type="button"
                    class="btn-link"
                    phx-click="browser_edit_section"
                    phx-value-name={category["name"]}
                    title="Rename section"
                  >
                    ✎
                  </button>
                </div>
                <form
                  :if={category["name"] && @editing_section == category["name"]}
                  class="sb-section-form"
                  style="padding:2px 8px 2px 20px;"
                  phx-submit="browser_rename_section"
                >
                  <input type="hidden" name="old_name" value={category["name"]} />
                  <input
                    type="text"
                    name="new_name"
                    class="field"
                    value={category["name"]}
                    autocomplete="off"
                    spellcheck="false"
                    phx-mounted={Phoenix.LiveView.JS.focus()}
                  />
                  <p :if={@section_form_error} class="new-class-error" role="alert">
                    {@section_form_error}
                  </p>
                  <div class="modal-actions">
                    <button type="button" class="btn ghost" phx-click="browser_cancel_section">
                      Cancel
                    </button>
                    <button class="btn primary" type="submit">Save</button>
                  </div>
                </form>
                <div
                  :for={m <- category_methods}
                  class={[
                    "row method-row",
                    @active_method && @active_method.class == @selected_class &&
                      @active_method.side == @browser_side &&
                      @active_method.selector == m["selector"] && "sel"
                  ]}
                  style={if category["name"], do: "padding-left: 20px;", else: ""}
                  phx-click="browser_select_method"
                  phx-value-class={@selected_class}
                  phx-value-side={@browser_side}
                  phx-value-selector={m["selector"]}
                >
                  <span class="twig" style="color: var(--accent);">m</span>
                  <span class="mname mono">{m["selector"]}</span>
                </div>
              <% end %>
            </div>
          <% else %>
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
                title={SystemBrowser.method_row_title(m)}
              >
                <span class="twig" style="color: var(--accent);">m</span>
                <span class="mname mono">{m["selector"]}</span>
                <span
                  :if={m["source_origin"] && m["source_origin"] != "project"}
                  class={"source-origin-tag #{SystemBrowser.source_origin_class(m)}"}
                  title={SystemBrowser.source_origin_title(m)}
                >
                  {SystemBrowser.source_origin_label(m)}
                </span>
                <span :if={SystemBrowser.runtime_only?(m)} class="runtime-tag" title="runtime-only">
                  ⚡
                </span>
                <span
                  :if={SystemBrowser.synthetic?(m)}
                  class="derived-tag"
                  title="compiler-derived (auto-generated synthetic method)"
                >
                  derived
                </span>
              </div>
            </div>
          <% end %>
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

  # `<.notice>` (dismissable status banner, BT-2612) moved to
  # `BtAttachWeb.CoreComponents` (BT-3291) so extracted panes can render it
  # too — auto-imported here via `use BtAttachWeb, :live_view`, so every
  # existing `<.notice ...>` call site below is unchanged.

  @impl true
  def render(assigns) do
    ~H"""
    <%!-- Workspace-wide keyboard chords ride the root element so they work no
         matter which pane has focus (data-scope="window"): Esc closes the
         focused editor tab (same path as its ✕), ⌘/ toggles the documentation
         disclosure. "mod+w" is inert in a browser — Cmd/Ctrl+W is on every
         browser's reserved list and closes the tab before the page sees it —
         but the desktop (Tauri) webview delivers it once its menu no longer
         claims ⌘W, giving the native app the real chord. --%>
    <div
      class="bt-cockpit"
      id="workspace-shortcuts"
      phx-hook="KeyboardShortcuts"
      data-scope="window"
      data-shortcuts={
        Jason.encode!(%{
          "escape" => "tab_close_active",
          "mod+w" => "tab_close_active",
          "mod+/" => "toggle_doc"
        })
      }
    >
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
                  native_module_shown={SystemBrowser.active_native_module(assigns)}
                  browser_type_aliases={@browser_type_aliases}
                />
                <%!-- Draggable divider (BT-2576): rebalances the class tree vs.
                     the method list ("more class, less method"). phx-update="ignore"
                     (BT-2591): the gutter div is empty and hook-owned, so LiveView
                     should never patch it. The async mount load DOES strip the
                     hook-set --browser-split var off the PARENT .browser-split (it
                     re-renders the class tree inside it), but the SplitDrag hook's
                     own MutationObserver — not updated() — re-applies the saved size
                     when that happens. --%>
                <%!-- BT-2733/BT-2903: the method browser (gutter + protocol/method
                     list) is a Beamtalk-class surface — native `.erl` modules have no
                     protocol browsing here (their clauses open in an editor tab), and
                     a `type` alias has no methods at all. In Native/Type-Aliases mode
                     we hide it so a stale `@selected_class` method list can't linger
                     under the unrelated native-module/alias list; `@selected_class`
                     is left intact, so switching back to Classes restores it with no
                     re-fetch. The remaining class/native/aliases panel then fills the
                     column (the `.browser-split > .panel:last-of-type` flex rule). --%>
                <div
                  :if={@browser_mode == :classes}
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
                  :if={@browser_mode == :classes}
                  browser_protocols={@browser_protocols}
                  selected_protocol={@selected_protocol}
                  selected_class={@selected_class}
                  browser_side={@browser_side}
                  active_method={SystemBrowser.selected_method_ref(assigns)}
                  active_def={SystemBrowser.selected_def_ref(assigns)}
                  role={@role}
                  browser_categories={@browser_categories}
                  browser_group_mode={@browser_group_mode}
                  editing_section={@editing_section}
                  section_form_error={@section_form_error}
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
                      {if Dock.ws_selection?(assigns),
                        do: "evaluates selection",
                        else: "evaluates buffer"}
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
                          id={Dock.workspace_editor_id()}
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
                              <summary class="repl-summary">
                                {Dock.repl_preview(entry.response)}
                              </summary>
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
                          id={Dock.repl_input_id()}
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
                            <%!-- Kind/side column (BT-3195): before this, a
                                 same-selector instance-side patch and a
                                 class-side patch/removal — both possible
                                 simultaneously since BT-3187's `(class,
                                 selector, side)` shadow-key fix — rendered as
                                 visually identical rows. --%>
                            <th>Kind</th>
                            <th>Intent</th>
                            <th>Flushable</th>
                            <th>Author</th>
                            <%!-- revert column (BT-2293): owner-only --%>
                            <th :if={@role == :owner}></th>
                          </tr>
                        </thead>
                        <tbody>
                          <%= for c <- @changes do %>
                            <% expanded =
                              MapSet.member?(@expanded_changes, {c.class, c.selector, c[:side]}) %>
                            <%!-- Destructive-tier row marker (ADR 0113 Phase 4,
                                 BT-3210, joined by ADR 0114 Phase 5 BT-3277's
                                 rename-class/rename-method): a `remove-class`
                                 entry deletes a `.bt` file on flush, and a
                                 `rename-class`/`rename-method` entry rewrites
                                 one or more files — all three are deliberately
                                 excluded from the ordinary "Save All to Disk"
                                 write (BT-3207's Tier 1/Tier 2 split) — this
                                 class drives a purely presentational CSS marker
                                 (`.destructive-row`, a tinted left border + a
                                 "destructive" `::after` badge on the Kind cell)
                                 so the row never reads as just another dirty
                                 patch. Kept off the `<td>` cells themselves (as
                                 an attribute or extra child node) so the
                                 exact-match `<td>#{kind}</td>` regex
                                 `workspace_changes_side_test.exs` (BT-3195)
                                 already asserts on every other row's Kind cell
                                 stays untouched. --%>
                            <tr class={
                              if c.kind in ["remove-class", "rename-class", "rename-method"],
                                do: "destructive-row"
                            }>
                              <%!-- Leading disclosure caret (BT-2636): toggles
                                   this row's structured net-vs-disk diff. Only
                                   rendered when the entry carries a diff (the same
                                   defensive `:if={c[:diff]}` guard the old
                                   in-column disclosure used — a method reverted to
                                   its on-disk body has no net change and never
                                   reaches this pane). `phx-value-entry-side`
                                   (BT-3195) keys the expand/collapse state on this
                                   row's side too, alongside `phx-value-class`/
                                   `-selector` — see `handle_event("toggle_change_diff", …)`
                                   for why a same-selector instance-side and
                                   class-side row need independent toggle state. --%>
                              <td class="diff-toggle-col">
                                <button
                                  :if={c[:diff]}
                                  type="button"
                                  class="diff-toggle"
                                  phx-click="toggle_change_diff"
                                  phx-value-class={c.class}
                                  phx-value-selector={c.selector}
                                  phx-value-entry-side={c[:side] || ""}
                                  aria-expanded={to_string(expanded)}
                                  title={if expanded, do: "Hide diff", else: "Show diff"}
                                >
                                  {if expanded, do: "▼", else: "›"}
                                </button>
                              </td>
                              <td class="k">{c.class}</td>
                              <td>{c.selector}</td>
                              <td>{change_kind_label(c)}</td>
                              <td>{c.intent}</td>
                              <td>{if c.flushable, do: "yes", else: "no"}</td>
                              <td>{c.author_kind}</td>
                              <%!-- Revert one pending change (ADR 0082 Phase 5,
                                   completeness BT-2663/BT-2664/BT-2665; ADR 0114
                                   Phase 5 BT-3274 for rename-class/rename-method).
                                   Owner-only (`revert` is an :execute op).
                                   Instance-side and class-side method patches are
                                   revertable (a modify re-installs the prior body;
                                   an add removes the method), a new-class entry is
                                   revertable (it removes the just-created class), a
                                   `remove-method` entry is revertable (it restores
                                   the removed method — BT-3194, reusing
                                   `revert_method/3`'s side-aware selection from
                                   BT-3187), and a `rename-class`/`rename-method`
                                   entry is revertable (it splices every rewritten
                                   site back to its own prior body — BT-3274). Gate
                                   the button on a positive kind assertion so an
                                   unanticipated future kind hides the affordance
                                   rather than offering one that errors. New-class
                                   AND rename-class rows both carry no selector (a
                                   whole-class-identity entry, not a method-level
                                   one), so both send the `new-class` placeholder
                                   the workspace maps back to whichever of the
                                   two — by highest seq — the row shows
                                   (`find_revert_target/3`'s `match_selector/1`).

                                   `phx-value-entry-side` (ADR 0112, BT-3187) carries
                                   this *row's* side (`"instance"`/`"class"`, or `""`
                                   for a sideless new-class/rename-class row) into
                                   the revert call, so a same-selector instance-side
                                   and class-side entry — otherwise indistinguishable
                                   by (class, selector) alone — resolve to the one
                                   this row actually shows, not whichever has the
                                   higher seq. Named `entry-side` (not `side`) so it
                                   doesn't collide with the browser's
                                   `phx-value-side` instance/class toggle
                                   (BT-2491) — a plain `button[phx-value-side=...]`
                                   selector would otherwise match both controls. --%>
                              <td :if={@role == :owner}>
                                <button
                                  :if={
                                    c.kind in [
                                      "instance",
                                      "class",
                                      "new-class",
                                      "remove-method",
                                      "rename-class",
                                      "rename-method"
                                    ]
                                  }
                                  class="btn-link"
                                  type="button"
                                  phx-click="revert"
                                  phx-value-class={c.class}
                                  phx-value-selector={
                                    if(c.kind in ["new-class", "rename-class"],
                                      do: "new-class",
                                      else: c.selector
                                    )
                                  }
                                  phx-value-entry-side={c[:side] || ""}
                                  phx-disable-with="Reverting…"
                                >
                                  revert
                                </button>
                                <%!-- Delete file (ADR 0113 Phase 4, BT-3210):
                                     the browser's second, independently-
                                     confirmed gesture for a `remove-class`
                                     row — "Remove Class" (the editor action
                                     above) only removed the class from
                                     memory; this click is what actually
                                     deletes its `.bt` file, so it gets its
                                     own `data-confirm` dialog rather than
                                     silently riding along with "Save All to
                                     Disk" (which stays Tier-1-only, BT-3207). --%>
                                <button
                                  :if={c.kind == "remove-class"}
                                  class="btn-link danger"
                                  type="button"
                                  phx-click="flush_destructive"
                                  phx-value-class={c.class}
                                  phx-value-kind={c.kind}
                                  phx-disable-with="Deleting…"
                                  data-confirm={
                                    "Permanently delete #{c.class}'s source file from disk? This cannot be undone."
                                  }
                                >
                                  delete file
                                </button>
                                <%!-- Apply rename (ADR 0114 Phase 5, BT-3277):
                                     the same second, independently-confirmed
                                     gesture as "delete file" above, for a
                                     `rename-class`/`rename-method` row —
                                     "Rename Class"/"Rename Method" (the editor
                                     action) only renamed in memory; this click
                                     is what actually rewrites the confirmed
                                     sites on disk (a class-rename also moves
                                     the `.bt` file itself). Revertable (unlike
                                     `remove-class`), so this sits alongside —
                                     not instead of — the "revert" button
                                     above. --%>
                                <button
                                  :if={c.kind in ["rename-class", "rename-method"]}
                                  class="btn-link danger"
                                  type="button"
                                  phx-click="flush_destructive"
                                  phx-value-class={c.class}
                                  phx-value-kind={c.kind}
                                  phx-disable-with="Renaming…"
                                  data-confirm={rename_flush_confirm_message(c)}
                                >
                                  apply rename
                                </button>
                              </td>
                            </tr>
                            <%!-- The net change vs disk (ADR 0082 Phase 5,
                                 BT-2575), now a structured, gutter-aligned,
                                 coloured diff (BT-2636) rendered full-width
                                 beneath the row while expanded. Reuses the shared
                                 `unified_diff/1` renderer with the Git pane. --%>
                            <tr :if={c[:diff] && expanded} class="diff-row">
                              <td colspan={if @role == :owner, do: 8, else: 7}>
                                <.unified_diff diff={c[:diff]} />
                              </td>
                            </tr>
                          <% end %>
                        </tbody>
                      </table>
                    <% end %>

                    <%!-- Reload-induced findings (ADR 0105 Phase 1, BT-2779):
                         live, session-only type-check findings attributed to
                         a caller whose re-check surfaced a signature-change
                         or removed-selector diagnostic after a dependency
                         reloaded. Shares this tab with the ChangeLog table
                         above — both answer "what does the live image know
                         is different" — with its own heading + summary, and
                         clears the same way the ChangeLog row does: a fresh
                         `ReloadCheckCompleted` push replaces an owner's
                         findings wholesale (clearing-by-replacement), so a
                         fixed caller simply drops off this list. --%>
                    <h3 class="reload-findings-heading">Reload Checks</h3>
                    <%= if @reload_findings == [] do %>
                      <p class="muted-note">
                        No reload-induced findings. A stale caller appears here after a live
                        signature change or removed selector.
                      </p>
                    <% else %>
                      <table class="bt-table bt-reload-findings-table">
                        <thead>
                          <tr>
                            <th>Caller</th>
                            <th>Site</th>
                            <th>Severity</th>
                            <th>Message</th>
                          </tr>
                        </thead>
                        <tbody>
                          <%= for row <- reload_finding_rows(@reload_findings) do %>
                            <tr>
                              <td class="k">{row.owner}</td>
                              <td>
                                <%= if row.method do %>
                                  {row.owner}&gt;&gt;{row.method} (line {row.line})
                                <% else %>
                                  —
                                <% end %>
                              </td>
                              <td>{row.severity}</td>
                              <td>
                                {row.message}
                                <%= if row.note do %>
                                  <br /><span class="muted-note">{row.note}</span>
                                <% end %>
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
                        not match?(%{kind: :native}, MethodEditor.active_tab(assigns))
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
                  <% match?(%{kind: :native}, MethodEditor.active_tab(assigns)) -> %>
                    <%!-- BT-2667: a standalone native module opened as an editor TAB
                         (not the retired single-slot overlay). It scrolls inside the
                         editor pane (BT-2658) and shows a clean, project-relative
                         source path (BT-2668) — never the absolute host path.
                         BT-2670: a *project-owned* native (`view.editable == true`)
                         is editable for the Owner — edit → compile → reload →
                         write-back via `native_save`. Deps/stdlib natives, and every
                         non-Owner role, stay strictly read-only. --%>
                    <% nt = MethodEditor.active_tab(assigns) %>
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
                          {MethodEditor.method_editor_shortcuts_attrs()}
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
                  <% match?(%{}, MethodEditor.active_tab(assigns)) -> %>
                    <%!-- breadcrumb: Class › side › selector for the active tab. --%>
                    <% {bc_class, bc_side, bc_sel} =
                      MethodEditor.breadcrumb(MethodEditor.active_tab(assigns)) %>
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
                        :for={badge <- MethodEditor.modifier_badges(MethodEditor.active_tab(assigns))}
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
                      <% pkg_tab = MethodEditor.active_tab(assigns) %>
                      <% pkg_label = MethodEditor.header_package_label(pkg_tab) %>
                      <span
                        :if={pkg_label != ""}
                        class={"source-origin-tag header #{MethodEditor.header_origin_class(pkg_tab)}"}
                        title={MethodEditor.header_origin_title(pkg_tab)}
                        aria-label={MethodEditor.header_origin_title(pkg_tab)}
                      >
                        {pkg_label}
                      </span>
                      <%!-- Doc toggle inline on the breadcrumb line (BT-2604). --%>
                      <% doc_tab = MethodEditor.active_tab(assigns) %>
                      <%!-- BT-2714: a compiler-derived tab has no editor competing
                       for space, so its doc block is forced open below and the
                       collapse toggle is hidden (nothing to collapse into). --%>
                      <button
                        :if={doc_tab.doc != nil and not MethodEditor.synthetic_tab?(doc_tab)}
                        type="button"
                        class="doc-toggle-inline"
                        phx-click="toggle_doc"
                        aria-expanded={to_string(@doc_expanded)}
                        title={
                          if @doc_expanded, do: "Collapse documentation", else: "Expand documentation"
                        }
                      >
                        <span class="doc-caret">{if @doc_expanded, do: "▾", else: "▸"}</span>
                        <span class="doc-label">{MethodEditor.doc_summary_label(doc_tab)}</span>
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
                          MethodEditor.active_tab(assigns).disk_differs and
                            not MethodEditor.active_tab(assigns).runtime_only
                        }
                        class="runtime-tag"
                        title="unflushed live patch (image differs from disk)"
                      >
                        unflushed
                      </span>
                      <span
                        :if={MethodEditor.active_tab(assigns).runtime_only}
                        class="runtime-tag"
                        title="runtime-only (no source on disk)"
                      >
                        ⚡
                      </span>
                      <span :if={@role != :owner} class="meta-note read-only">
                        read-only
                      </span>
                      <span
                        :if={@role == :owner and MethodEditor.active_tab(assigns).new}
                        class="meta-note edited"
                      >
                        new
                      </span>
                      <span
                        :if={
                          @role == :owner and not MethodEditor.active_tab(assigns).new and
                            MethodEditor.active_tab(assigns).dirty
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
                          @role == :owner and not MethodEditor.active_tab(assigns).new and
                            not MethodEditor.active_tab(assigns).dirty and
                            not MethodEditor.synthetic_tab?(MethodEditor.active_tab(assigns))
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
                        :if={
                          doc_tab.doc != nil and
                            (@doc_expanded or MethodEditor.synthetic_tab?(doc_tab))
                        }
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
                            aria-expanded={
                              to_string(SystemBrowser.native_shown?(assigns, doc_tab.class))
                            }
                          >
                            {if SystemBrowser.native_shown?(assigns, doc_tab.class),
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
                        <div
                          :if={SystemBrowser.native_shown?(assigns, doc_tab.class)}
                          class="native-body"
                        >
                          <.native_source_body
                            view={@native_view}
                            fallback_module={doc_tab.native_module}
                            dismiss_event="dismiss_native_error"
                          />
                        </div>
                      </section>
                      <%= cond do %>
                        <% MethodEditor.synthetic_tab?(MethodEditor.active_tab(assigns)) -> %>
                          <%!-- BT-2714: a compiler-derived method (value accessor,
                           `with<Field>:` setter, actor `new`/`spawn`) has no editable
                           source. Render read-only where the editor would be: the
                           resolved signature + doc already show in the doc block above
                           (forced open for synthetic tabs), so this panel states the
                           read-only reason and keeps Senders/Implementors navigation.
                           Shown for every role, owner included — there is nothing to
                           edit, so the blank editable buffer (the old bug) is gone. --%>
                          <div class="synthetic-note">
                            <div
                              :if={MethodEditor.active_tab(assigns).signature}
                              class="mono synthetic-sig"
                            >
                              {MethodEditor.active_tab(assigns).signature}
                            </div>
                            <p class="muted-note">
                              Compiler-derived method — auto-generated, with no editable
                              source. Any documentation is shown above.
                            </p>
                            <div
                              :if={MethodEditor.active_tab(assigns).kind == :method}
                              class="nav-actions"
                            >
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
                            {MethodEditor.method_editor_shortcuts_attrs()}
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
                            <% tab = MethodEditor.active_tab(assigns) %>
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
                              data-lint-mode={
                                if MethodEditor.active_tab(assigns).kind == :method, do: "method"
                              }
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
                              <div
                                :if={MethodEditor.active_tab(assigns).kind == :method}
                                class="nav-actions"
                              >
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
                                  MethodEditor.active_tab(assigns).kind == :def and
                                    MethodEditor.active_tab(assigns)[:is_protocol]
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
                              <%!-- Remove Method (ADR 0112 Phase 4, BT-3189): only on an
                             existing (already-saved) method tab — a brand-new,
                             not-yet-compiled method has no live method to
                             remove. Wired the same way "Save" is: type="button"
                             so it never submits this form, phx-click drives
                             `remove_method` (→ `Class removeSelector: #sel`
                             via the existing `evaluate` op), and `data-confirm`
                             gates the destructive action the same way
                             `git_revert`'s discard button does. --%>
                              <button
                                :if={
                                  MethodEditor.active_tab(assigns).kind == :method and
                                    not MethodEditor.active_tab(assigns)[:new]
                                }
                                class="btn btn-sm"
                                type="button"
                                phx-click="remove_method"
                                data-confirm={
                                  "Remove #{@edit_selector} from #{@edit_class}#{if MethodEditor.active_tab(assigns).side == "class", do: " class", else: ""}? This cannot be undone from the editor."
                                }
                              >
                                Remove Method
                              </button>
                              <%!-- Rename Method (ADR 0114 Phase 5, BT-3277): same
                             existing-method-only guard as Remove Method above —
                             opens the Rename modal (`open_rename`) rather than
                             renaming immediately, since (unlike Remove) this
                             action needs a new selector typed first. Not
                             `data-confirm`-gated itself; the modal's own
                             submit is the confirming gesture (mirroring the
                             New Class modal, not a `data-confirm` dialog). --%>
                              <button
                                :if={
                                  MethodEditor.active_tab(assigns).kind == :method and
                                    not MethodEditor.active_tab(assigns)[:new]
                                }
                                class="btn btn-sm"
                                type="button"
                                phx-click="open_rename"
                              >
                                Rename Method
                              </button>
                              <%!-- Remove Class (ADR 0113 Phase 4, BT-3210): only on a
                             class-definition tab — a `:def` tab always names an
                             already-existing class (there is no "new, unsaved
                             class" draft state, see the NEW CLASS comment
                             below), so there is no `[:new]` guard to mirror
                             Remove Method's. Wired the same way: type="button"
                             so it never submits this form, phx-click drives
                             `remove_class` (→ `Class removeFromSystem` via the
                             existing `evaluate` op), and `data-confirm` gates
                             the destructive action — the browser's required
                             confirmation gesture per ADR 0113's Surface table.
                             This click removes the class from memory only; the
                             resulting `remove-class` ChangeLog entry still
                             needs its own "Delete file" confirmation in the
                             Changes pane to reach disk (ADR 0113's two-gesture
                             flow). --%>
                              <button
                                :if={MethodEditor.active_tab(assigns).kind == :def}
                                class="btn btn-sm"
                                type="button"
                                phx-click="remove_class"
                                data-confirm={
                                  "Remove #{@edit_class} from the running system? This deletes it from memory immediately; the source file is not touched until a separate confirmation to flush the removal. This cannot be undone from the editor."
                                }
                              >
                                Remove Class
                              </button>
                              <%!-- Rename Class (ADR 0114 Phase 5, BT-3277): same
                             `:def`-tab-only guard as Remove Class above —
                             opens the Rename modal rather than renaming
                             immediately, for the same "needs a new name
                             typed first" reason Rename Method does. --%>
                              <button
                                :if={MethodEditor.active_tab(assigns).kind == :def}
                                class="btn btn-sm"
                                type="button"
                                phx-click="open_rename"
                              >
                                Rename Class
                              </button>
                              <button class="btn btn-sm primary" type="submit">
                                Compile <span class="k">⌘S</span>
                              </button>
                              <button class="btn btn-sm" type="button" phx-click="flush">
                                Save All to Disk
                              </button>
                            </div>
                          </form>
                          <%!-- RENAME modal (ADR 0114 Phase 5, BT-3277): opened by
                         "Rename Class"/"Rename Method" above. One shared modal for
                         both kinds (`@rename_kind`), mirroring the New Class
                         modal's shape (single field, Cancel/submit actions,
                         Escape/click-away dismiss) — the memory-mutating rename
                         itself fires on submit; reaching disk is the Changes
                         pane's separate "apply rename" gesture (the
                         destructive-dirty-indicator affordance ADR 0113
                         established for delete, reused here per ADR 0114's
                         Surface table). --%>
                          <div
                            :if={@role == :owner and @rename_open}
                            id="rename-modal"
                            class="modal-scrim"
                            phx-window-keydown="close_rename"
                            phx-key="escape"
                          >
                            <div
                              class="modal-dialog"
                              role="dialog"
                              aria-modal="true"
                              aria-label={
                                if @rename_kind == :class, do: "Rename class", else: "Rename method"
                              }
                              phx-click-away="close_rename"
                            >
                              <div class="modal-head">
                                <h2 class="modal-title">
                                  {if @rename_kind == :class,
                                    do: "Rename Class",
                                    else: "Rename Method"}
                                </h2>
                                <button
                                  type="button"
                                  class="panel-close"
                                  phx-click="close_rename"
                                  aria-label="Close Rename dialog"
                                  title="Close"
                                >
                                  ×
                                </button>
                              </div>
                              <form
                                id="rename-form"
                                phx-submit="rename_submit"
                                class="new-class-modal-form"
                              >
                                <label class="new-class-field-label" for="rename-new-name">
                                  {if @rename_kind == :class,
                                    do: "New name for #{@rename_class}",
                                    else: "New selector for #{@rename_class} #{@rename_old_selector}"}
                                </label>
                                <input
                                  type="text"
                                  id="rename-new-name"
                                  name="new_name"
                                  class="field"
                                  value={@rename_new_name}
                                  autocomplete="off"
                                  spellcheck="false"
                                  placeholder={
                                    if @rename_kind == :class, do: "Accumulator", else: "incrementBy"
                                  }
                                  aria-describedby={if @rename_error, do: "rename-error"}
                                  aria-invalid={to_string(@rename_error != nil)}
                                  phx-mounted={Phoenix.LiveView.JS.focus()}
                                />
                                <p
                                  :if={@rename_error}
                                  id="rename-error"
                                  class="new-class-error"
                                  role="alert"
                                >
                                  {@rename_error}
                                </p>
                                <div class="modal-actions">
                                  <button type="button" class="btn ghost" phx-click="close_rename">
                                    Cancel
                                  </button>
                                  <button
                                    class="btn primary"
                                    type="submit"
                                    phx-disable-with="Renaming…"
                                  >
                                    Rename
                                  </button>
                                </div>
                              </form>
                            </div>
                          </div>
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
                          <div
                            :if={MethodEditor.active_tab(assigns).kind == :method}
                            class="nav-actions"
                          >
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
                              MethodEditor.active_tab(assigns).kind == :def and
                                MethodEditor.active_tab(assigns)[:is_protocol]
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
                     tolerates an absent `tab` and validates an empty class.

                     BT-2588: this form must carry the SAME `phx-hook`/`data-scope`
                     as the active-tab form below (both share id="method-editor-form")
                     — see `method_editor_shortcuts_attrs/0` for why a divergence
                     here silently breaks ⌘S. It passes `%{}` (no chords bound): the
                     hook must still mount here, but must not request-submit this
                     empty/hidden form — no tab means nothing to save. --%>
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
                        {MethodEditor.method_editor_shortcuts_attrs(%{})}
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
                    <span class="panel-title">Bindings</span> <span class="spacer"></span>
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
                    <span class="panel-title">Inspector</span> <span class="spacer"></span>
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
                        <span :if={Inspector.stat_status(@inspect_stats)} class="chip pid-stat">
                          <span class="dot"></span>{Inspector.stat_status(@inspect_stats)}
                        </span>
                        <%!-- not is_nil, not truthiness: a mailbox depth of 0 (the
                             actor drained) is the most reassuring reading and must
                             still show, but 0 is falsy in a HEEx `:if`. --%>
                        <span
                          :if={not is_nil(Inspector.stat_mailbox(@inspect_stats))}
                          class="chip pid-stat"
                        >
                          mailbox <b>{Inspector.stat_mailbox(@inspect_stats)}</b>
                        </span>
                        <span
                          :if={not is_nil(Inspector.stat_reductions(@inspect_stats))}
                          class="chip pid-stat"
                        >
                          reductions <b>{Inspector.stat_reductions(@inspect_stats)}</b>
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
                          Inspector.pokeable?(assigns)
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
          <Inspector.inspector_windows :if={@windows != []} windows={@windows} role={@role} />
          <%!-- The Changes (ChangeLog) viewer and the live Transcript stream now
               live in the tabbed Workspace dock above (BT-2490), not a separate
               full-width footer. --%>
        <% else %>
          <div class="cockpit" style="grid-template-columns: minmax(0, 1fr);">
            <div class="col">
              <div class="panel" style="flex:1;">
                <div class="panel-head"><span class="panel-title">Workspace</span></div>
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

  @doc false
  # The single source of truth for `@dock_events` outside this module — a
  # test asserts every name here resolves to an implemented
  # `Dock.handle_event/3` clause (not a `FunctionClauseError`), so a name
  # added/removed on one side without the other fails CI instead of only a
  # crafted WebSocket event in production. Kept away from the `handle_event/3`
  # clause group (rather than beside `@dock_events`'s own definition) since a
  # `def` of a different name interposed between two `handle_event/3` clauses
  # trips the compiler's "clauses should be grouped together" warning.
  def dock_events, do: @dock_events

  @doc false
  # The single source of truth for `@method_editor_events` outside this
  # module — mirrors `dock_events/0`'s rationale: a test asserts every name
  # here resolves to an implemented `MethodEditor.handle_event/3` clause.
  def method_editor_events, do: @method_editor_events

  @doc false
  # Unlike `dock_events/0`/`method_editor_events/0` above, `@inspector_events`
  # is not an independent hand-maintained copy — it IS
  # `Inspector.__inspector_events__/0` (BT-3301). This accessor exists only
  # so a test can assert the two stay literally identical, guarding against a
  # future edit that reintroduces a second hardcoded list here.
  def inspector_events, do: @inspector_events

  @doc false
  # Mirrors `inspector_events/0`: `@system_browser_events` IS
  # `SystemBrowser.__system_browser_events__/0` (BT-3297, following the
  # BT-3301 fix), never a second hand-maintained copy. This accessor exists
  # only so a test can assert the two stay literally identical.
  def system_browser_events, do: @system_browser_events
end

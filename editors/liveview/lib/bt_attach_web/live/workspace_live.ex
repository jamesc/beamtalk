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
  import BtAttachWeb.Live.BrowserComponents
  import BtAttachWeb.Live.ShellComponents

  require Logger

  alias BtAttach.Facade
  alias BtAttach.SessionRegistry
  alias BtAttach.Workspace
  alias BtAttachWeb.Live.ClassModals
  alias BtAttachWeb.Live.Dock
  alias BtAttachWeb.Live.Inspector
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.RequestContext
  alias BtAttachWeb.Live.SessionLifecycle
  alias BtAttachWeb.Live.SystemBrowser
  alias BtAttachWeb.Live.TestRunner

  # All RBAC-relevant workspace ops go through the curated facade (ADR 0091
  # Decision 3) — never a raw Workspace/:rpc call from an event handler. Pure
  # transport/display/lifecycle helpers (connect, render_term, session start)
  # go through the injectable workspace client so the mount and session-start
  # paths are testable without a live node (BT-2554). `RequestContext.build/1`
  # carries the request identity the facade audits / RBAC gates on (BT-2421).

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
      SessionLifecycle.attach(assign(socket, :token, token))
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
      %{"workspace_token" => token} when is_binary(token) ->
        scope_token(token, socket.assigns[:current_user])

      _ ->
        nil
    end
  end

  # Bind the client-held resume token to the logged-in user (BT-5070): the
  # registry key is a digest of the user's subject and the token, so a token
  # presented by a different user misses and gets a fresh session instead of
  # resuming someone else's. With no authenticated subject (auth disabled, or
  # dev-auth's single fixed identity) there is nobody to bind to and the token
  # is used as-is.
  @doc false
  def scope_token(token, %{"sub" => sub}) when is_binary(sub) and sub != "" do
    :sha256 |> :crypto.hash([sub, <<0>>, token]) |> Base.url_encode64(padding: false)
  end

  def scope_token(token, _user), do: token

  # Test Runner pane events (BT-2557/BT-2597/BT-2599, epic BT-3290): test
  # catalogue discovery, running all/one TestCase subclass, loading the
  # project's test/ files, opening a failing test method, and the
  # cross-pane `dismiss_notice` utility all delegate to
  # `BtAttachWeb.Live.TestRunner`, extracted out of this module (BT-3298) so
  # its `handle_event/3`/`handle_async/3` clauses are directly unit-testable.
  # See that module's docs for the full per-event behaviour (each clause used
  # to run here). Reads its event list from `__test_runner_events__/0` —
  # mirroring the BT-3301 fix that keeps this list from becoming a second,
  # hand-maintained copy of the event names.
  @test_runner_events TestRunner.__test_runner_events__()

  # BT-5081: the "workspace lost" banner's Reconnect control. Re-runs the same
  # attach path a socket reconnect uses: the registry's session is resumed when
  # it is still alive (a distribution blip) or a fresh one is started when the
  # workspace restarted. A restart discards the session's bindings, which is why
  # this is an explicit, visible action rather than a silent re-attach.
  @impl true
  def handle_event("workspace_reconnect", _params, %{assigns: %{workspace_lost: lost}} = socket)
      when not is_nil(lost) do
    {:noreply, SessionLifecycle.reconnect_workspace(socket)}
  end

  def handle_event("workspace_reconnect", _params, socket), do: {:noreply, socket}

  @impl true
  def handle_event(event, params, socket) when event in @test_runner_events do
    TestRunner.handle_event(event, params, socket)
  end

  # New Class / Rename modals + Remove Method / Remove Class (BT-2293/BT-2645,
  # ADR 0112 Phase 4 BT-3189, ADR 0113 Phase 4 BT-3210, ADR 0114 Phase 5
  # BT-3277) all delegate to `BtAttachWeb.Live.ClassModals`, extracted out of
  # this module (BT-3298, epic BT-3290) so its `handle_event/3` clauses are
  # directly unit-testable. See that module's docs for the full per-event
  # behaviour (each clause used to run here). Reads its event list from
  # `__class_modals_events__/0` — mirroring the BT-3301 fix that keeps this
  # list from becoming a second, hand-maintained copy of the event names.
  @class_modals_events ClassModals.__class_modals_events__()

  @impl true
  def handle_event(event, params, socket) when event in @class_modals_events do
    ClassModals.handle_event(event, params, socket)
  end

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
    flush_destructive eval_interrupt
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
  #
  # `remove_method`/`remove_class`/`open_rename`/`close_rename`/`rename_submit`
  # (ADR 0112 Phase 4 BT-3189, ADR 0113 Phase 4 BT-3210, ADR 0114 Phase 5
  # BT-3277) all delegate to `BtAttachWeb.Live.ClassModals` (BT-3298, epic
  # BT-3290) — see the `@class_modals_events` guard clause above.

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
  #
  # `new_method`/`toggle_new_class`/`close_new_class`/`new_class` (BT-2293,
  # BT-2645) all delegate to `BtAttachWeb.Live.ClassModals` (BT-3298, epic
  # BT-3290) — see the `@class_modals_events` guard clause above.

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

  # `dismiss_notice` (the generic dismiss for every pane's inline `.notice`
  # status banner, BT-2612) delegates to `BtAttachWeb.Live.TestRunner`
  # (BT-3298, epic BT-3290) — see the `@test_runner_events` guard clause
  # above.

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

  # BT-5081: the monitored workspace session pid went away (workspace restart,
  # lost distribution link, or the session was stopped). Flag it so the cockpit
  # shows the Reconnect banner; the tab itself stays up and usable.
  def handle_info(
        {:DOWN, ref, :process, _pid, reason},
        %{assigns: %{session_monitor: ref}} = socket
      ) do
    Logger.warning("workspace session went down: #{inspect(reason)}",
      domain: [:beamtalk, :liveview]
    )

    {:noreply, assign(socket, session_monitor: nil, workspace_lost: reason)}
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  # Git panel async load (BT-2590 S1, extracted BT-3295): the off-socket
  # `assign_git/1` `start_async` result forwards to
  # `BtAttachWeb.Live.Dock.handle_async/3` unchanged, mirroring the
  # `handle_event`/`handle_info` delegation above.
  @impl true
  def handle_async(:git_load, result, socket), do: Dock.handle_async(:git_load, result, socket)

  # BT-5081: a Workspace/REPL eval ran off-socket; fold its outcome via the Dock.
  def handle_async(:eval, result, socket), do: Dock.handle_async(:eval, result, socket)

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
  # We gate each surface on its own "loaded" flag (`:browser_classes_loaded`,
  # `:changes_loaded`, `:bindings_loaded` — BT-3321: independent flags, not one
  # shared `:source_loaded`, since `browser_classes` and `changes` each have
  # their own direct-write callers too, not just `:do_source_refresh`/
  # `:BindingChanged`): a flag is set `true` ONLY by a *successful* apply of
  # that surface's data (see `apply_changes/2`, `SystemBrowser.apply_browser_
  # classes/2`, `apply_bindings/2`), so we apply a surface's mount read only
  # while its flag is still `false`, then force it `true` so a later sync
  # refresh path stays the source of truth.
  #
  # Success-only gating is what handles the "early write errored, mount
  # succeeded" edge case: a write that fired before this fold and itself
  # failed (e.g. `ClassLoaded` while the workspace was momentarily unreachable
  # → `changes_error` set, list still empty) leaves the flag `false` — so this
  # fold's *successful* mount data still folds in and the pane shows real data
  # rather than getting stuck on the transient error until the next write. A
  # genuinely-empty workspace (no write at all) keeps every flag `false`, so
  # the empty-but-successful mount read still applies and the panes render
  # their empty state.
  #
  # `autoflush` is a stable settings probe with no live push, so it always folds in.
  def handle_async(:mount_load, {:ok, result}, socket) do
    socket =
      socket
      |> fold_mount_read(
        :browser_classes_loaded,
        result.browser_classes,
        &SystemBrowser.apply_browser_classes/2
      )
      |> fold_mount_read(:bindings_loaded, result.bindings, &apply_bindings/2)
      |> fold_mount_read(:changes_loaded, result.changes, &apply_changes/2)
      |> assign(browser_classes_loaded: true, changes_loaded: true, bindings_loaded: true)
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

  # Test Runner async loads (BT-2597/BT-2599, extracted BT-3298): the
  # off-socket `:test_op` (run/load) and `:test_discover` (catalogue
  # discovery) `start_async` results forward to
  # `BtAttachWeb.Live.TestRunner.handle_async/3` unchanged, mirroring the
  # `:git_load` delegation above.
  @impl true
  def handle_async(:test_op, result, socket),
    do: TestRunner.handle_async(:test_op, result, socket)

  @impl true
  def handle_async(:test_discover, result, socket),
    do: TestRunner.handle_async(:test_discover, result, socket)

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

  # The `dismiss_notice` key whitelist (BT-2612) moved to
  # `BtAttachWeb.Live.TestRunner.dismiss_key_to_assign/1` (BT-3298) alongside
  # the `handle_event("dismiss_notice", …)` clause it backs.

  # `facade_error/1` moved to `BtAttachWeb.Live.FacadeError` (BT-3291) so
  # extracted panes (e.g. `BtAttachWeb.Live.Inspector`) render the same
  # facade-error copy instead of keeping their own; `WorkspaceLive` itself has
  # had no direct caller left since the New Class/Rename/Remove handlers
  # moved to `BtAttachWeb.Live.ClassModals` (BT-3298).

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

  # Remove Method / Remove Class / Rename modal (ADR 0112 Phase 4 BT-3189, ADR
  # 0113 Phase 4 BT-3210, ADR 0114 Phase 5 BT-3277) and the New Class modal
  # (BT-2293, BT-2645) all moved to `BtAttachWeb.Live.ClassModals` (BT-3298)
  # alongside the `handle_event/3` clauses they back.

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
  # (BT-3295) and `BtAttachWeb.Live.ClassModals`'s remove/rename paths
  # (BT-3298).
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
  # git panel's own `assign_git/1` now lives there — and
  # `BtAttachWeb.Live.ClassModals`'s new-class create path (BT-3298).
  def maybe_refresh_git(socket) do
    if socket.assigns.dock_tab == "git", do: Dock.assign_git(socket), else: socket
  end

  # BT-2590 (S2): refresh the git panel after a *save* only when autoflush is on.
  # With autoflush off a per-method save patches the live image only — the on-disk
  # working tree is unchanged, so the git shell-out would return an identical
  # result and is pure waste. When autoflush is on the save wrote through to disk,
  # so the panel must reflect it (gated, as ever, on the Git tab being active).
  # Public: `BtAttachWeb.Live.MethodEditor`'s save-definition path (BT-3296)
  # and `BtAttachWeb.Live.ClassModals`'s new-class create path (BT-3298)
  # call it directly.
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
  # (BT-3295) and `BtAttachWeb.Live.ClassModals`'s remove/rename/new-class
  # paths (BT-3298).
  def assign_changes(socket), do: apply_changes(socket, read_changes(socket))

  # The raw `:changes` dispatch result — runs off the LiveView process in the
  # mount-load task, so it captures only `ctx`, never `socket`.
  defp read_changes(socket), do: Facade.dispatch(:changes, %{}, RequestContext.build(socket))

  # Fold a completed `:changes` read into the socket. Pure (no dispatch); shared by
  # `handle_async(:mount_load, …)` and every direct-write caller (`save_method`,
  # `ClassModals`, `Dock`'s git-panel refreshes, `:do_source_refresh`).
  defp apply_changes(socket, rows) when is_list(rows) do
    # Prune expanded-diff carets for rows that have left @changes (flush /
    # revert), so a re-saved method doesn't re-appear already-expanded. Keyed
    # by `{class, selector, side}` (BT-3195), matching the toggle handler and
    # the template's `expanded` lookup below.
    live_keys = MapSet.new(rows, &{&1.class, &1.selector, &1[:side]})
    expanded = MapSet.intersection(socket.assigns.expanded_changes, live_keys)

    # BT-3321: mark this surface loaded on every SUCCESSFUL apply, not just a
    # push refresh — a direct write (e.g. `save_method`) racing the mount-time
    # fold needs this flag too, or `handle_async(:mount_load, …)` would later
    # overwrite it with the stale pre-write snapshot. Monotonic (never reset
    # to `false` here): see `bind_session/3`'s flag-init comment.
    assign(socket,
      changes: rows,
      changes_error: nil,
      expanded_changes: expanded,
      changes_loaded: true
    )
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

  # BT-2619: fold one mount read into its surface's assigns only if a *successful*
  # write hasn't already loaded that surface. `loaded_key` is the per-surface
  # flag (`:browser_classes_loaded` / `:changes_loaded` / `:bindings_loaded`,
  # BT-3321): it is `true` only when a write succeeded, so a `false` flag means
  # either no write landed yet OR one landed but errored — in both cases the
  # mount read (which carries real, successful data here) should win. We do NOT
  # clear the flag here; the caller sets all flags `true` after the fold so the
  # post-mount sync refresh path remains the source of truth.
  defp fold_mount_read(socket, loaded_key, read, apply_fun) do
    if socket.assigns[loaded_key], do: socket, else: apply_fun.(socket, read)
  end

  # BT-2619: mark the bindings surface as loaded by a push — only on a successful
  # refresh (no `bindings_error`). `browser_classes`/`changes` no longer need an
  # equivalent here (BT-3321): `SystemBrowser.apply_browser_classes/2` and
  # `apply_changes/2` each set their own flag inline on success, so every
  # caller of `refresh_after_source_change/1` (this handler included) marks
  # both surfaces loaded as a side effect of the same call chain.
  defp mark_bindings_loaded(socket) do
    if is_nil(socket.assigns.bindings_error),
      do: assign(socket, :bindings_loaded, true),
      else: socket
  end

  # The Test-runner pane data source (BT-2557/BT-2597/BT-2599) — discovery,
  # running, and the render-template status-glyph helpers — moved to
  # `BtAttachWeb.Live.TestRunner` (BT-3298) alongside the `handle_event/3`/
  # `handle_async/3` clauses they back.

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
    case Facade.dispatch(:symbols, %{scope: "all"}, RequestContext.build(socket)) do
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

  # BT-5081: re-read the bindings once an off-socket eval has finished. The
  # workspace announces `BindingChanged` while the eval is still running, and
  # the session only commits the new bindings when it ends; when evals blocked
  # the LiveView, that push was handled after the eval returned, so it read the
  # committed state. Now that the LiveView stays responsive, the push can be
  # handled mid-eval and read the old bindings — and no further push follows.
  @doc false
  def refresh_bindings(%{assigns: %{session_pid: pid}} = socket) when is_pid(pid),
    do: assign_bindings(socket, pid)

  def refresh_bindings(socket), do: socket

  defp read_bindings(socket, pid),
    do: Facade.dispatch(:bindings, %{session_pid: pid}, RequestContext.build(socket))

  defp apply_bindings(socket, {:error, reason}),
    do: assign(socket, bindings: [], bindings_error: Workspace.render_error(reason))

  defp apply_bindings(socket, pairs) when is_list(pairs) do
    # One batched render RPC for the whole pane (BT-5081), not one per binding.
    values = Workspace.render_terms(Enum.map(pairs, fn {_name, term} -> term end))

    rows =
      Enum.zip_with(pairs, values, fn {name, term}, value ->
        %{
          name: name,
          value: value,
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

  # `<.notice>` (dismissable status banner, BT-2612) moved to
  # `BtAttachWeb.CoreComponents` (BT-3291) so extracted panes can render it
  # too — auto-imported here via `use BtAttachWeb, :live_view`, so every
  # existing `<.notice ...>` call site below is unchanged.

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

  @doc false
  # Mirrors `inspector_events/0`/`system_browser_events/0`: `@test_runner_events`
  # IS `TestRunner.__test_runner_events__/0` (BT-3298, following the BT-3301
  # fix), never a second hand-maintained copy. This accessor exists only so a
  # test can assert the two stay literally identical.
  def test_runner_events, do: @test_runner_events

  @doc false
  # Mirrors `test_runner_events/0`: `@class_modals_events` IS
  # `ClassModals.__class_modals_events__/0` (BT-3298, following the BT-3301
  # fix), never a second hand-maintained copy. This accessor exists only so a
  # test can assert the two stay literally identical.
  def class_modals_events, do: @class_modals_events
end

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

  The cockpit's client-side behaviour rides three LiveView JS hooks, registered
  on the `LiveSocket` in `assets/js/app.js` and referenced via `phx-hook`:

    * `CodeEditor` — a syntax-highlighting editor overlay. A transparent
      `<textarea>` sits over a `<pre>` the hook paints with the Beamtalk
      highlighter (`assets/js/hooks/highlight.js`); `.tok-*` colours resolve the
      themed `--t-*` CSS variables. The method-editor `source` field
      (`#method-editor-overlay`) uses it; the Workspace dock's editor
      (`#workspace-editor-overlay`, BT-2490) does too.
    * `KeyboardShortcuts` — maps Cmd/Ctrl chords to actions from a
      `data-shortcuts` JSON map. The method-editor form binds ⌘S → `submit`
      (request-submits the form so class/selector/source ride the normal
      `save_method` `phx-submit`); the Workspace dock binds ⌘D/⌘P/⌘I →
      `submit:<action>` (BT-2490), riding the eval form's hidden `action` field.
    * `SelectionTracker` — reports the editor textarea's selection
      (`{text, start, end}`) via the `select_source` event, held in
      `:edit_selection` so a later pane can evaluate the selection vs the buffer.

  ## Tweaks panel (BT-2487, epic BT-2482 Phase 1)

  The left column carries a **Tweaks** panel (`tweaks_panel/1`) — the cockpit's
  appearance controls, ported from the spike (`spikes/cockpit-ux-spike`): a
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
  Transcript, and Changes panes into one panel switched by `dock_tab` (held in
  `:dock_tab`, default `"workspace"`):

    * **Workspace** — a highlighted code editor (the BT-2485 `CodeEditor` overlay
      + `SelectionTracker`) wrapped in the eval `<form>` (`#eval-form`,
      `phx-submit="eval"`, field `expr`), with three actions that ride the same
      submit via an `action` field: **Do it** (⌘D — evaluate for side effects),
      **Print it** (⌘P — evaluate and show the result term), and **Inspect it**
      (⌘I — evaluate and open the result in the Inspector). All three reuse the
      existing `eval` op + `render_term`; inspectIt reuses `inspect_term/4`. They
      evaluate the editor's tracked selection if there is one (its own
      `SelectionTracker` → `select_workspace` → `:ws_selection`, kept separate
      from the method editor's `:edit_selection`), else the whole buffer.
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
  over the BT-2485 highlighted editor. The open-tab list is `:tabs` (each a map
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
    * **Dirty tracking** — the form's `phx-debounce`d `phx-change="edit_source"`
      reports live edits; `track_edit/2` flips the active tab's `dirty` when its
      `source` diverges from the last-compiled `base`, and a successful compile
      (`compile_clean/3`) clears the dot + re-bases. To avoid caret-jump we do
      NOT echo the live value back into the textarea on change — the element is
      re-keyed on `@active_tab`, so switching tabs remounts it with the new tab's
      source.
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
    * **Method source** — selecting a method (`browser_select_method`) drives a
      read-only centre display (`#browse-method-source`) of its image-accurate
      source with a `Class instance » selector · category` breadcrumb and
      image-diverged badges (`runtime` / unflushed `>>` patch). Read-only is
      enough until the tabbed editor consumes the browse selection in a follow-up.

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
  # stay on Workspace: they are not browser-supplied ops. `ctx/1` carries the
  # request identity the facade audits / RBAC gates on (BT-2421).
  defp ctx(socket),
    do: %{user: socket.assigns[:current_user], role: socket.assigns[:role] || :owner}

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
      case Workspace.connect() do
        :ok ->
          # Resume the tab's existing session if the registry still holds a live
          # one (reconnect within the grace window); otherwise start fresh.
          case resume_or_start(token, session_meta(socket)) do
            {:ok, session_id, pid} ->
              bind_session(socket, session_id, pid)

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
          {:ok, session_id, pid}
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

    case Workspace.start_session(session_id, meta) do
      pid when is_pid(pid) ->
        # Register before binding so a crash mid-bind can't leak: the registry
        # owns the close, keyed by the tab token (a nil token simply skips
        # registration — that session is non-resumable and closed in terminate/2).
        SessionRegistry.register(token, session_id, pid)
        {:ok, session_id, pid}

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
      socket
      |> assign(:connected, true)
      |> assign(:node, Workspace.node_name())
      |> assign(:session_id, session_id)
      |> assign(:session_pid, pid)
      |> assign(:result, nil)
      |> assign(:output, nil)
      |> assign(:error, nil)
      |> assign(:expr, "3 + 4")
      # Workspace dock active tab (BT-2490): Workspace | Transcript | Changes.
      |> assign(:dock_tab, "workspace")
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
      # Method-editor selection (BT-2485), reported by the SelectionTracker hook.
      |> assign(:edit_selection, nil)
      # Workspace-editor selection (BT-2490): the dock's own SelectionTracker,
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
      # protocol filter (`nil` = "all"). Selecting a method drives the center
      # method-source display (`browser_method`) with its breadcrumb. All four
      # browse ops are `:read`, so the pane works for the Observer role too.
      |> assign(:browser_view, "hierarchy")
      |> assign(:browser_side, "instance")
      |> assign(:selected_class, nil)
      |> assign(:selected_protocol, nil)
      |> assign(:browser_protocols, [])
      |> assign(:browser_method, nil)
      |> assign(:browser_error, nil)
      |> assign_browser_classes()
      |> assign_bindings(pid)
      |> assign_changes()
      |> stream(:transcript, [])
    else
      other ->
        Logger.error("subscribe failed: #{inspect(other)}")
        # Drop whichever subscription may have succeeded, then release the
        # session through the registry (which closes it) so we don't leave a
        # dangling subscriber or an orphaned session behind.
        Workspace.unsubscribe_transcript(self())
        Workspace.unsubscribe_bindings(self())
        force_close(socket.assigns[:token], pid)

        assign(socket,
          connected: false,
          error: "subscribe failed: #{inspect(other)}"
        )
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

  # Switch the Workspace dock's active tab (Workspace / Transcript / Changes,
  # BT-2490). Pure view state — no workspace round-trip; an unknown tab is
  # ignored rather than rendered, so a crafted value can't blank the dock.
  @impl true
  def handle_event("dock_tab", %{"tab" => tab}, socket)
      when tab in ~w(workspace transcript changes) do
    {:noreply, assign(socket, dock_tab: tab)}
  end

  def handle_event("dock_tab", _params, socket), do: {:noreply, socket}

  # Inspect a binding by name: look up its live term and drill into it via the
  # read-surface `inspect` op. Reference-following starts here.
  @impl true
  def handle_event("inspect", %{"name" => name}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    {:noreply, inspect_binding(socket, pid, name)}
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

  # Close a tab (the spike's × affordance). The last tab is never closable — the
  # editor always holds ≥1 tab — so a close request that would empty the strip is
  # ignored. Closing the active tab moves focus to the previous (or first)
  # remaining tab.
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

  # Track edits to the active tab so its dirty dot reflects unsaved changes
  # (BT-2494). The save_method form's `phx-change` reports the live source on
  # each keystroke; we stash it on the active tab and recompute its dirty flag
  # (source != the last-compiled base). Client-supplied, so a non-binary / absent
  # source is ignored rather than crashing.
  def handle_event("edit_source", %{"source" => source}, socket) when is_binary(source) do
    {:noreply, track_edit(socket, source)}
  end

  def handle_event("edit_source", _params, socket), do: {:noreply, socket}

  # Flush all pending durable changes to disk ("Save All to Disk", ADR 0082
  # `Workspace flush`). The summary's conflicts/skipped lists carry recoverable
  # conditions; a hard runtime failure renders as a structured error.
  def handle_event("flush", _params, socket) do
    {:noreply, flush_changes(socket)}
  end

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

  # Toggle the instance/class side. The protocol/method list is class-side
  # specific (a class's instance methods differ from its class methods), so
  # flipping the side re-fetches the selected class's protocols for the new side
  # and clears the protocol filter + any open method source. Pure toggle when no
  # class is selected yet.
  def handle_event("browser_side", %{"side" => side}, socket)
      when side in ~w(instance class) do
    socket = assign(socket, browser_side: side, selected_protocol: nil, browser_method: nil)

    case socket.assigns.selected_class do
      nil -> {:noreply, assign(socket, browser_protocols: [])}
      class -> {:noreply, load_protocols(socket, class, side)}
    end
  end

  def handle_event("browser_side", _params, socket), do: {:noreply, socket}

  # Select a class in the tree: fetch its protocols (for the current side) and
  # reset the protocol filter + open method. A non-binary / absent class name is
  # ignored rather than crashing the LiveView.
  def handle_event("browser_select_class", %{"class" => class}, socket)
      when is_binary(class) do
    socket =
      assign(socket, selected_class: class, selected_protocol: nil, browser_method: nil)

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

  # Select a method: fetch its image-accurate source and drive the center
  # method-source display (read-only until the tabbed editor consumes it in a
  # follow-up issue). The class/side/selector ride the click; a malformed payload
  # is ignored.
  def handle_event(
        "browser_select_method",
        %{"class" => class, "side" => side, "selector" => selector},
        socket
      )
      when is_binary(class) and is_binary(side) and is_binary(selector) do
    {:noreply, load_method_source(socket, class, side, selector)}
  end

  def handle_event("browser_select_method", _params, socket), do: {:noreply, socket}

  # Selection tracking (BT-2485): the SelectionTracker JS hook reports the
  # method-editor textarea's current selection (text + offsets). We hold it in
  # `edit_selection` so a later pane can evaluate the selected expression rather
  # than the whole buffer (the spike's "evaluates selection" vs "evaluates
  # buffer" distinction). The payload is client-supplied, so accept only the
  # well-formed shape and ignore anything else rather than crash the LiveView.
  def handle_event("select_source", %{"text" => text} = params, socket)
      when is_binary(text) do
    selection = %{
      text: text,
      start: clamp_offset(params["start"]),
      end: clamp_offset(params["end"])
    }

    {:noreply, assign(socket, edit_selection: selection)}
  end

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

  # Transcript push, delivered directly over distribution to this LiveView pid.
  @impl true
  def handle_info({:transcript_output, text}, socket) do
    line = %{id: System.unique_integer([:positive]), text: to_string(text)}
    {:noreply, stream_insert(socket, :transcript, line)}
  end

  # Bindings-changed push (BT-2399 `bindings` stream): a *signal*, not the data.
  # An eval on any session in the workspace may have changed binding values, so
  # re-read this session's bindings through the read-surface and re-render the
  # pane. This is the "updating live as bindings change" acceptance criterion,
  # driven by the facade subscription rather than polling.
  def handle_info({:bindings_changed, _session_id}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    {:noreply, assign_bindings(socket, pid)}
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
    cond do
      assigns.inspect_frozen or not watched_pid?(assigns.inspect_watch, pid) ->
        {:noreply, socket}

      assigns.refresh_pending ->
        # A refresh is already queued for this burst — collapse this push into it.
        {:noreply, socket}

      true ->
        Process.send_after(self(), :do_object_refresh, refresh_debounce_ms())
        {:noreply, assign(socket, refresh_pending: true)}
    end
  end

  # True when `pid` is the pid backing the currently-watched object term — so a
  # late push for an object we've since navigated away from (or never watched) is
  # ignored rather than spuriously re-reading + flashing the current head.
  defp watched_pid?({:beamtalk_object, _c, _m, watched}, pid)
       when is_pid(watched) and is_pid(pid),
       do: watched == pid

  defp watched_pid?(_watch, _pid), do: false

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

  def handle_info(_msg, socket), do: {:noreply, socket}

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

      case socket.assigns[:token] do
        token when is_binary(token) ->
          # Resumable session: defer teardown to the grace window.
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

  # ── Workspace dock actions (BT-2490) ────────────────────────────────────────

  # Which dock action the eval submit carried. The clicked action button rides
  # the form as `action`; a plain submit (the e2e `render_submit(%{expr: …})`)
  # carries none and defaults to printIt — the historical eval behaviour.
  defp eval_action(%{"action" => action}) when action in ~w(do_it print_it inspect_it),
    do: action

  defp eval_action(_params), do: "print_it"

  # The code an action evaluates: the Workspace editor's tracked selection if
  # there is one (the spike's "evaluates selection"), else the whole entered
  # buffer ("evaluates buffer"). The Workspace editor's SelectionTracker keeps
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

  # Render an eval success according to the chosen action, reusing the existing
  # `render_term` formatting and (for inspectIt) the same `inspect_term/4`
  # read-surface path bindings drill through:
  #
  #   * print_it   — show the result term (classic eval; the default).
  #   * do_it      — evaluate for side effects; show a terse confirmation only.
  #   * inspect_it — show the term AND open it in the Inspector (when inspectable).
  defp eval_success(socket, "do_it", _term, output, expr) do
    assign(socket, result: "✓ evaluated", output: present(output), error: nil, expr: expr)
  end

  defp eval_success(socket, "inspect_it", term, output, expr) do
    socket
    |> assign(
      result: Workspace.render_term(term),
      output: present(output),
      error: nil,
      expr: expr
    )
    |> inspect_term("→ result", term, [%{label: "→ result", term: term}])
  end

  # print_it (and the historical default).
  defp eval_success(socket, _print_it, term, output, expr) do
    assign(socket,
      result: Workspace.render_term(term),
      output: present(output),
      error: nil,
      expr: expr
    )
  end

  # Blank captured output is not worth rendering a pane for.
  defp present(""), do: nil
  defp present(nil), do: nil
  defp present(output) when is_binary(output), do: output

  # Normalise a client-supplied selection offset to a non-negative integer (or
  # nil). The SelectionTracker hook sends integer offsets, but the payload is
  # untrusted, so a missing / negative / non-integer value collapses to nil.
  defp clamp_offset(n) when is_integer(n) and n >= 0, do: n
  defp clamp_offset(_), do: nil

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

  # Validate the edit form, then drive the write-surface save. Empty class or
  # selector is a local validation error (rendered without a round-trip); a real
  # save threads the body value straight to the workspace install chokepoint.
  defp save_method_body(socket, class, selector, source, tab_id) do
    class = String.trim(class)
    selector = String.trim(selector)

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
        assign(socket, save_result: nil, save_error: "Enter a selector to save a method.")

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

      {:error, reason, _output, _warnings} ->
        assign(socket, save_result: nil, save_error: Workspace.render_error(reason))

      {:error, reason} ->
        assign(socket, save_result: nil, save_error: facade_error(reason))
    end
  end

  # Drive the write-surface flush and render its summary, then refresh changes so
  # the (now-flushed) entries drop out of the active view.
  defp flush_changes(socket) do
    case Facade.dispatch(:flush, %{}, ctx(socket)) do
      {:ok, summary} ->
        socket
        |> assign(flush_result: Workspace.format_flush_summary(summary), flush_error: nil)
        |> assign_changes()

      {:error, reason} ->
        assign(socket, flush_result: nil, flush_error: facade_error(reason))
    end
  end

  # Read the active ChangeLog ("Workspace changes", ADR 0082) and assign display
  # rows. A workspace that is unreachable or returns an unexpected shape renders an
  # error rather than crashing the pane.
  defp assign_changes(socket) do
    case Facade.dispatch(:changes, %{}, ctx(socket)) do
      rows when is_list(rows) ->
        assign(socket, changes: rows, changes_error: nil)

      {:error, reason} ->
        assign(socket, changes: [], changes_error: Workspace.render_error(reason))
    end
  end

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
  defp assign_browser_classes(socket) do
    case Facade.dispatch(:browse_classes, %{}, ctx(socket)) do
      {:value, rows} when is_list(rows) ->
        assign(socket, browser_classes: rows, browser_error: nil)

      {:error, reason} ->
        assign(socket, browser_classes: [], browser_error: facade_error(reason))
    end
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

  # Load one method's image-accurate source (op 3, `browse-method-source`) for the
  # center method-source display. The result carries `source` (`null` for a
  # sourceless runtime method), `origin`, and `disk_differs` (an unflushed live
  # patch) — all surfaced in the breadcrumb / badge. We keep the protocol of the
  # selected selector for the breadcrumb's `· category` segment by looking it up
  # in the already-loaded protocol tree.
  defp load_method_source(socket, class, side, selector) do
    case Facade.dispatch(
           :browse_method_source,
           %{class: class, side: side, selector: selector},
           ctx(socket)
         ) do
      {:value, result} when is_map(result) ->
        method = Map.put(result, "protocol", protocol_of(socket, selector))
        assign(socket, browser_method: method, browser_error: nil)

      {:error, reason} ->
        assign(socket, browser_method: nil, browser_error: facade_error(reason))
    end
  end

  # The protocol (method category) a selector belongs to, read from the loaded
  # protocol tree — the breadcrumb's `· category` part. Falls back to nil when the
  # selector isn't found (e.g. the tree hasn't loaded), which the breadcrumb omits.
  defp protocol_of(socket, selector) do
    Enum.find_value(socket.assigns.browser_protocols, fn proto ->
      selectors = Map.get(proto, "selectors", [])

      if Enum.any?(selectors, &(Map.get(&1, "selector") == selector)),
        do: Map.get(proto, "name")
    end)
  end

  # ── System Browser view helpers (BT-2491) ───────────────────────────────────

  # The class rows in display order for the active view. Hierarchy walks
  # roots→children indenting by superclass depth (capped at 2 like the spike);
  # Category groups by the class annotation, each group a `{category, rows}` pair.
  # Both return `{row, indent}` tuples so the template renders one branch.

  # Hierarchy: a flat, ordered list of `{class_row, indent}` walking the
  # superclass tree from roots down. A class whose superclass is not itself in the
  # browse set is treated as a root, so an external/kernel superclass doesn't hide
  # its subclasses. Any class unreachable from a root — e.g. a member of a
  # superclass cycle in a transiently-inconsistent live image, where every member
  # has an in-set superclass so none is a root — is appended at indent 0 rather
  # than silently dropped from the view (it still renders, just un-nested).
  defp hierarchy_rows(classes) do
    by_parent =
      Enum.group_by(classes, fn c ->
        super_name = Map.get(c, "superclass")

        if super_name && Enum.any?(classes, &(Map.get(&1, "name") == super_name)),
          do: super_name,
          else: :__root
      end)

    walked = Enum.reverse(walk_hierarchy(by_parent, :__root, 0, []))
    emitted = MapSet.new(walked, fn {class, _indent} -> Map.get(class, "name") end)
    orphans = for c <- classes, not MapSet.member?(emitted, Map.get(c, "name")), do: {c, 0}
    walked ++ Enum.sort_by(orphans, fn {c, _} -> Map.get(c, "name") end)
  end

  defp walk_hierarchy(by_parent, parent, indent, acc) do
    by_parent
    |> Map.get(parent, [])
    |> Enum.sort_by(&Map.get(&1, "name"))
    |> Enum.reduce(acc, fn class, acc ->
      acc = [{class, indent} | acc]
      walk_hierarchy(by_parent, Map.get(class, "name"), min(indent + 1, 2), acc)
    end)
  end

  # Category: `{category, [class_row]}` groups, each group's classes sorted by
  # name, the groups themselves sorted by category. A class with no category falls
  # into an "(uncategorized)" bucket rather than vanishing.
  defp category_groups(classes) do
    classes
    |> Enum.group_by(fn c -> Map.get(c, "category") || "(uncategorized)" end)
    |> Enum.sort_by(fn {category, _} -> category end)
    |> Enum.map(fn {category, rows} ->
      {category, Enum.sort_by(rows, &Map.get(&1, "name"))}
    end)
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

  # ── tabbed method editor data model (BT-2494) ───────────────────────────────
  #
  # A tab is a plain map; the open-tab list lives in `:tabs` and the focused
  # tab's id in `:active_tab`. The visible class/selector/source assigns (which
  # the save_method form binds) always mirror the active tab, so the existing
  # write-surface handler reads them unchanged.
  #
  #   %{
  #     id: stable string id (method-key or "def:<Class>"),
  #     kind: :method | :def,
  #     class: "Counter",
  #     side: "instance" | "class",     # methods only
  #     selector: "increment",          # methods only
  #     source: live edit buffer,
  #     base: last-compiled source (dirty = source != base),
  #     dirty: boolean
  #   }
  #
  # The cockpit opens with one starter method tab so the editor is never empty
  # (the strip always holds ≥1 tab) and the ⌘S / Save Method e2e flow has a tab
  # to compile into.

  defp init_tabs(socket) do
    tab = %{
      id: "method:Counter:instance:increment",
      kind: :method,
      class: "Counter",
      side: "instance",
      selector: "increment",
      source: "",
      base: "",
      dirty: false
    }

    socket
    |> assign(:tabs, [tab])
    |> assign(:active_tab, tab.id)
    # Mirror the starter tab into the form-backing edit assigns NOW, so the
    # method editor's class/selector/source inputs match the breadcrumb from the
    # first connected render (BT-2518). Without this, the inputs stay `""` until
    # the user clicks the tab (`tab_select` → `sync_active/2`), so a ⌘S /
    # Compile on open fails with "Enter a class name to save a method."
    |> sync_active(tab)
  end

  defp find_tab(socket, id), do: Enum.find(socket.assigns.tabs, &(&1.id == id))

  # The focused tab. Takes the bare `assigns` (not the socket) so the render
  # template can call it for the breadcrumb / dirty-state too; falls back to the
  # first tab if the active id somehow no longer maps (the strip is never empty).
  defp active_tab(%{tabs: tabs, active_tab: id}) do
    Enum.find(tabs, &(&1.id == id)) || List.first(tabs)
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
      save_result: nil,
      save_error: nil,
      flush_result: nil,
      flush_error: nil
    )
  end

  # Close a tab, keeping at least one open. Closing the active tab moves focus to
  # the previous remaining tab (or the first), re-syncing the form assigns.
  defp close_tab(socket, id) do
    tabs = socket.assigns.tabs

    if length(tabs) <= 1 or not Enum.any?(tabs, &(&1.id == id)) do
      socket
    else
      idx = Enum.find_index(tabs, &(&1.id == id))
      remaining = List.delete_at(tabs, idx)
      socket = assign(socket, :tabs, remaining)

      if socket.assigns.active_tab == id do
        next = Enum.at(remaining, max(idx - 1, 0))
        sync_active(assign(socket, :active_tab, next.id), next)
      else
        socket
      end
    end
  end

  # Open (or re-focus) a class-definition tab for the active tab's class. A def
  # tab evals its definition source on compile (saving compiles the class).
  defp open_definition(socket) do
    class = active_tab(socket.assigns).class
    id = "def:" <> class

    case find_tab(socket, id) do
      %{} ->
        activate_tab(socket, id)

      nil ->
        tab = %{
          id: id,
          kind: :def,
          class: class,
          side: nil,
          selector: nil,
          source: "",
          base: "",
          dirty: false
        }

        socket
        |> assign(:tabs, socket.assigns.tabs ++ [tab])
        |> assign(:active_tab, id)
        |> sync_active(tab)
    end
  end

  # Record a keystroke edit on the active tab and recompute its dirty flag
  # (source != last-compiled base). We deliberately do NOT re-assign
  # `:edit_source` here: echoing the live value back would make LiveView patch
  # the textarea mid-typing and jump the caret. The tab's `source` is the truth
  # the next compile / tab-switch reads; `:edit_source` is only the *initial*
  # textarea value, re-synced when a tab is (re)focused (the element is re-keyed
  # on `@active_tab`, so a fresh mount picks it up).
  defp track_edit(socket, source) do
    update_active_tab(socket, fn tab -> %{tab | source: source, dirty: source != tab.base} end)
  end

  # After a successful compile, clear the saved tab's dirty dot and re-base it on
  # the compiled source. `tab_id` is nil for the historical no-tab save payload —
  # then there's nothing to reconcile.
  defp compile_clean(socket, nil, _source), do: socket

  defp compile_clean(socket, tab_id, source) do
    update_active_tab_by_id(socket, tab_id, fn tab ->
      %{tab | source: source, base: source, dirty: false}
    end)
  end

  defp update_active_tab(socket, fun),
    do: update_active_tab_by_id(socket, socket.assigns.active_tab, fun)

  defp update_active_tab_by_id(socket, id, fun) do
    tabs = Enum.map(socket.assigns.tabs, fn t -> if t.id == id, do: fun.(t), else: t end)
    assign(socket, :tabs, tabs)
  end

  # The Class › side › selector breadcrumb label parts for the active tab.
  defp breadcrumb(%{kind: :def, class: class}), do: {class, nil, "class definition"}
  defp breadcrumb(%{class: class, side: side, selector: selector}), do: {class, side, selector}

  # ── bindings + inspector helpers ────────────────────────────────────────────

  # Read the session's live bindings through the read-surface and assign display
  # rows. Each row keeps the live `term` so the Inspector can follow object
  # references without a string round-trip; `inspectable?` flags object-valued
  # bindings the user can drill into.
  defp assign_bindings(socket, pid) do
    case Facade.dispatch(:bindings, %{session_pid: pid}, ctx(socket)) do
      {:error, reason} ->
        assign(socket, bindings: [], bindings_error: Workspace.render_error(reason))

      pairs when is_list(pairs) ->
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

  # Drop the current per-object subscription (if any) and forget the watched term.
  # Idempotent: a nil watch unsubscribes nothing.
  defp unwatch(%{assigns: %{inspect_watch: term}} = socket)
       when not is_nil(term) do
    Facade.dispatch(:unsubscribe_object, %{term: term, pid: self()}, ctx(socket))
    assign(socket, inspect_watch: nil)
  end

  defp unwatch(socket), do: assign(socket, inspect_watch: nil)

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
        assign(socket,
          poke_result: "→ #{Workspace.render_term(term)}",
          poke_error: nil
        )

      {:error, reason, _output, _warnings} ->
        assign(socket, poke_result: nil, poke_error: Workspace.render_error(reason))

      {:error, reason} ->
        assign(socket, poke_result: nil, poke_error: facade_error(reason))
    end
  end

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

  defp scalar_kind(term) when is_integer(term), do: "number"
  defp scalar_kind(term) when is_float(term), do: "number"
  defp scalar_kind(term) when is_binary(term), do: "string"
  defp scalar_kind(term) when is_boolean(term), do: "boolean"
  defp scalar_kind(term) when is_list(term), do: "collection"
  defp scalar_kind(term) when is_map(term), do: "map"
  defp scalar_kind(_term), do: "value"

  # Map a live term to the spike Inspector's value-kind class (inspector.jsx
  # `valueClass`), driving the type-chip / value colour. Object references render
  # as `ref` (the drillable, follow-able class); scalars map to their CSS class.
  # A boolean must be matched before the integer guard (`is_boolean` ⊂ atoms, not
  # integers, but kept explicit and first for clarity).
  defp term_kind({:beamtalk_object, _class, _module, pid}) when is_pid(pid), do: "ref"
  defp term_kind(term) when is_boolean(term), do: "bool"
  defp term_kind(term) when is_integer(term) or is_float(term), do: "int"
  defp term_kind(term) when is_binary(term), do: "string"
  defp term_kind(term) when is_atom(term), do: "symbol"
  defp term_kind(_term), do: "value"

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
    codeFont: "IBM Plex Mono"
  }

  # The curated accent swatches (paper/squeak only — dusk keeps its built-in
  # accent), UI-font and code-font options — exactly the sets the spike offers.
  @tweak_accents ~w(#b9711b #a8324e #2c6e8e #5d7a2e #7a4ea8)
  @tweak_ui_fonts ["Hanken Grotesk", "Inter Tight", "Public Sans", "Schibsted Grotesk"]
  @tweak_code_fonts ["IBM Plex Mono", "JetBrains Mono", "Spline Sans Mono", "Courier Prime"]

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
  attr :selected_class, :string, default: nil
  attr :browser_error, :string, default: nil

  defp system_browser_classes(assigns) do
    ~H"""
    <div id="system-browser" class="panel" style="flex:1;">
      <div class="panel-head">
        System Browser <span class="spacer"></span>
        <div class="seg" role="tablist" aria-label="Class tree view">
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
      </div>
      <div class="panel-body">
        <div :if={@browser_error} class="io-block err">{@browser_error}</div>
        <%= if @browser_classes == [] do %>
          <p :if={!@browser_error} class="muted-note">No classes in the image yet.</p>
        <% else %>
          <div class="tree">
            <%= if @browser_view == "category" do %>
              <div :for={{category, rows} <- category_groups(@browser_classes)} class="cat-group">
                <div class="cat-row">{category}</div>
                <.class_rows
                  rows={Enum.map(rows, &{&1, 1})}
                  selected_class={@selected_class}
                  browser_side={@browser_side}
                />
              </div>
            <% else %>
              <.class_rows
                rows={hierarchy_rows(@browser_classes)}
                selected_class={@selected_class}
                browser_side={@browser_side}
              />
            <% end %>
          </div>
        <% end %>
      </div>
      <div class="actionbar sb-side">
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
    </div>
    """
  end

  # Render a list of `{class_row, indent}` tuples — shared by the Hierarchy and
  # Category views. The selected class is highlighted; an indented row reads as a
  # subclass. A runtime-only class is badged; the class-side selection shows a
  # `class` pill so the side is visible in the tree.
  attr :rows, :list, required: true
  attr :selected_class, :string, default: nil
  attr :browser_side, :string, required: true

  defp class_rows(assigns) do
    ~H"""
    <div
      :for={{class, indent} <- @rows}
      class={[
        "row",
        indent == 2 && "subclass2",
        indent == 1 && "subclass",
        @selected_class == class["name"] && "sel"
      ]}
      phx-click="browser_select_class"
      phx-value-class={class["name"]}
      title={class["name"]}
    >
      <span class="twig">{if class["superclass"], do: "→", else: "●"}</span>
      <span class="cls">{class["name"]}</span>
      <span :if={runtime_only?(class)} class="runtime-tag" title="runtime-only (not on disk)">
        runtime
      </span>
      <span :if={@selected_class == class["name"] and @browser_side == "class"} class="pill">
        class
      </span>
    </div>
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
  attr :browser_method, :map, default: nil

  defp system_browser_methods(assigns) do
    assigns =
      assigns
      |> assign(:methods, filtered_methods(assigns.browser_protocols, assigns.selected_protocol))
      |> assign(:total_methods, protocol_method_count(assigns.browser_protocols))

    ~H"""
    <div class="panel" style="flex:1;">
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
                @browser_method && @browser_method["selector"] == m["selector"] &&
                  @browser_method["side"] == @browser_side && "sel"
              ]}
              phx-click="browser_select_method"
              phx-value-class={@selected_class}
              phx-value-side={@browser_side}
              phx-value-selector={m["selector"]}
            >
              <span class="twig" style="color: var(--accent);">ƒ</span>
              <span class="mname mono">{m["selector"]}</span>
              <span :if={runtime_only?(m)} class="runtime-tag">runtime</span>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  # The center method-source display (BT-2491): the selected method's
  # image-accurate source, read-only, with a `Class instance » selector ·
  # category` breadcrumb and image-diverged badges (runtime-only / unflushed
  # patch). Read-only is enough until the tabbed editor consumes it in a
  # follow-up issue. Renders nothing until a method is selected.
  attr :browser_method, :map, default: nil

  defp browser_method_source(assigns) do
    ~H"""
    <div :if={@browser_method} id="browse-method-source" class="panel" style="order:3; flex:none;">
      <div class="panel-head">
        Method Source <span class="spacer"></span>
        <span
          :if={@browser_method["disk_differs"] == true}
          class="runtime-tag"
          title="unflushed live patch"
        >
          unflushed
        </span>
        <span
          :if={runtime_only?(@browser_method)}
          class="runtime-tag"
          title="runtime-only (not on disk)"
        >
          runtime
        </span>
      </div>
      <div class="editor-meta">
        <% {bc_class, bc_side, bc_sel, bc_cat} = browse_breadcrumb(@browser_method) %>
        <span class="crumb">
          <b>{bc_class}</b>
          <span class="mono sep-side">{bc_side}</span>
          <span class="sep">»</span>
          <span class="mono">{bc_sel}</span>
          <span :if={bc_cat} class="meta-note">· {bc_cat}</span>
        </span>
      </div>
      <div class="panel-body">
        <%= if @browser_method["source"] do %>
          <pre class="bt-source mono">{@browser_method["source"]}</pre>
        <% else %>
          <p class="muted-note">No source — runtime-only method (defined in the live image).</p>
        <% end %>
      </div>
    </div>
    """
  end

  # The breadcrumb parts for a selected method: `Class instance » selector ·
  # category` (BT-2491, the spike's crumb at app.jsx:401). `side` renders as the
  # word "class"/"instance"; `category` is the method's protocol (nil when
  # unknown, then the breadcrumb omits the `· category` segment).
  defp browse_breadcrumb(method) do
    {method["class"], side_label(method["side"]), method["selector"], method["protocol"]}
  end

  defp side_label("class"), do: "class"
  defp side_label(_), do: "instance"

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
          <span class="spacer"></span>
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
          <%!-- ── three-column cockpit grid ──────────────────────────────── --%>
          <div class="cockpit">
            <%!-- LEFT — System Browser (BT-2491, 286px) + Tweaks panel.
                 A class tree (Hierarchy / Category views, instance/class side
                 toggle) over a protocol-grouped method list, driven by the
                 BT-2488 browse ops (ADR 0096). --%>
            <div class="col">
              <div class="browser-split">
                <.system_browser_classes
                  browser_view={@browser_view}
                  browser_side={@browser_side}
                  browser_classes={@browser_classes}
                  selected_class={@selected_class}
                  browser_error={@browser_error}
                />
                <.system_browser_methods
                  browser_protocols={@browser_protocols}
                  selected_protocol={@selected_protocol}
                  selected_class={@selected_class}
                  browser_side={@browser_side}
                  browser_method={@browser_method}
                />
              </div>

              <.tweaks_panel />
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
              <div class="dock" style="order:2;">
                <div id="workspace-dock" class="panel">
                  <div class="panel-head">
                    <span class="dock-tabs" role="tablist">
                      <button
                        :for={
                          {tab, label} <- [
                            {"workspace", "Workspace"},
                            {"transcript", "Transcript"},
                            {"changes", "Changes"}
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
                  </div>

                  <%!-- WORKSPACE tab: highlighted editor + doIt/printIt/inspectIt --%>
                  <div class="dock-pane ws-pane" hidden={@dock_tab != "workspace"}>
                    <%= if @role == :owner do %>
                      <%!-- eval form: the FIRST <form> on the page. The CodeEditor
                           overlay (BT-2485) highlights the entered code; the
                           textarea keeps name="expr" so the existing `eval`
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
                        <div
                          id="workspace-editor-overlay"
                          class="bt-editor-wrap ws-wrap"
                          phx-hook="CodeEditor"
                        >
                          <pre class="bt-editor-pre" aria-hidden="true"><code></code></pre>
                          <textarea
                            id="workspace-editor-source"
                            class="bt-editor-ta"
                            name="expr"
                            phx-hook="SelectionTracker"
                            data-select-event="select_workspace"
                            spellcheck="false"
                            autocomplete="off"
                          ><%= @expr %></textarea>
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
                    <div :if={@output} class="io-block">{@output}</div>
                    <div :if={@result} class="ws-result">
                      <span class="arrow">→</span>
                      <span class="val">{@result}</span>
                    </div>
                    <div :if={@error} class="ws-result err">
                      <span class="arrow">→</span>
                      <span class="val">{@error}</span>
                    </div>
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
                    <div :if={@changes_error} class="io-block err">{@changes_error}</div>
                    <%= if @changes == [] do %>
                      <p class="muted-note">No pending changes. Save a method to record one.</p>
                    <% else %>
                      <table class="bt-table">
                        <thead>
                          <tr>
                            <th>Class</th>
                            <th>Selector</th>
                            <th>Intent</th>
                            <th>Flushable</th>
                            <th>Author</th>
                          </tr>
                        </thead>
                        <tbody>
                          <tr :for={c <- @changes}>
                            <td class="k">{c.class}</td>
                            <td>{c.selector}</td>
                            <td>{c.intent}</td>
                            <td>{if c.flushable, do: "yes", else: "no"}</td>
                            <td>{c.author_kind}</td>
                          </tr>
                        </tbody>
                      </table>
                    <% end %>
                  </div>
                </div>
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
                      {if t.kind == :def, do: t.class <> " ▸ def", else: t.selector}
                    </span>
                    <span
                      :if={length(@tabs) > 1}
                      class="x"
                      title="Close tab"
                      phx-click="tab_close"
                      phx-value-id={t.id}
                    >
                      ×
                    </span>
                  </button>
                  <span class="spacer"></span>
                  <%!-- "+ def" opens (or focuses) the active class's definition
                       tab — saving it compiles the class (ADR 0082). --%>
                  <button
                    :if={@role == :owner}
                    type="button"
                    class="tab tab-add"
                    title="Open class definition"
                    phx-click="open_definition"
                  >
                    + def
                  </button>
                </div>

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
                  <span class="spacer"></span>
                  <span :if={@role != :owner} class="meta-note read-only">read-only · Observer</span>
                  <span :if={@role == :owner and active_tab(assigns).dirty} class="meta-note edited">
                    edited — ⌘S to compile
                  </span>
                  <span :if={@role == :owner and not active_tab(assigns).dirty} class="meta-note">
                    in image
                  </span>
                </div>

                <div class="panel-body">
                  <%= if @role == :owner do %>
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
                      <%!-- a method tab edits class + selector inline; a class-
                           definition tab carries them as hidden fields (the class
                           name) so the form payload shape is unchanged. --%>
                      <%= if active_tab(assigns).kind == :def do %>
                        <input type="hidden" name="class" value={@edit_class} />
                        <input type="hidden" name="selector" value="▸ class definition" />
                      <% else %>
                        <div style="display:flex; gap:.5rem; margin-bottom:.5rem;">
                          <input
                            class="field"
                            name="class"
                            value={@edit_class}
                            placeholder="Class (e.g. Counter)"
                            autocomplete="off"
                            style="flex:1;"
                          />
                          <input
                            class="field"
                            name="selector"
                            value={@edit_selector}
                            placeholder="selector (e.g. increment)"
                            autocomplete="off"
                            style="flex:1;"
                          />
                        </div>
                      <% end %>
                      <%!-- syntax-highlighting editor overlay (BT-2485). Re-keyed
                           on the active tab id (`@active_tab`) so switching tabs
                           replaces the element and the textarea picks up the new
                           tab's source. The textarea keeps name="source" so
                           save_method reads it. --%>
                      <div
                        id={"method-editor-overlay-" <> @active_tab}
                        class="bt-editor-wrap field"
                        style="display:block; height:6rem; padding:0;"
                        phx-hook="CodeEditor"
                      >
                        <pre class="bt-editor-pre" aria-hidden="true"><code></code></pre>
                        <textarea
                          id={"method-editor-source-" <> @active_tab}
                          class="bt-editor-ta"
                          name="source"
                          phx-debounce="300"
                          phx-hook="SelectionTracker"
                          data-select-event="select_source"
                          placeholder={
                            if active_tab(assigns).kind == :def,
                              do: "Actor subclass: Counter\n  state: value = 0",
                              else: "increment => self.value := self.value + 1"
                          }
                        ><%= @edit_source %></textarea>
                      </div>
                      <div style="display:flex; gap:.5rem; margin-top:.5rem;">
                        <button class="btn primary" type="submit">
                          Compile <span class="k">⌘S</span>
                        </button>
                        <button class="btn" type="button" phx-click="flush">
                          Save All to Disk (flush)
                        </button>
                      </div>
                    </form>
                  <% else %>
                    <p class="muted-note">
                      Your role is read-only — evaluation and editing are disabled. You can
                      still browse bindings, follow references in the Inspector, and watch
                      the live Transcript.
                    </p>
                  <% end %>

                  <div :if={@save_result} class="io-block ok">{@save_result}</div>
                  <div :if={@save_error} class="io-block err">{@save_error}</div>
                  <div :if={@flush_result} class="io-block warn">{@flush_result}</div>
                  <div :if={@flush_error} class="io-block err">{@flush_error}</div>
                </div>
              </div>

              <.browser_method_source browser_method={@browser_method} />
            </div>

            <%!-- RIGHT — Bindings + Inspector (348px), with ChangeLog + Transcript --%>
            <div class="col">
              <div class="right-split">
                <div id="bindings-panel" class="panel bindings-panel">
                  <div class="panel-head">
                    Bindings <span class="spacer"></span>
                    <span class="count">{length(@bindings)} in session</span>
                  </div>
                  <div class="panel-body">
                    <div :if={@bindings_error} class="io-block err">{@bindings_error}</div>
                    <%= if @bindings == [] do %>
                      <p class="muted-note">No bindings yet. Try <code>x := 42</code>.</p>
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
                    <div :if={@inspect_error} class="io-block warn">{@inspect_error}</div>
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
                    <% end %>
                    <%!-- Owner-only poke bar (BT-2492, spike PokeBar): send a Beamtalk
                         message to the inspected actor. Rendered only for a pid-backed
                         target at a pokeable root (a single named-binding crumb) AND
                         the owner role — an Observer's eval is refused by RBAC, so the
                         bar is hidden for them (a crafted poke is still refused
                         server-side), and a drilled field has no session binding to
                         address. Sends `<binding> <message>` via eval. --%>
                    <div
                      :if={@inspect_target && @inspect_target.pid && @role == :owner && pokeable?(assigns)}
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
end

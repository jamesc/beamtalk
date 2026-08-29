# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.SystemBrowser do
  @moduledoc """
  The System Browser pane (BT-2491, epic BT-2482 Phase 2) — class tree,
  instance/class side toggle, protocol + method list, method source opening,
  and the source-navigation affordances (hover/goto-definition/senders/
  implementors) — extracted out of `BtAttachWeb.WorkspaceLive` (BT-3297,
  epic BT-3290) so its `handle_event/3` clauses and the browse/navigation
  data model they drive are directly unit-testable instead of only reachable
  through a full-LiveView integration test. Follows the same extraction shape
  `BtAttachWeb.Live.Inspector` (BT-3291), `BtAttachWeb.Live.Dock` (BT-3295),
  and `BtAttachWeb.Live.MethodEditor` (BT-3296) established.

  This module owns:

    * **Class tree** — two toggleable views (`"browser_view"`): Hierarchy
      (indented by superclass depth) and Category (grouped by the class
      annotation). Selecting a class (`"browser_select_class"`) fetches its
      protocols for the current side. Hierarchy layout itself is NOT
      reimplemented here — it delegates to `BtAttachWeb.ClassTree`
      (`hierarchy_rows_with_context/2`), the existing extraction precedent.
    * **Instance / class side toggle** (`"browser_side"`) — re-populates the
      protocol + method list for the flipped side.
    * **Protocol + method list** — `"browser_select_protocol"` (the filter
      row), `"browser_select_method"` (opens the method editor tab via
      `BtAttachWeb.Live.MethodEditor.open_method_tab/4`), and the grouped
      method view's `// === Name ===` section-divider events
      (`"browser_group_mode"`, `"browser_edit_section"`,
      `"browser_cancel_section"`, `"browser_rename_section"`,
      `"browser_add_section"`).
    * **Class definitions + Native browser** — `"browser_open_definition"`,
      `"browser_open_native"`, `"browser_mode"`, `"browser_open_native_module"`,
      `"browser_jump_native"`.
    * **Type Aliases panel** — `"browser_open_alias"` (BT-3314: toggle a
      read-only source view inline under the clicked alias row, mirroring
      `"browser_open_native"`'s inline toggle) and `"dismiss_alias_error"`.
    * **Panel visibility** — `"close_browser"`, `"toggle_browser"`.
    * **Source-navigation affordances** — `"complete"`/`"hover"`/
      `"diagnostics"` (the CodeMirror editors' backend-driven autocomplete,
      live-image hover, and parse-only lint sources), `"senders"` /
      `"implementors"` / `"native_callers"` / `"required_methods"` /
      `"conforming_classes"` (the Senders/Implementors/protocol-action result
      popover), `"nav_open"` / `"nav_open_class"` / `"nav_required_open"` /
      `"nav_close"` / `"dismiss_nav_error"` (opening/dismissing that
      popover), and `"goto_definition"` (Ctrl/Cmd-click go-to-definition,
      BT-2666).

  Every workspace read/write goes through `BtAttach.Facade.dispatch/3` (ADR
  0091 Decision 3) with `BtAttachWeb.Live.RequestContext` — never a raw
  `BtAttach.Workspace`/`:rpc` call — so this module never reimplements the
  `browse_*`/`senders`/`implementors`/… ops or the RBAC gates they ride
  (CLAUDE.md no-duplicate-implementations), and reuses `BtAttachWeb.ClassTree`
  directly for the Hierarchy walk rather than duplicating it.

  State (`:browser_view`, `:browser_side`, `:browser_source`,
  `:browser_source_chosen`, `:selected_class`, `:selected_protocol`,
  `:browser_protocols`, `:browser_error`, `:browser_categories`,
  `:browser_group_mode`, `:editing_section`, `:section_form_error`,
  `:native_view`, `:browser_mode`, `:browser_native_modules`,
  `:browser_type_aliases`, `:alias_view`, `:native_source`,
  `:native_source_chosen`, `:nav_popover`, `:browser_classes`) stays on the
  LiveView's own socket —
  initialised in `WorkspaceLive.bind_session/3` and mount, same as the
  Dock/Inspector/MethodEditor assigns. `WorkspaceLive` still owns
  `handle_event/3` (`Phoenix.LiveView` callback contracts) and `render/1`
  (the System Browser panel markup is woven together with the top-bar omni
  search and the still-resident New Class / Rename / Remove UI, so it does
  not split cleanly along this extraction's event boundary — see
  `Dock`'s/`Inspector`'s/`MethodEditor`'s moduledocs for the same call), but
  delegates every System Browser event to the functions here by name — see
  the `@system_browser_events` guard clause in `WorkspaceLive`, which reads
  its event list from `__system_browser_events__/0` below (mirroring the
  BT-3301 fix that keeps `WorkspaceLive` from hand-maintaining a second copy
  of the event names).

  The top-bar omni search (`"omni_search"`/`"omni_open"`/`"omni_close"`) is
  a *different* navigator (a flat symbol index over every loaded class +
  selector, not the tree/protocol/method browse ops) and stays
  `WorkspaceLive`-owned; `open_class/2` here (which the omni search's class
  result and `BtAttachWeb.Live.Dock`'s REPL `:help` meta-command both
  cross-call) is the shared "point the System Browser at this class" primitive
  the two navigators meet at.
  """

  use BtAttachWeb, :html

  import Phoenix.LiveView, only: [put_flash: 3]

  require Logger

  alias BtAttach.Facade
  alias BtAttachWeb.Live.FacadeError
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.RequestContext

  # ── handle_event dispatch ────────────────────────────────────────────────
  #
  # `WorkspaceLive.handle_event/3` forwards every event whose name is in
  # `@system_browser_events` (read from `__system_browser_events__/0` below)
  # here unchanged (same event name, params, socket), so each clause below is
  # exactly the body the LiveView used to run directly.
  @system_browser_events ~w(
    complete hover diagnostics
    close_browser toggle_browser
    browser_open_definition browser_open_native browser_mode
    browser_open_native_module browser_jump_native
    browser_open_alias dismiss_alias_error
    browser_view browser_source browser_side browser_select_class
    browser_select_protocol browser_select_method browser_group_mode
    browser_edit_section browser_cancel_section browser_rename_section
    browser_add_section
    senders implementors native_callers required_methods conforming_classes
    nav_open_class nav_required_open nav_open nav_close goto_definition
    dismiss_nav_error
  )

  @doc false
  def __system_browser_events__, do: @system_browser_events

  # Backend-driven autocomplete (BT-2544): the CodeMirror editors' autocomplete
  # CompletionSource round-trips the current line up to the caret; we answer with
  # ranked candidates from the live session via the `complete` op (receiver-aware:
  # class names, selectors for a known receiver, session bindings, pseudo-vars).
  # `:read` capability — completion runs no user code — so the Observer role
  # completes too. We `{:reply, …}` on the same event so CodeMirror resolves its
  # async source; an unreachable workspace, a denied op, or a missing session all
  # degrade to an empty list (autocomplete simply shows nothing).
  def handle_event("complete", %{"code" => code}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) and is_binary(code) do
    completions =
      case Facade.dispatch(
             :complete,
             %{session_pid: pid, code: code},
             RequestContext.build(socket)
           ) do
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
  def handle_event("hover", %{"code" => code}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) and is_binary(code) do
    hover =
      case Facade.dispatch(:hover, %{session_pid: pid, code: code}, RequestContext.build(socket)) do
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
             RequestContext.build(socket)
           ) do
        {:ok, list} when is_list(list) -> list
        _ -> []
      end

    {:reply, %{"diagnostics" => diagnostics}, socket}
  end

  def handle_event("diagnostics", _params, socket) do
    {:reply, %{"diagnostics" => []}, socket}
  end

  # Panel visibility toggles (BT-2559): close/toggle the System Browser panel.
  def handle_event("toggle_browser", _params, socket) do
    {:noreply, assign(socket, show_browser: !socket.assigns.show_browser)}
  end

  def handle_event("close_browser", _params, socket) do
    {:noreply, assign(socket, show_browser: false)}
  end

  # Open the *selected* class's definition tab from the System Browser's "class
  # definition" entry (BT-2491). The class rides the click; a malformed payload
  # is ignored rather than crashing the LiveView.
  def handle_event("browser_open_definition", %{"class" => class}, socket)
      when is_binary(class) and class != "" do
    {:noreply, MethodEditor.open_definition(socket, class)}
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

  # BT-3314: toggle the read-only source view inline under a clicked Type
  # Aliases row, mirroring `browser_open_native`'s inline toggle (a type alias
  # has no compiled module to back a first-class editor tab the way
  # `browser_open_native_module` opens one — see `load_alias_view/2`'s doc).
  # `package` disambiguates a same-named alias declared by more than one
  # package (`browse_type_aliases/0`'s no-dedupe note); a re-click on the same
  # row collapses it.
  def handle_event("browser_open_alias", %{"name" => name} = params, socket)
      when is_binary(name) and name != "" do
    package = nonempty_string(Map.get(params, "package"))

    if alias_shown?(socket.assigns, name, package) do
      {:noreply, assign(socket, alias_view: nil)}
    else
      {:noreply, assign(socket, alias_view: load_alias_view(socket, name, package))}
    end
  end

  def handle_event("browser_open_alias", _params, socket), do: {:noreply, socket}

  # Dismiss the error inside the live alias-source pane: `@alias_view` is a
  # map whose `:error` field carries the banner. Clear only that field so the
  # rest of the pane (content/meta) is preserved; if the pane is closed
  # (`alias_view: nil`) this is a no-op.
  def handle_event("dismiss_alias_error", _params, socket),
    do: {:noreply, dismiss_pane_error(socket, :alias_view)}

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

  # BT-2903: switch into the "Type Aliases" panel, re-fetching so an alias
  # declared mid-session is discoverable — mirroring the Native mode switch.
  def handle_event("browser_mode", %{"mode" => "aliases"}, socket) do
    {:noreply, assign(assign_browser_type_aliases(socket), browser_mode: :aliases)}
  end

  def handle_event("browser_mode", _params, socket), do: {:noreply, socket}

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

  # BT-2578: jump from a `self delegate` method to its Erlang implementation.
  # Opens (or focuses) the class-definition tab and expands the native pane with
  # the method's selector resolved to its matching `handle_call` clause (which the
  # pane highlights). The selector→clause mapping is best-effort: a delegate that
  # completes in `handle_info` resolves to no clause, and the pane says so rather
  # than pretending.
  def handle_event("browser_jump_native", %{"class" => class, "selector" => selector}, socket)
      when is_binary(class) and class != "" and is_binary(selector) do
    socket = MethodEditor.open_definition(socket, class)
    {:noreply, assign(socket, native_view: load_native_view(socket, class, selector))}
  end

  def handle_event("browser_jump_native", _params, socket), do: {:noreply, socket}

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
        assign(socket,
          selected_class: nil,
          selected_protocol: nil,
          browser_protocols: [],
          browser_categories: default_categories(),
          browser_group_mode: "protocol",
          editing_section: nil,
          section_form_error: nil
        )
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
      |> load_protocols(class, socket.assigns.browser_side)
      |> load_categories(class)

    {:noreply, socket}
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
    {:noreply, MethodEditor.open_method_tab(socket, class, side, selector)}
  end

  def handle_event("browser_select_method", _params, socket), do: {:noreply, socket}

  # ── grouped method view: `// === Name ===` section dividers (BT-3238) ──────

  # Toggle the method list between the existing per-protocol grouping and the
  # divider-category grouping. The UI only renders this toggle when
  # `browser_categories["has_dividers"]` is true (a divider-free class never
  # shows it, so its method list renders exactly as before), but the handler
  # itself just no-ops on an unrecognised mode rather than trusting that gate.
  def handle_event("browser_group_mode", %{"mode" => mode}, socket)
      when mode in ~w(protocol section) do
    {:noreply,
     assign(socket, browser_group_mode: mode, editing_section: nil, section_form_error: nil)}
  end

  def handle_event("browser_group_mode", _params, socket), do: {:noreply, socket}

  # Open the inline section form: renaming an existing section (`name` is
  # that section's current divider text) or adding a new one (`name` is
  # empty, the `:new` sentinel). Owner-only, matching `new_method`'s gate —
  # authoring is Owner-only, so the entry is rendered only for `:owner` and a
  # crafted event from a read-only role is a no-op.
  def handle_event(
        "browser_edit_section",
        %{"name" => name},
        %{assigns: %{role: :owner}} = socket
      )
      when is_binary(name) do
    target = if name == "", do: :new, else: name
    {:noreply, assign(socket, editing_section: target, section_form_error: nil)}
  end

  def handle_event("browser_edit_section", _params, socket), do: {:noreply, socket}

  # Close the inline section form without saving.
  def handle_event("browser_cancel_section", _params, socket) do
    {:noreply, assign(socket, editing_section: nil, section_form_error: nil)}
  end

  # Rename an existing section: `old_name` identifies the divider being
  # edited, `new_name` is the submitted form text.
  def handle_event(
        "browser_rename_section",
        %{"old_name" => old_name, "new_name" => new_name},
        %{assigns: %{role: :owner, selected_class: class}} = socket
      )
      when is_binary(old_name) and is_binary(new_name) and is_binary(class) do
    {:noreply, submit_section(socket, class, new_name, old_name: old_name)}
  end

  def handle_event("browser_rename_section", _params, socket), do: {:noreply, socket}

  # Add a brand-new section directly above `before_selector` (the method
  # picked in the inline "add section" form's dropdown).
  def handle_event(
        "browser_add_section",
        %{"new_name" => new_name, "before_selector" => before_selector} = params,
        %{assigns: %{role: :owner, selected_class: class, browser_side: side}} = socket
      )
      when is_binary(new_name) and is_binary(before_selector) and is_binary(class) do
    before_side = Map.get(params, "before_side", side)

    {:noreply,
     submit_section(socket, class, new_name,
       before_selector: before_selector,
       before_side: before_side
     )}
  end

  def handle_event("browser_add_section", _params, socket), do: {:noreply, socket}

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
      |> MethodEditor.open_definition(class)
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
      |> MethodEditor.open_method_tab(class, side, selector)
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

  # Dismiss the error inside the Senders/Implementors popover without closing the
  # whole popover (which `nav_close` does). `@nav_popover` is a map; clear only
  # its `:error` field.
  def handle_event("dismiss_nav_error", _params, socket) do
    case socket.assigns[:nav_popover] do
      %{} = nav -> {:noreply, assign(socket, nav_popover: Map.put(nav, :error, nil))}
      _ -> {:noreply, socket}
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
  #
  # Split into the off-socket read + pure fold (BT-2591) so the mount-load async
  # task and the post-action refresh callers share one fold. Public:
  # `BtAttachWeb.Live.MethodEditor`'s `refresh_after_source_change/1` (BT-3296)
  # and `WorkspaceLive`'s Remove/Rename/New-Class code call it directly.
  def assign_browser_classes(socket),
    do: apply_browser_classes(socket, read_browser_classes(socket))

  defp read_browser_classes(socket),
    do: Facade.dispatch(:browse_classes, %{}, RequestContext.build(socket))

  # Public: `WorkspaceLive.handle_async(:mount_load, …)` folds the off-socket
  # mount read through this directly (`&SystemBrowser.apply_browser_classes/2`).
  def apply_browser_classes(socket, {:value, rows}) when is_list(rows) do
    socket
    |> assign(browser_classes: rows, browser_error: nil)
    |> apply_default_browser_source(rows)
  end

  def apply_browser_classes(socket, {:error, reason}),
    do: assign(socket, browser_classes: [], browser_error: FacadeError.render(reason))

  # Defensive catch-all (BT-2591): this fold runs in `handle_async(:mount_load,
  # …)` AND on the sync `assign_browser_classes/1` refresh path. An unexpected
  # dispatch shape (a facade evolution, a bare atom) would otherwise crash the
  # LiveView — on the async path *after* the empty page rendered, harder to spot
  # than the old mount-time crash. Degrade to an empty tree with an error instead.
  def apply_browser_classes(socket, unexpected) do
    Logger.warning("unexpected browse_classes result: #{inspect(unexpected)}",
      domain: [:beamtalk, :liveview]
    )

    assign(socket, browser_classes: [], browser_error: FacadeError.render(:unexpected_response))
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
  # primary navigator) must still render. Public: `WorkspaceLive.bind_session/3`
  # calls it directly at mount.
  def assign_browser_native_modules(socket) do
    rows =
      case Facade.dispatch(:browse_native_modules, %{}, RequestContext.build(socket)) do
        {:value, rows} when is_list(rows) -> rows
        _ -> []
      end

    socket
    |> assign(browser_native_modules: rows)
    |> apply_default_native_source(rows)
  end

  # BT-2903 (ADR 0108 Phase 8): load every loaded package's declared `type`
  # aliases for the "Type Aliases" panel. Each row carries `name`,
  # `expansion`, `doc`, `source_file`, `internal`, `package`, and
  # `source_origin`. A dispatch failure / RBAC denial yields an empty list
  # rather than crashing the browser — the class tree (the primary
  # navigator) must still render. No client-side origin filter: unlike
  # native modules, a dependency's `internal` alias is already excluded
  # server-side at the seeding boundary, so there is nothing left to filter.
  # Public: `WorkspaceLive.bind_session/3` calls it directly at mount.
  def assign_browser_type_aliases(socket) do
    rows =
      case Facade.dispatch(:browse_type_aliases, %{}, RequestContext.build(socket)) do
        {:value, rows} when is_list(rows) -> rows
        _ -> []
      end

    assign(socket, browser_type_aliases: rows)
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
    case Facade.dispatch(
           :browse_protocols,
           %{class: class, side: side},
           RequestContext.build(socket)
         ) do
      {:value, %{"protocols" => protocols}} when is_list(protocols) ->
        assign(socket, browser_protocols: protocols, browser_error: nil)

      {:value, _other} ->
        assign(socket, browser_protocols: [], browser_error: nil)

      {:error, reason} ->
        assign(socket, browser_protocols: [], browser_error: FacadeError.render(reason))
    end
  end

  # Load `class`'s methods grouped by `// === Name ===` section-divider
  # category (op `browse-categories`, BT-3238) for the grouped method view.
  # Class-wide (not per-side, unlike protocols) — a category can hold both
  # instance- and class-side methods. Resets the group-mode toggle back to
  # "protocol" and clears any in-progress section edit whenever the class
  # changes, so switching classes never leaves a stale rename form open. A
  # dispatch failure degrades to the default "unaffected" shape (`browser_categories`
  # is a rendering aid, not something worth surfacing its own error banner for
  # — the protocol/method list is still fully usable).
  defp load_categories(socket, class) do
    socket
    |> refresh_categories(class)
    |> assign(browser_group_mode: "protocol", editing_section: nil, section_form_error: nil)
  end

  # BT-3238: re-fetch `browse-categories` WITHOUT resetting `browser_group_mode`
  # / `editing_section` — the post-save refresh path (`submit_section/4`) uses
  # this alone, not `load_categories/2`, so a successful rename/add doesn't
  # kick the viewer back to Protocol mode right after they used Sections mode
  # to get there (review finding: `load_categories/2` used to reset the mode
  # unconditionally on every call, including this one).
  defp refresh_categories(socket, class) do
    view =
      case Facade.dispatch(:browse_categories, %{class: class}, RequestContext.build(socket)) do
        {:value, %{"has_dividers" => _, "categories" => _} = view} -> view
        _ -> default_categories()
      end

    assign(socket, browser_categories: view)
  end

  # Public: `WorkspaceLive.bind_session/3` calls it directly at mount.
  def default_categories, do: %{"has_dividers" => false, "categories" => []}

  # BT-3238: flattens `browse-categories`' category list into one ordered
  # list of `side`-only method rows, in source order across every category —
  # the section-mode method list renders from this (filtered per-category at
  # render time), and the "has any methods at all" empty-state check uses it
  # unfiltered. Public: `WorkspaceLive`'s `system_browser_methods/1` render
  # component calls it directly.
  def category_methods_for_side(categories, side) when is_list(categories) do
    Enum.flat_map(categories, fn category ->
      Enum.filter(category["methods"] || [], &(&1["side"] == side))
    end)
  end

  def category_methods_for_side(_categories, _side), do: []

  # BT-3238: the "add a new section" form's before-selector dropdown draws
  # from this, NOT the full `category_methods_for_side/2` list — it excludes
  # each named category's own first method. Inserting a new divider directly
  # above a method that already starts a category would write two dividers
  # back-to-back with nothing between them; `find_divider_span`'s own doc
  # (`method_category.rs`) says only the nearer one survives, so the
  # existing category's divider would be silently orphaned (its methods
  # merging into whatever category preceded it) rather than cleanly split.
  # Every method in the implicit (unnamed) leading group stays a valid
  # insertion point — there is no divider there yet to collide with — and so
  # does every method after a category's first, since a new divider there
  # cleanly splits that category into two. Public: `WorkspaceLive`'s
  # `system_browser_methods/1` render component calls it directly.
  def insertable_methods_for_side(categories, side) when is_list(categories) do
    Enum.flat_map(categories, fn category ->
      same_side = Enum.filter(category["methods"] || [], &(&1["side"] == side))

      case category["name"] do
        nil ->
          same_side

        _named ->
          # A category's "first method" is its first method OVERALL (any
          # side), matching the server-side collision guard
          # (`starts_named_category/3`, `beamtalk_repl_ops_load.erl`) — not
          # the first *same-side* method. A category can legitimately open
          # with a class-side method and continue with instance-side ones;
          # filtering by side before dropping "the first" would wrongly
          # treat that instance-side method as the category's start and
          # exclude it, when the server would happily accept it.
          case category["methods"] || [] do
            [first_overall | _] -> Enum.reject(same_side, &(&1 == first_overall))
            [] -> []
          end
      end
    end)
  end

  def insertable_methods_for_side(_categories, _side), do: []

  # BT-3238: dispatch `save-section` (rename via `old_name:`, or insert via
  # `before_selector:`/`before_side:` — the caller passes exactly one shape
  # via `opts`) and, on success, close the inline form and refresh the
  # grouped view so the new/renamed divider shows up immediately. A failure
  # surfaces inline on the form (`section_form_error`) rather than the
  # page-wide `@save_error` notice, since it's scoped to the small form, not
  # the whole editor.
  defp submit_section(socket, class, new_name, opts) do
    params = Map.merge(%{class: class, new_name: new_name}, Map.new(opts))

    case Facade.dispatch(:save_section, params, RequestContext.build(socket)) do
      {:value, %{"ok" => true}} ->
        socket
        |> assign(editing_section: nil, section_form_error: nil)
        |> refresh_categories(class)

      {:error, reason} ->
        assign(socket, section_form_error: FacadeError.render(reason))

      _other ->
        assign(socket, section_form_error: "Could not save the section.")
    end
  end

  # Open a class in the System Browser (the omni-search "class" result): select it
  # in the tree and load its protocols for the current side, exactly as a click in
  # the class tree would. Public: `WorkspaceLive`'s own `omni_open` handler and
  # `BtAttachWeb.Live.Dock`'s REPL `:help` / `Beamtalk help:` follow-up (BT-3295)
  # call it directly (the omni search itself stays `WorkspaceLive`-owned — see
  # this module's `@moduledoc`).
  def open_class(socket, class) do
    socket
    |> assign(selected_class: class, selected_protocol: nil)
    |> load_protocols(class, socket.assigns.browser_side)
    |> load_categories(class)
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
    |> load_categories(class)
  end

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

    case Facade.dispatch(:browse_native_source, params, RequestContext.build(socket)) do
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
        %{base | error: FacadeError.render(reason)}

      _ ->
        %{base | error: "Could not load Erlang source."}
    end
  end

  # True when a clause row is the one a jump selected (selector + line match).
  # Public: `WorkspaceLive`'s `native_source_body/1` render component calls it
  # directly.
  def clause_active?(%{"selector" => s, "line" => l}, %{"selector" => s, "line" => l}), do: true
  def clause_active?(_clause, _selected), do: false

  defp nonempty_string(s) when is_binary(s) and s != "", do: s
  defp nonempty_string(_), do: nil

  # Normalise the Erlang `null` atom (delivered as `:null` over distribution) and
  # any non-conforming value to a clean Elixir `nil` / typed value, so template
  # guards (`is_nil/1`, `:if`) and interpolation behave (BT-2578).
  defp map_or_nil(m) when is_map(m), do: m
  defp map_or_nil(_), do: nil

  # True when the native pane is currently showing `class`'s backing source.
  # Public: `WorkspaceLive`'s render template calls it directly (the def-tab
  # native pane toggle).
  def native_shown?(%{native_view: %{class: shown}}, class), do: shown == class
  def native_shown?(_assigns, _class), do: false

  # BT-3314: fetch a declared type alias's read-only source for the inline
  # pane under its Type Aliases row, keyed by `name` (+ optional `package` to
  # disambiguate a same-named alias from a different package —
  # `browse_type_aliases/0`'s no-dedupe note). Unlike `load_native_view/3`,
  # there is no compiled module to recover an absolute path from at read time
  # — a `type Name = ...` declaration erases entirely — so project-owned
  # aliases resolve against the workspace's own project root server-side,
  # while stdlib/dependency aliases have no live path cache to resolve
  # against and degrade to `content: nil`, never an error (see
  # `beamtalk_repl_ops_browse:browse_alias_source/2`'s doc). `editable` is
  # always `false` — there is no save op for alias source.
  defp load_alias_view(socket, name, package) do
    base = %{
      name: name,
      package: package,
      source_file: nil,
      source_origin: nil,
      editable: false,
      content: nil,
      clauses: [],
      requested_selector: nil,
      selected_clause: nil,
      error: nil
    }

    case Facade.dispatch(
           :browse_alias_source,
           %{name: name, package: package},
           RequestContext.build(socket)
         ) do
      {:value, %{} = r} ->
        %{
          base
          | source_file: clean_native_path(Map.get(r, "source_file")),
            source_origin: Map.get(r, "source_origin"),
            content: nonempty_string(Map.get(r, "content"))
        }

      {:error, reason} ->
        %{base | error: FacadeError.render(reason)}

      _ ->
        %{base | error: "Could not load type alias source."}
    end
  end

  # True when the alias pane is currently showing `name`/`package`'s source.
  # Public: `WorkspaceLive`'s render template calls it directly (the Type
  # Aliases row toggle).
  def alias_shown?(%{alias_view: %{name: shown_name, package: shown_package}}, name, package),
    do: shown_name == name and shown_package == package

  def alias_shown?(_assigns, _name, _package), do: false

  # Clear the `:error` field on a single-slot socket-assign pane view
  # (`native_view`/`alias_view`) without disturbing the rest of it
  # (content/meta/clauses); a no-op when the pane is closed (assign is `nil`)
  # or absent. Shared so `MethodEditor`'s `dismiss_native_error` and this
  # module's `dismiss_alias_error` can't drift on the clear-one-field
  # contract — see CLAUDE.md's no-duplicate-implementations rule. Public:
  # `MethodEditor` (which already aliases this module) calls it directly.
  def dismiss_pane_error(socket, key) when is_atom(key) do
    case socket.assigns[key] do
      %{} = view -> assign(socket, key, Map.put(view, :error, nil))
      _ -> socket
    end
  end

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

    case Facade.dispatch(
           :browse_native_module_source,
           %{module: module},
           RequestContext.build(socket)
         ) do
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
        %{base | error: FacadeError.render(reason)}

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

    case MethodEditor.find_tab(socket, id) do
      %{} -> MethodEditor.activate_tab(socket, id)
      nil -> add_native_module_tab(socket, id, module)
    end
  end

  # Builds the 4th tab kind (`:native`) sharing `BtAttachWeb.Live.MethodEditor`'s
  # `:tabs` list/shape (see that module's "tabbed method editor data model"
  # comment). A field added/renamed on the MethodEditor side must be mirrored
  # here too (and vice versa).
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
    |> MethodEditor.sync_active(tab)
  end

  # BT-2667: the module name of the focused `:native` tab (or nil) — drives the
  # "sel" highlight on the System Browser's Native modules list so it tracks the
  # native tab the editor is showing, mirroring how the class tree tracks the
  # active class/def tab. Public: `WorkspaceLive`'s render template calls it
  # directly.
  def active_native_module(assigns) do
    case MethodEditor.active_tab(assigns) do
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
      case MethodEditor.active_tab(socket.assigns) do
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
      case Facade.dispatch(kind, %{selector: selector}, RequestContext.build(socket)) do
        {:value, %{"sites" => sites}} when is_list(sites) ->
          assign(socket, nav_popover: %{kind: kind, selector: selector, sites: sites})

        {:value, _other} ->
          assign(socket, nav_popover: %{kind: kind, selector: selector, sites: []})

        {:error, reason} ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: selector,
              sites: [],
              error: FacadeError.render(reason)
            }
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
      case MethodEditor.active_tab(socket.assigns) do
        %{kind: :def, class: class, is_protocol: true} -> class
        _ -> nil
      end

    if is_binary(protocol) and protocol != "" do
      case Facade.dispatch(kind, %{protocol: protocol}, RequestContext.build(socket)) do
        {:value, %{"sites" => sites}} when is_list(sites) ->
          assign(socket, nav_popover: %{kind: kind, selector: protocol, sites: sites})

        {:value, _other} ->
          assign(socket, nav_popover: %{kind: kind, selector: protocol, sites: []})

        {:error, reason} ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: protocol,
              sites: [],
              error: FacadeError.render(reason)
            }
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
      case MethodEditor.active_tab(socket.assigns) do
        %{kind: :native, class: class} -> class
        _ -> nil
      end

    if is_binary(module) and module != "" do
      kind = :callers_of_native_module

      case Facade.dispatch(kind, %{module: module}, RequestContext.build(socket)) do
        {:value, %{"sites" => sites}} when is_list(sites) ->
          assign(socket, nav_popover: %{kind: kind, selector: module, sites: sites})

        {:value, _other} ->
          assign(socket, nav_popover: %{kind: kind, selector: module, sites: []})

        {:error, reason} ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: module,
              sites: [],
              error: FacadeError.render(reason)
            }
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
        |> MethodEditor.open_definition(token)
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
    case Facade.dispatch(:implementors, %{selector: selector}, RequestContext.build(socket)) do
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
      |> MethodEditor.open_method_tab(class, side, selector)
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

  # Category: `{category, [class_row]}` groups, each group's classes sorted by
  # name, the groups themselves sorted by category. A class with no category falls
  # into an "(uncategorized)" bucket rather than vanishing.
  #
  # BT-2557: TestCase subclasses (`is_test`) are pulled into a dedicated "Tests"
  # bucket regardless of their package — the browser surfaces them as a category
  # so a project's tests are one click away once loaded.
  #
  # Public: `WorkspaceLive`'s `system_browser_classes/1` render component calls
  # it directly.
  def category_groups(classes) do
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
  # never silently blanked). Public: `WorkspaceLive`'s `system_browser_classes/1`
  # render component calls it directly.
  def filter_by_source(classes, "all"), do: classes

  def filter_by_source(classes, "deps") do
    Enum.filter(classes, &(Map.get(&1, "source_origin") == "dependency"))
  end

  def filter_by_source(classes, src) when src in ~w(project stdlib) do
    Enum.filter(classes, &(Map.get(&1, "source_origin") == src))
  end

  def filter_by_source(classes, _src), do: classes

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
  # by selector for stable order. Public: `WorkspaceLive`'s
  # `system_browser_methods/1` render component calls it directly.
  def filtered_methods(protocols, filter) do
    protocols
    |> Enum.filter(fn p -> filter == nil or Map.get(p, "name") == filter end)
    |> Enum.flat_map(fn p ->
      name = Map.get(p, "name")
      Enum.map(Map.get(p, "selectors", []), &Map.put(&1, "protocol", name))
    end)
    |> Enum.sort_by(&Map.get(&1, "selector"))
  end

  # Total selector count across the protocol tree (the "all" filter row's count).
  # Public: `WorkspaceLive`'s `system_browser_methods/1` render component calls
  # it directly.
  def protocol_method_count(protocols) do
    Enum.reduce(protocols, 0, fn p, acc -> acc + length(Map.get(p, "selectors", [])) end)
  end

  # A row is "runtime-only" (image-diverged, ADR 0096 / BT-2483) when its origin
  # is `runtime` — in the live image with no static/disk source. The class tree
  # and method list badge these so an observer sees what is not on disk. Public:
  # `BtAttachWeb.Live.MethodEditor`'s `method_source_info/4` (BT-3296) and
  # `WorkspaceLive`'s render template call it directly.
  def runtime_only?(%{"origin" => "runtime"}), do: true
  def runtime_only?(_), do: false

  # A selector row is "derived" (BT-2714) when its xref `source_status` is
  # `synthetic` — a compiler-synthesized method (Value accessors, `with<Field>:`
  # setters, keyword constructors, actor `new`/`spawn`) with no hand-written
  # source line. The method list badges these `derived` so the synthetic magic is
  # visible rather than indistinguishable from real methods. Public:
  # `WorkspaceLive`'s `system_browser_methods/1` render component calls it
  # directly.
  def synthetic?(%{"source_status" => "synthetic"}), do: true
  def synthetic?(_), do: false

  # BT-2735: the method-row hover (`title`). When the browse op enriched the row
  # with a `signature` (and, for a compiler-derived method, a resolved `doc`,
  # BT-2714) the hover reads VS Code-style — the rendered signature over the
  # first line of the doc — so it conveys what the method is without a click.
  # Rows the op left unenriched (hand-written selectors, kept off the browse hot
  # path) fall back to the bare selector, the pre-BT-2735 behaviour. Public:
  # `WorkspaceLive`'s `system_browser_methods/1` render component calls it
  # directly.
  def method_row_title(m) do
    case {presence(m["signature"]), first_doc_line(m["doc"])} do
      {nil, nil} -> presence(m["selector"])
      {sig, nil} -> sig
      {nil, doc} -> "#{m["selector"]}\n#{doc}"
      {sig, doc} -> "#{sig}\n#{doc}"
    end
  end

  # The first non-blank line of a method's `///` doc, or nil when the doc is
  # absent/blank. Keeps the hover to a single descriptive line (the full doc is
  # a click away in the read-only pane).
  defp first_doc_line(doc) when is_binary(doc) do
    doc
    |> String.split("\n", parts: 2)
    |> hd()
    |> presence()
  end

  defp first_doc_line(_), do: nil

  # A binary trimmed to nil when blank, itself otherwise — so an empty
  # signature/doc string reads the same as an absent one.
  defp presence(s) when is_binary(s) do
    case String.trim(s) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp presence(_), do: nil

  # Source origin badge helpers (BT-2552, BT-2643, BT-2641). Two orthogonal
  # fields drive the badge: `source_origin` is the classification ("stdlib" |
  # "dependency" | "project") and keys the css class + title; `package` is the
  # package name and feeds the dependency badge label. The dependency badge reads
  # as a "DEP" marker (parity with the generic "STDLIB" marker), suffixed with
  # the package name when known ("DEP · HTTP") and bare ("DEP") otherwise.
  # Public: `WorkspaceLive`'s render template calls these directly.
  def source_origin_class(%{"source_origin" => "stdlib"}), do: "stdlib"
  def source_origin_class(%{"source_origin" => "dependency"}), do: "dependency"
  def source_origin_class(_), do: "project"

  def source_origin_label(%{"source_origin" => "stdlib"}), do: "stdlib"

  def source_origin_label(%{"source_origin" => "dependency"} = row),
    do: dependency_badge_label(row)

  def source_origin_label(_), do: ""

  # Dependency badge text: "DEP · <pkg>" when the package is known, plain "DEP"
  # when it is absent/unknown. This is the badge-specific label; `package_name/1`
  # (which degrades to "unknown") still serves callers that need the raw package.
  # Public: `BtAttachWeb.Live.MethodEditor`'s `header_package_label/1` (BT-3296)
  # calls it directly for the editor-header badge.
  def dependency_badge_label(row) do
    case Map.get(row, "package") do
      pkg when is_binary(pkg) and pkg != "" -> "DEP · #{pkg}"
      _ -> "DEP"
    end
  end

  def source_origin_title(%{"source_origin" => "stdlib"}), do: "Standard library"

  def source_origin_title(%{"source_origin" => "dependency"} = row),
    do: "Dependency: #{package_name(row)}"

  def source_origin_title(_), do: "Project"

  # The package name carried on a browse row's `package` field (BT-2643). Absent /
  # null packages degrade to "unknown" so a dependency badge never renders blank.
  # Public: `BtAttachWeb.Live.MethodEditor`'s `header_origin_title/1` (BT-3296)
  # calls it directly for the editor-header badge.
  def package_name(row) do
    case Map.get(row, "package") do
      pkg when is_binary(pkg) and pkg != "" -> pkg
      _ -> "unknown"
    end
  end

  # The method shown in the focused tab as a `%{class, side, selector}` ref (or nil
  # for a class-definition tab), so the System Browser can highlight the matching
  # method row. Takes bare `assigns` for the render template. An unsaved new-method
  # tab has no real selector yet (`new: true`, `selector: ""`), so it highlights
  # nothing — returning a `selector: ""` ref would never match a row anyway.
  # Public: `WorkspaceLive`'s render template calls it directly.
  def selected_method_ref(assigns) do
    case MethodEditor.active_tab(assigns) do
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
  # Public: `WorkspaceLive`'s render template calls it directly.
  def selected_def_ref(assigns) do
    case MethodEditor.active_tab(assigns) do
      %{kind: :def, class: class} -> class
      _ -> nil
    end
  end
end

# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.BrowserComponents do
  @moduledoc """
  System Browser function components extracted from `BtAttachWeb.WorkspaceLive`
  (BT-5092): the class tree (`system_browser_classes/1`, `class_rows/1`), the
  protocol + method pane, the read-only native/alias source body and the
  Senders / Implementors popover. Imported by `WorkspaceLive`, so the
  `<.system_browser_classes>` etc. call sites in its template are unchanged.
  """

  use BtAttachWeb, :html

  alias BtAttachWeb.ClassTree
  alias BtAttachWeb.Live.SystemBrowser

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
  # BT-3314: the read-only alias source pane, mirroring `native_view`. `nil` =
  # every alias row collapsed; otherwise the fetched source for whichever
  # alias was last clicked (single-slot, same as `native_view`).
  attr :alias_view, :map, default: nil

  def system_browser_classes(assigns) do
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
           module (aliases erase entirely), so each row shows the alias's
           expansion inline, with a package/origin badge (mirroring the class
           tree/Native vocabulary) and an "internal" tag when set. BT-3314:
           clicking a row toggles a read-only source view inline underneath it
           (mirroring `browser_open_native`'s inline toggle) — `content: nil`
           degrades to the honest "source not available" empty state for
           stdlib/dependency aliases (no live path cache for those origins). --%>
      <div :if={@browser_mode == :aliases} class="panel-body" id="type-aliases-browser">
        <%= cond do %>
          <% @browser_type_aliases == [] -> %>
            <p class="muted-note">No type aliases in the workspace.</p>
          <% true -> %>
            <div class="tree type-aliases-list">
              <div :for={alias_row <- @browser_type_aliases} class="alias-item">
                <div
                  class={[
                    "row",
                    SystemBrowser.alias_shown?(assigns, alias_row["name"], alias_row["package"]) &&
                      "sel"
                  ]}
                  phx-click="browser_open_alias"
                  phx-value-name={alias_row["name"]}
                  phx-value-package={alias_row["package"]}
                  title={alias_row["name"]}
                >
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
                <div
                  :if={SystemBrowser.alias_shown?(assigns, alias_row["name"], alias_row["package"])}
                  class="native-body alias-body"
                >
                  <.native_source_body view={@alias_view} dismiss_event="dismiss_alias_error">
                    <:unavailable>
                      No source available for type alias <code class="mono">{alias_row["name"]}</code>.
                    </:unavailable>
                  </.native_source_body>
                </div>
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
  def class_row_indent(indent) when is_integer(indent) and indent > 0,
    do: "padding-left: #{10 + indent * 14}px"

  def class_row_indent(_), do: nil

  # BT-2578/BT-2648/BT-3314: the read-only source-view body, shared by the
  # class-definition tab's native pane (keyed by a `native:` class's backing
  # module), the standalone "Native modules" pane (keyed by a module), and the
  # Type Aliases panel's inline alias pane (keyed by a `name`/`package` pair —
  # BT-3314). `view` is the fetched native_view/alias_view map
  # (`error`/`content`/`source_file`/`source_origin`/`editable`/`clauses`/
  # `selected_clause`/`requested_selector` — an alias_view always carries
  # `clauses: []` and `requested_selector: nil`, so those branches simply don't
  # render); `dismiss_event` clears the in-pane error. `content == nil`
  # degrades to the `:unavailable` slot, never an error — the caller supplies
  # that empty-state message directly (it's the one thing that genuinely
  # differs per caller: "the module shipped without source" vs. "no source
  # available for this alias"), so the component itself stays agnostic to
  # what kind of source it's showing rather than branching on a caller kind.
  attr :view, :map, required: true
  attr :dismiss_event, :string, required: true
  slot :unavailable, required: true

  def native_source_body(assigns) do
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
      <div :if={is_nil(@view.error)} class="muted-note">{render_slot(@unavailable)}</div>
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

  def system_browser_methods(assigns) do
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

  def nav_popover(assigns) do
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

  # The Senders/Implementors popover heading for a nav kind (BT-2495).
  defp nav_kind_label(:senders), do: "Senders"
  defp nav_kind_label(:implementors), do: "Implementors"
  # BT-2639: the protocol-action popover headings.
  defp nav_kind_label(:required_methods), do: "Required methods"
  defp nav_kind_label(:conforming_classes), do: "Conforming classes"
  # BT-2669: the native-module callers popover heading.
  defp nav_kind_label(:callers_of_native_module), do: "Callers"
end

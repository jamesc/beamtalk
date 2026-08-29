# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.ClassModals do
  @moduledoc """
  The New Class / Rename modals, "Add a method…", and the Remove Method /
  Remove Class editor actions (BT-2293/BT-2645, ADR 0112 Phase 4 BT-3189, ADR
  0113 Phase 4 BT-3210, ADR 0114 Phase 5 BT-3277) — extracted out of
  `BtAttachWeb.WorkspaceLive` (BT-3298, epic BT-3290, the fifth and final
  sequential extraction) so its `handle_event/3` clauses and the
  create/rename/remove data model they drive are directly unit-testable
  instead of only reachable through a full-LiveView integration test.
  Follows the same extraction shape `BtAttachWeb.Live.Inspector` (BT-3291),
  `BtAttachWeb.Live.Dock` (BT-3295), `BtAttachWeb.Live.MethodEditor`
  (BT-3296), and `BtAttachWeb.Live.SystemBrowser` (BT-3297) established.

  This module owns:

    * **New Class modal** (BT-2293, BT-2645) — `"toggle_new_class"` /
      `"close_new_class"` open/close the modal; `"new_class"` validates the
      submitted PascalCase name + superclass locally, synthesizes the
      `<Superclass> subclass: <Name>` definition and its derived `src/`
      path, and dispatches `Workspace newClass:at:` (ADR 0082 Phase 5).
    * **"Add a method…"** — `"new_method"` opens a blank `:method` tab for
      the selected class via `BtAttachWeb.Live.MethodEditor.open_new_method/3`
      — this module never opens its own editor surface.
    * **Remove Method / Remove Class** (ADR 0112 Phase 4 BT-3189, ADR 0113
      Phase 4 BT-3210) — `"remove_method"` / `"remove_class"` read the
      target from the active method-editor tab (never client-supplied
      params) and submit `Class removeSelector: #sel` /
      `Class removeFromSystem` through the existing `evaluate` op, exactly
      like the REPL meta-commands and MCP tools.
    * **Rename modal** (ADR 0114 Phase 5, BT-3277) — `"open_rename"` reads
      the active tab (a `:def` tab renames its class via `renameTo:`, a
      `:method` tab renames its selector via `renameSelector:to:`) and
      captures the target at open time; `"rename_submit"` dispatches the
      typed name through the matching primitive, and `"close_rename"`
      dismisses the modal without renaming anything.

  Every workspace read/write goes through `BtAttach.Facade.dispatch/3` (ADR
  0091 Decision 3) with `BtAttachWeb.Live.RequestContext` — never a raw
  `BtAttach.Workspace`/`:rpc` call — so this module never reimplements the
  `new_class`/`evaluate` ops or the RBAC gates they ride (CLAUDE.md
  no-duplicate-implementations). Every mutation is submitted through the same
  generic `evaluate`/`new_class` ops the REPL meta-commands and MCP tools
  use — there is no dedicated workspace-side "remove"/"rename" op to
  duplicate (ADR 0112's/ADR 0113's/ADR 0114's "Surface" tables).

  State (`:new_class_open`, `:new_class_error`, `:new_class_name`,
  `:new_class_super`, `:rename_open`, `:rename_kind`, `:rename_class`,
  `:rename_side`, `:rename_old_selector`, `:rename_new_name`,
  `:rename_error`) stays on the LiveView's own socket — initialised in
  `WorkspaceLive.bind_session/3` and mount, same as the
  Dock/Inspector/MethodEditor/SystemBrowser/TestRunner assigns.
  `WorkspaceLive` still owns `handle_event/3` (`Phoenix.LiveView` callback
  contract) and `render/1` (the New Class/Rename modal markup is woven into
  the System Browser panel and the tabbed method editor's action row, so it
  does not split cleanly along this extraction's event boundary — see the
  same call in `Dock`'s/`Inspector`'s/`MethodEditor`'s/`SystemBrowser`'s
  moduledocs), but delegates every event this module owns to the functions
  here by name — see the `@class_modals_events` guard clause in
  `WorkspaceLive`, which reads its event list from
  `__class_modals_events__/0` below (mirroring the BT-3301 fix that keeps
  `WorkspaceLive` from hand-maintaining a second copy of the event names).

  `valid_class_name?/1` is public: `BtAttachWeb.Live.Dock`'s
  `flush_destructive/3` (BT-3295) validates a client-controlled `class`
  value against this same bare-PascalCase-identifier rule before
  interpolating it into a raw `evaluate` expression, so a crafted event
  can't inject arbitrary source — reusing this module's rule rather than
  duplicating it.
  """

  use BtAttachWeb, :html

  alias BtAttach.Facade
  alias BtAttachWeb.Live.FacadeError
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.RequestContext
  alias BtAttachWeb.Live.SystemBrowser
  alias BtAttachWeb.WorkspaceLive

  # ── handle_event dispatch ────────────────────────────────────────────────
  #
  # `WorkspaceLive.handle_event/3` forwards every event whose name is in
  # `@class_modals_events` (read from `__class_modals_events__/0` below) here
  # unchanged (same event name, params, socket), so each clause below is
  # exactly the body the LiveView used to run directly.
  @class_modals_events ~w(
    remove_method remove_class open_rename close_rename rename_submit
    new_method toggle_new_class close_new_class new_class
  )

  @doc false
  def __class_modals_events__, do: @class_modals_events

  # Bare PascalCase class-name identifier (BT-2645) — the shape the New Class
  # modal and the Rename modal's class-rename path both enforce. Defined here
  # (ahead of every clause below that reads it) rather than beside
  # `validate_new_class_name/2` further down, since `rename_class/3` also
  # needs it and module attributes must be set before their first use.
  @new_class_name_re ~r/^[A-Z][A-Za-z0-9_]*$/

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

  # ── Remove Method (ADR 0112 Phase 4, BT-3189) ───────────────────────────────

  defp remove_active_method(socket) do
    case MethodEditor.active_tab(socket.assigns) do
      %{kind: :method, new: true} ->
        WorkspaceLive.status_error(
          socket,
          "This method hasn't been saved yet — nothing to remove."
        )

      %{kind: :method, class: class, side: side, selector: selector} = tab
      when is_binary(selector) and selector != "" ->
        remove_method(socket, tab, class, side, selector)

      _ ->
        WorkspaceLive.status_error(socket, "Open an existing method to remove it.")
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
      WorkspaceLive.status_error(socket, "not attached to workspace")
    else
      remove_method_eval(socket, tab, receiver, selector, expr, pid)
    end
  end

  defp remove_method_eval(socket, tab, receiver, selector, expr, pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, RequestContext.build(socket)) do
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
        |> WorkspaceLive.assign_changes()

      {:error, _, _, _} = err ->
        WorkspaceLive.status_error(socket, FacadeError.render_eval_error(err))

      {:error, _} = err ->
        WorkspaceLive.status_error(socket, FacadeError.render_eval_error(err))
    end
  end

  # ── Remove Class (ADR 0113 Phase 4, BT-3210) ────────────────────────────────

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
        WorkspaceLive.status_error(socket, "Open a class definition to remove it.")
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
      WorkspaceLive.status_error(socket, "not attached to workspace")
    else
      remove_class_eval(socket, tab, class, expr, pid)
    end
  end

  defp remove_class_eval(socket, tab, class, expr, pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, RequestContext.build(socket)) do
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
        |> WorkspaceLive.assign_changes()

      {:error, _, _, _} = err ->
        WorkspaceLive.status_error(socket, FacadeError.render_eval_error(err))

      {:error, _} = err ->
        WorkspaceLive.status_error(socket, FacadeError.render_eval_error(err))
    end
  end

  # ── Rename modal (ADR 0114 Phase 5, BT-3277) ────────────────────────────────

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
        WorkspaceLive.status_error(
          socket,
          "This method hasn't been saved yet — nothing to rename."
        )

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
        WorkspaceLive.status_error(
          socket,
          "Open a class definition or an existing method to rename it."
        )
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
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, RequestContext.build(socket)) do
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
        |> WorkspaceLive.assign_changes()
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

      {:error, _, _, _} = err ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_name,
          rename_error: FacadeError.render_eval_error(err)
        )

      {:error, _} = err ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_name,
          rename_error: FacadeError.render_eval_error(err)
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
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, RequestContext.build(socket)) do
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
        |> WorkspaceLive.assign_changes()

      {:error, _, _, _} = err ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_selector,
          rename_error: FacadeError.render_eval_error(err)
        )

      {:error, _} = err ->
        assign(socket,
          rename_open: true,
          rename_new_name: new_selector,
          rename_error: FacadeError.render_eval_error(err)
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

  # ── New Class modal (BT-2293, BT-2645) ──────────────────────────────────────

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
  # (`^[A-Z][A-Za-z0-9_]*$`, `@new_class_name_re` above), and not a duplicate
  # of an existing class in the browse list. Returns `:ok` or `{:error,
  # message}` for an in-modal error.

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

  defp dispatch_new_class(socket, name, superclass, source, path) do
    case Facade.dispatch(:new_class, %{source: source, path: path}, RequestContext.build(socket)) do
      {:ok, created_path} ->
        socket
        |> SystemBrowser.assign_browser_classes()
        |> WorkspaceLive.assign_changes()
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
        |> WorkspaceLive.maybe_refresh_git_after_save()

      {:error, reason} ->
        # Route a failed create to the in-modal error (keep fields + modal open),
        # never the method-editor's `save_error` (BT-2645).
        assign(socket,
          new_class_open: true,
          new_class_name: name,
          new_class_super: superclass,
          new_class_error: FacadeError.render(reason)
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
end
